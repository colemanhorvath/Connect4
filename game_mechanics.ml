type piece =
  | None
  | Normal of int
  | Anvil of int
  | Wall
  | Bomb of int 
  | Force of int

type status = 
  | Play
  | Draw
  | Win of int

type board = piece list list

exception InvalidRow of int

exception InvalidPieceType of string

(* num_players is the number of players in the game
   rows is the number of rows in the gameboard
   cols is the number of columns in the gameboard
   gameboard is a board
   player_turn is the current player's turn, starts at index 0 for player 1, 
   1 for player 2, etc
   connect_num is the number of pieces that need to be connected for a win, 
   would be 4 for regular connect 4
   colors is a list of all the colors the players have picked used for 
   printing the gameboard piece colors, index 0 is the color of player 1, etc. 
   game_mode is an int representing the 3 possible game modes:
   1 is no special pieces, 2 is 1 of each special piece, 
   3 is Random chance of receiving special pieces 
   special pieces is a list of the numbers of special pieces each player has 
   with the format [[num of anvils; wall; bomb; force];
   [num of anvils; wall; bomb; force]; etc]
   where index 0 is the special pieces player 1 has, etc.
   is_player_forced is a true if the current player is forced to play their 
   opponent's piece on their turn per the Force special piece, false otherwise. 
   is_awaiting_bomb is true if the next move is the player playing a bomb, 
   false otherwise.*)
type t = {
  num_players : int;
  rows : int;
  cols : int;
  gameboard : board;
  player_turn : int;
  connect_num : int;
  colors : ANSITerminal.style list;
  game_mode : int;
  special_pieces : int list list;
  is_player_forced: bool;
  is_awaiting_bomb: bool;
}

type move_result =  Valid of t | Invalid

(** [board_helper col_counter acc] is used to create a board with the initial
    number of [col_counter] columns *)
let rec board_helper col_counter acc =
  if col_counter = 0 then acc else board_helper (col_counter - 1) ([] :: acc)

(** [create_board cols] is a board with [cols] number of columns *)
let create_board cols = 
  board_helper (cols - 1) [[]]

(** [special_piece_maker] generates a list of the number of each special piece
    for a player based on the game [mode]  *)
let special_piece_maker mode = 
  match mode with 
  | 1 -> [0;0;0;0]
  | 2 -> [1;1;1;1]
  | 3 -> [Random.int 3; Random.int 3; Random.int 3; Random.int 3]
  | _ -> [0;0;0;0]

(** [special_piece_list_maker] generates a list of lists of the number of each 
    special piece for each player based on the game [mode]  *)
let rec special_piece_list_maker mode count acc = 
  if count = 0 then acc 
  else special_piece_list_maker mode (count - 1) (special_piece_maker mode :: acc)

(* will need to update this for load, if loading an old game the special_pieces 
   should be saved, not made from scratch *)
let load_game players rows cols board turn connect colors mode bomb 
    force = {
  num_players = players;
  rows = rows;
  cols = cols; 
  gameboard = board;
  player_turn = turn;
  connect_num = connect;
  colors = colors;
  game_mode = mode;
  special_pieces = special_piece_list_maker mode players [];
  is_awaiting_bomb = bomb;
  is_player_forced = force;
}

let start_game rows cols players connect colors mode = 
  load_game players rows cols (create_board cols) 0 connect colors mode 
    false false

let create_piece piece_type player = 
  match piece_type with 
  | "normal" -> Normal player
  | "anvil" -> Anvil player
  | "wall" -> Wall
  | "bomb" -> Bomb player
  | "force" -> Force player
  | _ -> None

(** [verify_placement board col rows] is false if placing a piece in a col,
    with col indexing starting at 0, that is full and true otherwise *)
let verify_placement board col rows =
  let col_length = List.nth board col |> List.length in
  if col_length >= rows then false else true

(** [update_board board col piece] is a board with the new [piece] added to the 
    beginning of the [col], with col indexing starting at 0 *)
let update_board board col piece =
  List.mapi (fun i x -> if i = col then (piece :: x) else x) board

(** [clear_column board col] is a board with the column at index [col] set to 
    empty. *)
let clear_column board col =
  List.mapi (fun i x -> if i = col then [] else x) board

(** [next_player state piece] is the player that should make the next move, 
    depending on the current [state] and the type of the last [piece] played. 
    Another move from the current player is necessary if they played a wall, 
    a bomb, or an opponents piece. *)
let next_player state piece = 
  if state.is_player_forced = true then state.player_turn 
  else
    match piece with
    | Wall -> state.player_turn
    | Bomb _ -> state.player_turn
    | _ -> (state.player_turn + 1) mod state.num_players

(** [get_new_board state col piece] is the new board as a result of placing 
    [piece] in [col] of the board in [state]. For anvils, this includes removing 
    all pieces in [col] before placing [piece], otherwise the [piece] is just 
    placed in [col]. *)
let get_new_board state col piece =
  match piece with 
  | Anvil _ -> 
    let temp_board = clear_column state.gameboard (col - 1) in
    update_board temp_board (col - 1) piece
  | _ -> update_board state.gameboard (col - 1) piece

(** [is_force_piece piece] is true if [piece] is of type Force, false 
    otherwise. *)
let is_force_piece piece = 
  match piece with
  | Force _ -> true
  | _ -> false

(** [is_force_piece piece] is true if [piece] is of type Bomb, false 
    otherwise. *)
let is_bomb_piece piece = 
  match piece with
  | Bomb _ -> true
  | _ -> false

(** [player_piece_decrementer piece_loc pieces_list count acc] decrements the 
    special piece count at [piece_loc] in [pieces_list] *)
let rec player_piece_decrementer piece_loc pieces_list count acc = 
  match pieces_list with 
  | [] -> List.rev acc 
  | x :: xs -> if count = piece_loc then player_piece_decrementer piece_loc xs
        (count + 1) (x - 1 :: acc)
    else player_piece_decrementer piece_loc xs (count + 1) (x :: acc)

(** [special_piece_decrementer player piece_loc special_pieces_list count acc] 
    gets the special piece list for [player] in special_pieces_list so it can 
    be decremented with [player_piece_decrementer] *)
let rec special_piece_decrementer player piece_loc special_pieces_list count acc = 
  match special_pieces_list with 
  | [] -> List.rev acc 
  | x :: xs -> if count = player then special_piece_decrementer player piece_loc
        xs (count + 1) ((player_piece_decrementer piece_loc x 0 []) :: acc)
    else special_piece_decrementer player piece_loc xs (count + 1) (x :: acc)

(** [special_piece_matcherplayer player piece special_pieces_list] 
    gets the location of the special piece count in the player's special piece
    list so it can be decremented with [special_piece_decrementer] *)
let special_piece_matcher player piece special_pieces_list =
  match piece with 
  | Anvil _ -> special_piece_decrementer player 0 special_pieces_list 0 []
  | Wall -> special_piece_decrementer player 1 special_pieces_list 0 []
  | Bomb x -> special_piece_decrementer player 2 special_pieces_list 0 []
  | Force x -> special_piece_decrementer player 3 special_pieces_list 0 []
  | _ -> special_pieces_list

let move state col piece = 
  if col > state.cols || col <= 0 then Invalid
  else if not (verify_placement state.gameboard (col - 1) state.rows) 
  then Invalid
  else 
    let new_board = get_new_board state col piece in
    let new_player = next_player state piece in
    let force_bool = is_force_piece piece in
    let bomb_bool = is_bomb_piece piece in
    let special_pieces = special_piece_matcher state.player_turn piece 
        state.special_pieces in
    Valid {state with gameboard = new_board; 
                      player_turn = new_player;
                      special_pieces = special_pieces;
                      is_player_forced = force_bool;
                      is_awaiting_bomb = bomb_bool}

(** [rec bomb_board_helper col row_i iterator] is recursively the new column
    without the piece at [row_i] with the other pieces shifted down in the 
    current [col]. [iterator] starts at the highest row in [col] and decreases
     throughout. *)
let rec bomb_board_helper col row_i iterator =
  match col with 
  | [] -> []
  | elt::t ->
    if row_i <> iterator then elt::(bomb_board_helper t row_i (iterator - 1))
    else bomb_board_helper t row_i (iterator - 1)

(** [rec bomb_board board row_i col_i iterator] is recursively the new board 
    without the piece at [row_i] [col_i] in the current [board]. [iterator] 
    starts at the initial column index (1), and increases throughout. *)
let rec bomb_board board row_i col_i iterator = 
  match board with 
  | [] -> []
  | column::t -> 
    if iterator <> col_i then column::(bomb_board t row_i col_i (iterator + 1))
    else 
      let num_rows = List.length column in
      if row_i > num_rows then raise (InvalidRow row_i)
      else (bomb_board_helper column row_i num_rows)
           ::(bomb_board t row_i col_i (iterator + 1))

let bomb state row col =
  try
    if col > state.cols || col <= 0 || row > state.rows || row <= 0 then Invalid
    else 
      let new_board = bomb_board state.gameboard row col 1 in
      Valid {state with gameboard = new_board; 
                        player_turn = (state.player_turn + 1) 
                                      mod state.num_players;
                        is_player_forced = false;
                        is_awaiting_bomb = false}
  with
  | InvalidRow r -> Invalid


let get_gameboard state = 
  state.gameboard

let get_player_turn state = 
  state.player_turn + 1

let get_prev_player_turn state = 
  if state.player_turn = 0 then state.num_players else state.player_turn

let get_player_hand state player =
  List.nth state.special_pieces (player - 1)

let get_num_of_piece_type state player piece_type =
  let hand = get_player_hand state player in
  match piece_type with
  | "anvil" -> List.nth hand 0
  | "wall" -> List.nth hand 1
  | "bomb" -> List.nth hand 2
  | "force" -> List.nth hand 3
  | _ -> raise (InvalidPieceType piece_type)

let format p = 
  match p with 
  | Normal x -> "\"Normal " ^ (string_of_int x) ^ "\""
  | Anvil x -> "\"Anvil " ^ (string_of_int x) ^ "\""
  | Wall -> "\"Wall" ^ "\""
  | Bomb x -> "\"Bomb " ^ (string_of_int x) ^ "\""
  | Force x -> "\"Force " ^ (string_of_int x) ^ "\""
  | _ -> "\"Other " ^ "error" ^ "\""

(** [string_of_piece p] is the JSON string representation of piece [p]. *)
let string_of_piece p = 
  match p with 
  | None -> "{\"type\":\"None\",\"player\":\"-1\"}"
  | Normal p ->
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Normal\",\"player\":\""; player; "\"}"]
  | Anvil p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Anvil\",\"player\":\""; player; "\"}"]
  | Wall -> 
    String.concat "" ["{\"type\":\"Wall\":\"\"}"]
  | Bomb p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Bomb\",\"player\":\""; player; "\"}"]
  | Force p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Force\",\"player\":\""; player; "\"}"]

(** [string_of_col col] is the JSON string representation of piece list [col].*)
let string_of_col col = 
  if col = [] then "" else
    String.concat "," (List.map string_of_piece col)

(** [string_of_board board] is the JSON string representation of 
    gameboard [board]. *)
let string_of_board board = 
  let body = String.concat "],[" (List.map string_of_col board) in 
  String.concat "" ["[["; body; "]]"]

let to_json_string st = 
  let np_str = string_of_int st.num_players in 
  let row_str = string_of_int st.rows in 
  let col_str = string_of_int st.cols in 
  let board_str = string_of_board st.gameboard in 
  let turn_str = string_of_int st.player_turn in 
  String.concat "," [
    String.concat "" ["{\"num_players\":\""; np_str; "\""];
    String.concat "" ["\"rows\":\""; row_str; "\""];
    String.concat "" ["\"cols\":\""; col_str; "\""];
    String.concat "" ["\"gameboard\":"; board_str];
    String.concat "" ["\"player_turn\":\""; turn_str; "\""];
  ]

(** [get_piece_player p] is the player whose piece is p, the Wall has no 
    player associated because its colorless so it has a value of 100 *)
let get_piece_player p = 
  match p with 
  | Normal x -> x
  | Anvil x -> x
  | Wall -> 100
  | Bomb x -> x
  | Force x -> x
  | None -> 10

(** [check_col_match column player count connect] checks if there is a match
    of [connect] pieces for [player] in [column] *)
let rec check_col_match column player count connect =
  if count = connect then true else 
    match column with
    | [] -> false
    | x :: xs -> let piece_num = get_piece_player x in 
      if piece_num = player then check_col_match xs player (count + 1) connect
      else false

(** [check_col_win board player col connect max_cols] checks if there is 
    enough pieces in [col] of [board] for a match of [connect] pieces 
    so it can be checked for a win in [check_col_match]  *)
let check_col_win board player col connect max_cols = 
  if col >= max_cols then false 
  else 
    let column = List.nth board col in 
    if List.length column < connect
    then false else check_col_match column player 0 connect

(** [check_row_match board row player col count last max_cols connect] checks 
    if there is a match of [connect] pieces for [player] in [row]] *)
let rec check_row_match board row player col count last max_cols connect = 
  if count = connect then true
  else if col >= max_cols then false
  else if col = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_row_match board row player (col + 1) 0 last max_cols connect
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_row_match board row player (col + 1) (count + 1) last max_cols connect
      else check_row_match board row player (col + 1) 0 last max_cols connect

(** [check_diagonal_lr_match board row player col count inc last max_cols 
    max_rows connect] checks if there is a diagonal left right match
    of [connect] pieces for [player] by starting in the leftmost bottom
    piece in the diagonal and making its way up the diagonal *)
let rec check_diagonal_lr_match board row player col count inc last max_cols max_rows connect = 
  if count = connect then true
  else if row < 0 || row >= max_rows then false
  else if col < 0 || col >= max_cols then false
  else if inc = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_diagonal_lr_match board (row + 1) player (col + 1) 0 (inc + 1) last max_cols max_rows connect
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_diagonal_lr_match board (row + 1) player (col + 1) (count + 1) (inc + 1) last max_cols max_rows connect
      else check_diagonal_lr_match board (row + 1) player (col + 1) 0 (inc + 1) last max_cols max_rows connect

(** [check_diagonal_rl_match board row player col count inc last max_cols 
    max_rows connect] checks if there is a diagonal right left match
    of [connect] pieces for [player] by starting in the leftmost top
    piece in the diagonal and making its way down the diagonal *)
let rec check_diagonal_rl_match board row player col count inc last max_cols max_rows connect = 
  if count = connect then true
  else if row < 0 || row >= max_rows then false
  else if col < 0 || col >= max_cols then false
  else if inc = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_diagonal_rl_match board (row - 1) player (col + 1) 0 (inc + 1) last max_cols max_rows connect
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_diagonal_rl_match board (row - 1) player (col + 1) (count + 1) (inc + 1) last max_cols max_rows connect
      else check_diagonal_rl_match board (row - 1) player (col + 1) 0 (inc + 1) last max_cols max_rows connect

let check_win state player col =
  let col = col - 1 in
  let board = state.gameboard in
  if check_col_win board player col state.connect_num state.cols then true 
  else 
    let row = (List.nth board col |> List.length) - 1 in 
    let first = (max (col - 3) 0)in 
    let last = (min (col + 4) state.cols) in 
    if check_row_match board row player first 0 last state.cols state.connect_num then true 
    else 
      let rowbegin = (max (row - 3) 0) in
      let rowend = (min (row + 3) (state.rows - 1)) in 
      let colbegin = (max (col - 3) 0) in
      let colend = (min (col + 3) (state.cols - 1)) in 
      let start = (min (row - rowbegin) (col - colbegin)) in 
      let range = (min (rowend - rowbegin) (colend - colbegin)) + 1 in 
      if range < 4 then false else
      if check_diagonal_lr_match board (row - start) player 
          (col - start) 0 0 7 state.cols state.rows state.connect_num then true 
      else  
        let rowbegin = (min (row + 3) (state.rows - 1)) in
        let rowend = (max (row - 3) 0) in 
        let start = (min (rowbegin - row) (col - colbegin)) in 
        let range = (min (rowbegin - rowend) (colend - colbegin)) + 1 in 
        if range < 4 then false 
        else 
          check_diagonal_rl_match board (row + start) player 
            (col - start) 0 0 7 state.cols state.rows state.connect_num

(** [rec check_draw_helper state board] is recursively true if the board is
    completely full of pieces, false otherwise. *)
let rec check_draw_helper state board = 
  match board with
  | [] -> true
  | col::t ->
    if List.length col <> state.rows then false else check_draw_helper state t

let check_draw state =
  check_draw_helper state state.gameboard

(* TODO: add tests, maybe take draw/win out of mli *)
let check_status state player col =
  if check_win state player col then Win player else
  if check_draw state then Draw else Play

let get_dimensions state =
  (state.rows, state.cols)

let get_colors state =
  state.colors

let is_forced state =
  state.is_player_forced

let is_bombed state =
  state.is_awaiting_bomb

(*
          Format.fprintf Format.std_formatter "c:%d \n" col;
        Format.fprintf Format.std_formatter "r:%d \n" row;
        Format.fprintf Format.std_formatter "rb:%d \n" rowbegin;
        Format.fprintf Format.std_formatter "re:%d \n" rowend;
        Format.fprintf Format.std_formatter "cb:%d \n" colbegin;
        Format.fprintf Format.std_formatter "ce:%d \n" colend;
        Format.fprintf Format.std_formatter "start:%d \n" start;
        Format.fprintf Format.std_formatter "range:%d \n" range;
        Format.fprintf Format.std_formatter "rs:%d \n" (row + start);
        Format.fprintf Format.std_formatter "cs:%d \n\n" (col - start);
        *)