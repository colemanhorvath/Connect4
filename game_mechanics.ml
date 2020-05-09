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
  ai_active: bool
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
  else special_piece_list_maker mode (count - 1) 
      (special_piece_maker mode :: acc)

let load_game players rows cols board turn connect colors mode sps force 
    bomb ai = {
  num_players = players;
  rows = rows;
  cols = cols; 
  gameboard = board;
  player_turn = turn;
  connect_num = connect;
  colors = colors;
  game_mode = mode;
  special_pieces = sps;
  is_player_forced = force;
  is_awaiting_bomb = bomb;
  ai_active = ai
}

let start_game rows cols players connect colors mode = 
  load_game players rows cols (create_board cols) 0 connect colors mode 
    (special_piece_list_maker mode players []) false false false

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
let rec special_piece_decrementer player piece_loc special_pieces_list 
    count acc = 
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
    let num_rows = List.length column in
    if iterator <> col_i then column::(bomb_board t row_i col_i (iterator + 1))
    else if row_i > num_rows then raise (InvalidRow row_i)
    else (bomb_board_helper column row_i num_rows)
         ::(bomb_board t row_i col_i (iterator + 1))

let bomb state row col =
  try
    if col > state.cols || col <= 0 || row > state.rows || row <= 0 
    then Invalid
    else 
      let new_board = bomb_board state.gameboard row col 1 in
      Valid {state with gameboard = new_board; 
                        player_turn = (state.player_turn + 1) 
                                      mod state.num_players;
                        is_player_forced = false;
                        is_awaiting_bomb = false}
  with
  | InvalidRow r -> Invalid

let change_connect_num state n = 
  {state with connect_num = n}

let get_gameboard state = 
  state.gameboard

let get_player_turn state = 
  state.player_turn + 1

let get_prev_player_turn state = 
  if state.player_turn = 0 then state.num_players 
  else state.player_turn

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
    String.concat "" ["{\"type\":\"Wall\",\"player\":\"0\"}"]
  | Bomb p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Bomb\",\"player\":\""; player; "\"}"]
  | Force p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Force\",\"player\":\""; player; "\"}"]

(** [string_of_col col] is the JSON string representation of piece list [col].*)
let string_of_col col = 
  String.concat "," (List.map string_of_piece col)

(** [string_of_board board] is the JSON string representation of 
    gameboard [board]. *)
let string_of_board board = 
  let body = String.concat "],[" (List.map string_of_col board) in 
  String.concat "" ["[["; body; "]]"]

(** [string_of_color c] is the string of color of [c]*)
let string_of_color c =
  match c with 
  | ANSITerminal.Foreground Red -> "\"Red\""
  | ANSITerminal.Foreground Green -> "\"Green\""
  | ANSITerminal.Foreground Yellow -> "\"Yellow\""
  | ANSITerminal.Foreground Blue -> "\"Blue\""
  | ANSITerminal.Foreground Magenta -> "\"Magenta\""
  | ANSITerminal.Foreground Cyan -> "\"Cyan\""
  | _ -> "Illegal Color"

(** [string_of_colors colors] is the JSON string representation of [colors]*)
let string_of_colors colors = 
  let body = String.concat "," (List.map string_of_color colors) in 
  String.concat "" ["["; body; "]"]

(** [string_of_player_sp player_sps] is the JSON string representation of 
    one player's special pieces [player_sps]. *)
let string_of_player_sp player_sps = 
  let body = String.concat "\",\"" (List.map string_of_int player_sps) in 
  String.concat "" ["\""; body; "\""]

(** [string_of_special_pieces sps] is the JSON string representation of all
    players' special pieces [sps]*)
let string_of_special_pieces sps = 
  let body = String.concat "],[" (List.map string_of_player_sp sps) in 
  String.concat "" ["[["; body; "]]"]

let to_json_string st = 
  let np_str = string_of_int st.num_players in 
  let row_str = string_of_int st.rows in 
  let col_str = string_of_int st.cols in 
  let board_str = string_of_board st.gameboard in 
  let turn_str = string_of_int st.player_turn in
  let connect_str = string_of_int st.connect_num in 
  let colors_str = string_of_colors st.colors in 
  let mode_str = string_of_int st.game_mode in 
  let special_str = string_of_special_pieces st.special_pieces in 
  let forced_str = string_of_bool st.is_player_forced in 
  let bomb_str = string_of_bool st.is_awaiting_bomb in
  let ai_str = string_of_bool st.ai_active in
  String.concat "," [
    String.concat "" ["{\"num_players\":\""; np_str; "\""];
    String.concat "" ["\"rows\":\""; row_str; "\""];
    String.concat "" ["\"cols\":\""; col_str; "\""];
    String.concat "" ["\"gameboard\":"; board_str];
    String.concat "" ["\"player_turn\":\""; turn_str; "\""];
    String.concat "" ["\"connect_num\":\""; connect_str; "\""];
    String.concat "" ["\"colors\":"; colors_str];
    String.concat "" ["\"game_mode\":\""; mode_str; "\""];
    String.concat "" ["\"special_pieces\":"; special_str];
    String.concat "" ["\"is_player_forced\":\""; forced_str; "\""];
    String.concat "" ["\"is_awaiting_bomb\":\""; bomb_str; "\""];
    String.concat "" ["\"ai_active\":\""; ai_str; "\"}"]
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

(** [check_bounds row col max_rows max_cols] checks that row and col
    are within bounds of 0 and [max_rows] and 0 and [max_cols] respectively *)
let check_bounds row col max_rows max_cols = 
  if row < 0 || row >= max_rows then true
  else if col < 0 || col >= max_cols then true
  else false

(** [check_col_win column player count connect] checks if there has been enough
    consecutive pieces counted for a column win *)
let rec check_col_win column player count connect =
  if count = connect then true else 
    check_col_match column player count connect

(** [check_col_match column player count connect] checks if there is a match
    of [connect] pieces for [player] in [column] *)
and check_col_match column player count connect =
  match column with
  | [] -> false
  | x :: xs -> let piece_num = get_piece_player x in 
    if piece_num = player then check_col_win xs player (count + 1) connect
    else check_col_win xs player 0 connect

(** [check_col_bounds board player col max_cols connect] checks if there is 
    enough pieces in [col] of [board] for a match of [connect] pieces 
    so it can be checked for a win in [check_col_match]  *)
let check_col_bounds board player col max_cols connect = 
  if col >= max_cols then false 
  else 
    let column = List.nth board col in 
    if List.length column < connect
    then false else check_col_match column player 0 connect

(** [check_row_match board row player col count last max_cols connect] checks 
    if there is a match of [connect] pieces for [player] in [row] *)
let rec check_row_match board row player col count last max_cols connect = 
  let col_len = (List.nth board col |> List.length) in 
  if col_len <= row 
  then check_row_bounds board row player (col + 1) 0 last max_cols connect
  else let piece = List.nth (List.nth board col) (col_len - row - 1) in
    let piece_num = get_piece_player piece in
    if piece_num = player 
    then check_row_bounds board row player (col + 1) (count + 1) last 
        max_cols connect
    else check_row_bounds board row player (col + 1) 0 last max_cols connect

(** [check_row_bounds board row player col count last max_cols connect]
    checks if there is already a win or if the col argument is out of bounds *)
and check_row_bounds board row player col count last max_cols connect = 
  if count = connect then true
  else if check_bounds 0 col 1 max_cols || col = last then false
  else check_row_match board row player col count last max_cols connect

(** [check_diagonal_lr_match board row player col count inc last max_cols 
    max_rows connect] checks if there is a diagonal left right match
    of [connect] pieces for [player] by starting in the leftmost bottom
    piece in the diagonal and making its way up the diagonal *)
let rec check_diagonal_lr_match board row player col count inc last max_cols 
    max_rows connect = 
  let col_len = (List.nth board col |> List.length) in 
  if col_len <= row 
  then check_diagonal_lr_bounds board (row + 1) player (col + 1) 0 (inc + 1) 
      last max_cols max_rows connect
  else let piece = List.nth (List.nth board col) (col_len - row - 1) in
    let piece_num = get_piece_player piece in
    if piece_num = player 
    then check_diagonal_lr_bounds board (row + 1) player (col + 1) (count + 1) 
        (inc + 1) last max_cols max_rows connect
    else check_diagonal_lr_bounds board (row + 1) player (col + 1) 0 (inc + 1) 
        last max_cols max_rows connect

(** [check_diagonal_lr_bounds board row player col count inc last max_cols 
    max_rows connect] checks if there is already a win or if the row
    and col arguments are out of bounds *)
and check_diagonal_lr_bounds board row player col count inc last max_cols 
    max_rows connect = 
  if count = connect then true
  else if check_bounds row col max_rows max_cols || inc = last then false
  else
    check_diagonal_lr_match board row player col count inc last max_cols 
      max_rows connect

(** [check_diagonal_rl_match board row player col count inc last max_cols 
    max_rows connect] checks if there is a diagonal right left match
    of [connect] pieces for [player] by starting in the leftmost top
    piece in the diagonal and making its way down the diagonal *)
let rec check_diagonal_rl_match board row player col count inc last max_cols 
    max_rows connect =
  let col_len = (List.nth board col |> List.length) in 
  if col_len <= row 
  then check_diagonal_rl_bounds board (row - 1) player (col + 1) 0 (inc + 1) 
      last max_cols max_rows connect
  else let piece = List.nth (List.nth board col) (col_len - row - 1) in
    let piece_num = get_piece_player piece in
    if piece_num = player 
    then check_diagonal_rl_bounds board (row - 1) player (col + 1) (count + 1) 
        (inc + 1) last max_cols max_rows connect
    else check_diagonal_rl_bounds board (row - 1) player (col + 1) 0 (inc + 1) 
        last max_cols max_rows connect

(** [check_diagonal_rl_bounds board row player col count inc last max_cols 
    max_rows connect] checks if there is already a win or if the row
    and col arguments are out of bounds *)
and check_diagonal_rl_bounds board row player col count inc last max_cols 
    max_rows connect = 
  if count = connect then true
  else if check_bounds row col max_rows max_cols || inc = last then false
  else
    check_diagonal_rl_match board row player col count inc last max_cols 
      max_rows connect

let check_win state player col row =
  let col = col - 1 in
  let board = state.gameboard in
  let row = if row = -1 then (List.nth board col |> List.length) - 1 
    else row - 1 in 
  let connect_num = state.connect_num in
  let first = (max (col - connect_num - 1) 0)in 
  let last = (min (col + connect_num) state.cols) in 
  let rowbeginlr = (max (row - connect_num - 1) 0) in
  let colbegin = (max (col - connect_num - 1) 0) in
  let startlr = (min (row - rowbeginlr) (col - colbegin)) in 
  let rowbeginrl = (min (row + connect_num - 1) (state.rows - 1)) in
  let startrl = (min (rowbeginrl - row) (col - colbegin)) in 
  check_col_bounds board player col state.cols connect_num ||
  check_row_bounds board row player first 0 last state.cols 
    connect_num ||
  check_diagonal_lr_bounds board (row - startlr) player 
    (col - startlr) 0 0 ((connect_num * 2) - 1) state.cols state.rows 
    connect_num ||
  check_diagonal_rl_bounds board (row + startrl) player 
    (col - startrl) 0 0 ((connect_num * 2) - 1) state.cols state.rows 
    connect_num

(** [rec check_draw_helper state board] is recursively true if the board is
    completely full of pieces, false otherwise. *)
let rec check_draw_helper state board = 
  match board with
  | [] -> true
  | col::t ->
    if List.length col <> state.rows then false else check_draw_helper state t

let check_draw state =
  check_draw_helper state state.gameboard

let num_pieces_in_col state col = 
  if col > state.cols || col <= 0 then -1 
  else List.length (List.nth state.gameboard (col-1))

let check_status state player col =
  if check_win state player col (-1) then Win player else
  if check_draw state then Draw else Play

(** [check_status_bomb_helper state player col row] recursively checks each 
    [row] in [col] to see if [player] has won in [state]. *)
let rec check_status_bomb_helper state player col row = 
  if row = 0 then Play
  else if check_win state player col row then Win player
  else check_status_bomb_helper state player col (row - 1)

let check_status_bomb state player col = 
  let row_len = num_pieces_in_col state col in
  check_status_bomb_helper state player col row_len

let get_dimensions state =
  (state.rows, state.cols)

let get_colors state =
  state.colors

let get_gamemode state =
  state.game_mode

let is_forced state =
  state.is_player_forced

let is_bombed state =
  state.is_awaiting_bomb

let is_ai_active state = 
  state.ai_active

let is_standard st = 
  st.num_players = 2 && st.connect_num = 4 && st.game_mode = 1

let toggle_ai state = if is_standard state then 
    Valid {state with ai_active = not (is_ai_active state)} else Invalid

