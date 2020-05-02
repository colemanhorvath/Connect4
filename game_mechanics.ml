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

(* connect_num is the number of pieces that need to be connected for a win, 
   would be 4 for regular connect 4
   colors is a list of all the colors the players have picked used for 
   printing the gameboard piece colors, index 0 is the color of player 1, etc. 
   game_mode is an int representing the 3 possible game modes:
   1 is no special pieces, 2 is 1 of each special piece, 
   3 is Random chance of receiving special pieces 
   special pieces is a list of the numbers of special pieces each player has 
   with the format [num of anvils; wall; bomb; force] *)
type t = {
  num_players : int;
  rows : int;
  cols : int;
  gameboard : board;
  player_turn : int;
  total_moves : int;
  connect_num : int;
  colors : string list;
  game_mode : int;
  special_pieces : int list;
}

type move_result =  Valid of t | Invalid

(** [board_helper col_counter acc] is a board  *)
let rec board_helper col_counter acc =
  if col_counter = 0 then acc else board_helper (col_counter - 1) ([] :: acc)

(** [create_board cols] is a board with [cols] number of columns *)
let create_board cols = 
  board_helper (cols - 1) [[]]

let special_piece_maker mode = 
  match mode with 
  | 1 -> [0;0;0;0]
  | 2 -> [1;1;1;1]
  | 3 -> [Random.int 3; Random.int 3; Random.int 3; Random.int 3]
  | _ -> [0;0;0;0]

let load_game players rows cols board turn moves connect colors mode = {
  num_players = players;
  rows = rows;
  cols = cols; 
  gameboard = board;
  player_turn = turn;
  total_moves = moves;
  connect_num = connect;
  colors = colors;
  game_mode = mode;
  special_pieces = special_piece_maker mode;
}

let start_game rows cols players connect colors mode = 
  load_game players rows cols (create_board cols) 0 0 connect colors mode

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

let move state col piece = 
  if col > state.cols || col <= 0 then Invalid
  else if verify_placement state.gameboard (col - 1) state.rows 
  then let new_board = update_board state.gameboard (col - 1) piece in 
    Valid {state with gameboard = new_board; 
                      player_turn = (state.player_turn + 1) 
                                    mod state.num_players;
                      total_moves = state.total_moves + 1}
  else Invalid

let get_gameboard state = 
  state.gameboard

let get_player_turn state = 
  state.player_turn + 1

let format p = 
  match p with 
  | Normal x -> "\"Normal " ^ (string_of_int x) ^ "\""
  | Anvil x -> "\"Anvil " ^ (string_of_int x) ^ "\""
  | Wall -> "\"Wall " ^ "\""
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
  let moves_str = string_of_int st.total_moves in 
  String.concat "," [
    String.concat "" ["{\"num_players\":\""; np_str; "\""];
    String.concat "" ["\"rows\":\""; row_str; "\""];
    String.concat "" ["\"cols\":\""; col_str; "\""];
    String.concat "" ["\"gameboard\":"; board_str];
    String.concat "" ["\"player_turn\":\""; turn_str; "\""];
    String.concat "" ["\"total_moves\":\""; moves_str; "\"}"]
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


let rec check_col_match column player count connect =
  if count = connect then true else 
    match column with
    | [] -> false
    | x :: xs -> let piece_num = get_piece_player x in 
      if piece_num = player then check_col_match xs player (count + 1) connect
      else false


let check_col_win board player col connect = 
  let column = List.nth board col in 
  if List.length column < connect
  then false else check_col_match column player 0 connect


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
  if state.total_moves / state.num_players < 3 then false else
    let col = col - 1 in
    let board = state.gameboard in
    if check_col_win board player col state.connect_num then true 
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

let check_draw state =
  state.total_moves = (state.cols * state.rows)

(* TODO: add tests, maybe take draw/win out of mli *)
let check_status state player col =
  if check_win state player col then Win player else
  if check_draw state then Draw else Play

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