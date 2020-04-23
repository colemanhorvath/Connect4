type piece =
  | None
  | Normal of int
  | Bomb of int 

type status = 
  | Play
  | Draw
  | Win of int

type board = piece list list

type t = {
  num_players : int;
  rows : int;
  cols : int;
  gameboard : board;
  player_turn : int;
  total_moves : int;
}

type move_result =  Valid of t | Invalid

(** [board_helper col_counter acc] is a board  *)
let rec board_helper col_counter acc =
  if col_counter = 0 then acc else board_helper (col_counter - 1) ([] :: acc)

(** [create_board cols] is a board with [cols] number of columns *)
let create_board cols = 
  board_helper (cols - 1) [[]]

let load_game players rows cols board turn moves = {
  num_players = players;
  rows = rows;
  cols = cols; 
  gameboard = board;
  player_turn = turn;
  total_moves = moves;
}

let start_game rows cols players = 
  load_game players rows cols (create_board cols) 0 0

let create_piece piece_type player = 
  match piece_type with 
  | "normal" -> Normal player
  | "bomb" -> Bomb player
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
  | _ -> "\"Other " ^ "error" ^ "\""

(** [string_of_piece p] is the JSON string representation of piece [p]. *)
let string_of_piece p = 
  match p with 
  | None -> "{\"type\":\"None\",\"player\":\"-1\"}"
  | Normal p ->
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Normal\",\"player\":\""; player; "\"}"]
  | Bomb p -> 
    let player = string_of_int p in 
    String.concat "" ["{\"type\":\"Bomb\",\"player\":\""; player; "\"}"]

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

let get_piece_player p= 
  match p with 
  | Normal x -> x
  | Bomb x -> x
  | None -> 10


let rec check_col_match column player count =
  if count = 4 then true else 
    match column with
    | [] -> false
    | x :: xs -> let piece_num = get_piece_player x in 
      if piece_num = player then check_col_match xs player (count + 1) 
      else false


let check_col_win board player col = 
  let column = List.nth board col in 
  if List.length column < 4 
  then false else check_col_match column player 0


let rec check_row_match board row player col count last max_cols = 
  if count = 4 then true
  else if col >= max_cols then false
  else if col = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_row_match board row player (col + 1) 0 last max_cols
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_row_match board row player (col + 1) (count + 1) last max_cols
      else check_row_match board row player (col + 1) 0 last max_cols


let rec check_diagonal_lr_match board row player col count inc last max_cols max_rows = 
  if count = 4 then true
  else if row < 0 || row >= max_rows then false
  else if col < 0 || col >= max_cols then false
  else if inc = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_diagonal_lr_match board (row + 1) player (col + 1) 0 (inc + 1) last max_cols max_rows
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_diagonal_lr_match board (row + 1) player (col + 1) (count + 1) (inc + 1) last max_cols max_rows
      else check_diagonal_lr_match board (row + 1) player (col + 1) 0 (inc + 1) last max_cols max_rows

let rec check_diagonal_rl_match board row player col count inc last max_cols max_rows = 
  if count = 4 then true
  else if row < 0 || row >= max_rows then false
  else if col < 0 || col >= max_cols then false
  else if inc = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_diagonal_rl_match board (row - 1) player (col + 1) 0 (inc + 1) last max_cols max_rows
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_diagonal_rl_match board (row - 1) player (col + 1) (count + 1) (inc + 1) last max_cols max_rows
      else check_diagonal_rl_match board (row - 1) player (col + 1) 0 (inc + 1) last max_cols max_rows

let check_win state player col =
  if state.total_moves / state.num_players < 3 then false else
    let col = col - 1 in
    let board = state.gameboard in
    if check_col_win board player col then true 
    else 
      let row = (List.nth board col |> List.length) - 1 in 
      let first = (max (col - 3) 0)in 
      let last = (min (col + 4) state.cols) in 
      if check_row_match board row player first 0 last state.cols then true 
      else 
        let rowbegin = (max (row - 3) 0) in
        let rowend = (min (row + 3) (state.rows - 1)) in 
        let colbegin = (max (col - 3) 0) in
        let colend = (min (col + 3) (state.cols - 1)) in 
        let start = (min (row - rowbegin) (col - colbegin)) in 
        let range = (min (rowend - rowbegin) (colend - colbegin)) + 1 in 
        if range < 4 then false else
        if check_diagonal_lr_match board (row - start) player 
            (col - start) 0 0 7 state.cols state.rows then true 
        else  
          let rowbegin = (min (row + 3) (state.rows - 1)) in
          let rowend = (max (row - 3) 0) in 
          let start = (min (rowbegin - row) (col - colbegin)) in 
          let range = (min (rowbegin - rowend) (colend - colbegin)) + 1 in 
          if range < 4 then false 
          else 
            check_diagonal_rl_match board (row + start) player 
              (col - start) 0 0 7 state.cols state.rows

let check_draw state =
  state.total_moves = (state.cols * state.rows)

(* TODO: add tests, maybe take draw/win out of mli *)
let check_status state player col =
  if check_win state state.player_turn col then Win state.player_turn else
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