type piece =
  |None
  |Normal of int
  |Bomb of int 

type board = piece list list

type t = {
  num_players : int;
  rows : int;
  cols : int;
  gameboard : board;
  player_turn : int;
}

type move_result =  Valid of t | Invalid

(** [board_helper col_counter acc] is a board  *)
let rec board_helper col_counter acc =
  if col_counter = 0 then acc else board_helper (col_counter - 1) ([] :: acc)

(** [create_board cols] is a board with [cols] number of columns *)
let create_board cols = 
  board_helper (cols - 1) [[]]

let start_game rows cols players = 
  {
    num_players = players;
    rows = rows;
    cols = cols; 
    gameboard = create_board cols;
    player_turn = 0;
  }

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
  if verify_placement state.gameboard (col - 1) state.rows 
  then let new_board = update_board state.gameboard (col - 1) piece in 
    Valid {state with gameboard = new_board; 
                      player_turn = (state.player_turn + 1) 
                                    mod state.num_players}
  else Invalid

let get_gameboard state = 
  state.gameboard

let get_player_turn state = 
  state.player_turn + 1

let format p = 
  match p with 
  | Normal x -> "\"Normal " ^ (string_of_int x) ^ "\""
  | _ -> "\"Other " ^ "error" ^ "\""

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


let rec check_row_match board row player col count last = 
  if count = 4 then true
  else if col = last then false
  else 
    let col_len = (List.nth board col |> List.length) in 
    if col_len <= row 
    then check_row_match board row player (col + 1) 0 last
    else let piece = List.nth (List.nth board col) (col_len - row - 1) in
      let piece_num = get_piece_player piece in
      if piece_num = player 
      then check_row_match board row player (col + 1) (count + 1) last
      else check_row_match board row player (col + 1) 0 last


let check_win state player col =
  let col = col - 1 in
  let board = state.gameboard in
  if check_col_win board player col then true 
  else let row = (List.nth board col |> List.length) - 1 in 
    let first = if (col - 3) > 0 then col - 3 else 0 in 
    let last = if (col + 4) < state.cols then col + 4 else state.cols in 
    check_row_match board row player first 0 last