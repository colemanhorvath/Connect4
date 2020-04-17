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