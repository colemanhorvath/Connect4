exception InternalException

(** [get_string_from_player player] is the string character that represents 
    [player]. *)
let get_string_from_player player = 
  (* TODO: handle multiple players *)
  if player = 1 then "x" else "o"

(** [get_string_from_piece piece] is the string character that represents the 
    specific [piece] on the board. *)
let get_string_from_piece piece =
  match piece with
  | Game_mechanics.Normal player -> get_string_from_player player
  | Game_mechanics.Bomb player -> get_string_from_player player
  | Game_mechanics.None -> raise InternalException

(** [rec find_piece row_index curr_index col] is recursively the string 
    representation of a piece at [row_index] of column [col]. [curr_index] is
    initially the number of rows. *)
let rec find_piece row_index curr_index col = 
  try
    match col with
    | [] -> raise InternalException
    | h::t -> 
      if curr_index != row_index then find_piece row_index (curr_index-1) t
      else get_string_from_piece h
  with
  | InternalException -> print_endline("Internal exception reached."); ""

(** [rec row_string board row_index] is recursively the string representation 
    of a single row at [row_index] of [board]. *)
let rec row_string board row_index =
  (* TODO: change col and row size *)
  let num_cols = 7 in
  let num_rows = 7 in
  match board with 
  | [] -> ""
  | col::t -> 
    let num_pieces_in_col = List.length col in
    if row_index <= (num_cols - num_pieces_in_col) then 
      String.concat " " ["."; row_string t row_index] 
    else 
      String.concat " " [find_piece row_index num_rows (List.rev col); 
                         row_string t row_index]

(** [rec board_string board row_index] is recursively the string 
    representation of the game [board] with initial [row_index] of the 
    number of rows. *)
let rec board_string board row_index = 
  match row_index with
  | 0 -> ""
  | x -> String.concat "\n" [board_string board (row_index-1); 
                             row_string board row_index]

let print_board state = 
  let board = Game_mechanics.get_gameboard state in
  (* TODO: change col and row size *)
  let num_rows = 7 in

  let output = board_string board num_rows in
  print_endline(output);