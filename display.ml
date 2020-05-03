(** Represents an exception that should never occur if the code works as 
    expected. *)
exception InternalException

(** [rec get_string_from_player player colors] is the string character that 
    represents [player] based on the corresponding color in list [colors]. *)
let rec get_string_from_player player colors i =
  match colors with
  | [] -> raise InternalException
  | color::t -> 
    if player = i then (ANSITerminal.(print_string [color] "o");)
    else get_string_from_player player t (i+1)

(** [get_string_from_wall] is the string character that represents a wall. *)
let get_string_from_wall () = 
  (ANSITerminal.(print_string [white] "■");)

(** [get_string_from_piece piece st] is the string character that represents 
    the specific [piece] on the board based on colors in state [st]. *)
let get_string_from_piece st piece =
  let colors = Game_mechanics.get_colors st in
  match piece with
  | Game_mechanics.None -> raise InternalException
  | Game_mechanics.Wall -> get_string_from_wall ()
  | Game_mechanics.Normal player -> get_string_from_player player colors 1
  | Game_mechanics.Anvil player -> get_string_from_player player colors 1
  | Game_mechanics.Bomb player -> get_string_from_player player colors 1
  | Game_mechanics.Force player -> get_string_from_player player colors 1

(** [rec find_piece st row_index curr_index col] is recursively the string 
    representation of a piece at [row_index] of column [col] of state [st]. 
    [curr_index] is initially the number of rows. *)
let rec find_piece st row_index curr_index col = 
  try
    match col with
    | [] -> raise InternalException
    | h::t -> 
      if curr_index != row_index then find_piece st row_index (curr_index-1) t
      else get_string_from_piece st h
  with
  | InternalException -> print_endline("Internal exception reached.")

(** [rec row_string st board row_index] is recursively the string
    representation of a single row at [row_index] of [board] of state [st]. *)
let rec row_string st board row_index =
  let dimensions = Game_mechanics.get_dimensions st in
  let num_rows = fst dimensions in
  print_string " ";
  match board with 
  | [] -> ();
  | col::t -> 
    let num_pieces_in_col = List.length col in
    if row_index <= (num_rows - num_pieces_in_col) then (
      ANSITerminal.(print_string [cyan] ".");
      row_string st t row_index;
    )
    else (
      find_piece st row_index num_rows (List.rev col); 
      row_string st t row_index
    )

(** [rec board_string st board row_index] is recursively the string 
    representation of the game [board] of state [st] with initial [row_index] 
    of the number of rows. *)
let rec board_string st board row_index = 
  match row_index with
  | 0 -> ();
  | x -> 
    board_string st board (row_index-1); 
    print_newline ();
    row_string st board row_index

let print_board state = 
  let board = Game_mechanics.get_gameboard state in
  let num_rows = fst (Game_mechanics.get_dimensions state) in

  board_string state board num_rows;
  print_newline ();
  print_newline ()

let print_start_turn state =
  let curr_player = Game_mechanics.get_player_turn state in
  print_board state;
  print_endline(String.concat "" 
                  ["Player "; string_of_int curr_player; ", your move."])

let print_help () =
  print_newline ();
  print_endline("Here are the possible commands");
  print_endline("[help] - displays list of possible commands");
  print_endline("[print] - pretty prints the current board");
  print_endline("[place column] - places a piece on the board at column, \
                 starting at column 1");
  print_endline("[save filepath] - saves a json file of the current game \
                 state to filepath");
  print_newline ()

let pretty_print_string str =
  print_newline();
  print_endline(str);
  print_newline()
