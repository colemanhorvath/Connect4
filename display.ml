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

(** [get_color colors index] is the color at position [index] in [colors] 
    or the red style otherwise *) 
let get_color colors index =
  match List.nth_opt colors index with 
  | Some c -> c
  | None -> ANSITerminal.red

let print_player state = 
  let curr_player = Game_mechanics.get_player_turn state in
  let color = get_color (Game_mechanics.get_colors state) (curr_player - 1) in
  (ANSITerminal.(print_string [color] (String.concat "" 
                                         ["Player "; string_of_int curr_player;
                                          ", your move."]);));
  print_newline ();
  print_newline ()

let print_board state = 
  let board = Game_mechanics.get_gameboard state in
  let num_rows = fst (Game_mechanics.get_dimensions state) in
  board_string state board num_rows;
  print_newline ();
  print_newline ()

let print_start_turn state =
  print_board state;
  print_player state;
  if Game_mechanics.is_forced state then 
    let force_player = Game_mechanics.get_prev_player_turn state in 
    let force_color = 
      get_color (Game_mechanics.get_colors state) (force_player - 1) in
    let force_warning = 
      String.concat "" [
        "You are currently placing one of Player "; 
        string_of_int (force_player);
        "'s pieces."] in 
    ANSITerminal.(print_string [force_color] force_warning); 
    print_newline ()

let print_help state =
  print_newline ();
  print_endline("Here are the possible commands");
  print_endline("[help] - displays list of possible commands");
  print_endline("[print] - pretty prints the current board");
  print_endline("[hand] - displays list of special pieces in your hand \
                 (0 for all special pieces if playing regular Connect 4)");
  print_endline("[place column] - places a normal piece on the board at \
                 [column], starting at column 1");
  if Game_mechanics.get_gamemode state != 1 then (
    print_endline("[place bomb column] - places a bomb on the board at \
                   [column], starting at column 1. You will then be prompted \
                   for the row and column of the piece you want to remove");
    print_endline("[place force column] - places a force on the board at \
                   [column], starting at column 1. The next player will then \
                   first put down a normal piece of your color before taking \
                   their own turn");
    print_endline("[place anvil column] - places an anvil on the board at \
                   [column], starting at column 1. All other pieces in that \
                   column will be removed");
    print_endline("[place wall column] - places a wall on the board at \
                   [column], starting at column 1. The wall does not count \
                   for any player's color and after placing it you will be \
                   prompted to place another piece"); )
  else if Game_mechanics.is_standard state then (
    print_endline("[toggle] - toggles AI Player 2. Only usable in a regular \
                   Connect 4 2 player game"); )
  else     
    ();
  print_endline("[save filepath] - saves a json file of the current game \
                 state to [filepath]");
  print_endline("[quit] - ends the game and returns to the main menu ");
  print_newline ()

let print_hand state player =
  print_newline ();
  print_endline("Here are the pieces in your hand");
  print_endline("anvils: " ^ string_of_int 
                  (Game_mechanics.get_num_of_piece_type state player "anvil"));
  print_endline("walls: " ^ string_of_int 
                  (Game_mechanics.get_num_of_piece_type state player "wall"));
  print_endline("bombs: " ^ string_of_int 
                  (Game_mechanics.get_num_of_piece_type state player "bomb"));
  print_endline("force pieces: " ^ string_of_int 
                  (Game_mechanics.get_num_of_piece_type state player "force"));
  print_newline ()

let pretty_print_string str =
  print_newline();
  print_endline(str);
  print_newline()
