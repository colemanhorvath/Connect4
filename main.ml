(** [quit_game ()] exits the program. *)
let quit_game () = 
  exit 0

(** [start_game] starts a basic game of size 7x7 with 2 players. *)
let start_game = 
  (* TODO: allow different game sizes or player numbers *)
  Game_mechanics.start_game 7 7 2 4 ["red"; "yellow"] 1

(** [col_from_phrase object_phrase] is the column number from string list 
    [object_phrase].
    Raises [Failure str] if not a valid int or object_phrase is empty. *)
let col_from_phrase object_phrase = 
  match object_phrase with 
  | [] -> raise (Failure "")
  | h::_ -> int_of_string h

(** [load_from_phrase object_phrase] is the load_result from loading in the
    file present in [object_phrase].
    Raises [Command.Malformed] if loading failed or object_phrase is empty. *)
let load_from_phrase object_phrase =
  match object_phrase with 
  | [] -> raise Command.Malformed
  | h::_ -> Save.load h

(** [save_from_phrase state object_phrase] is the save_result from saving the
    [state] in the file present in [object_phrase].
    Raises [Command.Malformed] if saving failed or object_phrase is empty. *)
let save_from_phrase state object_phrase =
  match object_phrase with 
  | [] -> raise Command.Malformed
  | h::_ -> Save.save state h

(** [load_handler object_phrase] is the state from loading in the file in
    [object_phrase].
    Raises [Failure str] if the file could not be loaded, where [str] is the 
    filename. *)
let load_handler object_phrase = 
  match load_from_phrase object_phrase with
  | Save.Load_Success game_state ->
    game_state
  | Save.Load_Failure str -> 
    raise (Failure str)

(** [save_handler state object_phrase] is [true] if the file in [object_phrase]
    was successfully saved, [false] otherwise.
    Raises [Command.Malformed] if saving failed or object_phrase is empty. *)
let save_handler state object_phrase = 
  match save_from_phrase state object_phrase with
  | Save.Save_Success str ->
    Display.pretty_print_string(String.concat "" ["Game saved to file "; str]);
    true
  | Save.Save_Failure str -> 
    Display.pretty_print_string(String.concat "" 
                                  ["Game failed to save to file "; str; 
                                   ", please try again."]);
    false 

(** [check_win_condition state player col] checks to see if a game was won by
    [player] after they made a move in column [col] in [state]. Prints and 
    quits the game on a win or draw, and continues if the game is not over. *)
let check_win_condition state player col = 
  match (Game_mechanics.check_status state player col) with
  | Game_mechanics.Win win_player -> 
    Display.print_board state;
    Display.pretty_print_string(String.concat "" ["Congrats Player "; 
                                                  string_of_int win_player; ", you won!"]);
    quit_game ()
  | Game_mechanics.Draw ->
    Display.print_board state;
    Display.pretty_print_string("Game over! There is a Draw.");
    quit_game ()
  | Game_mechanics.Play -> ()

(** [place_piece state object_phrase player] is the new state after a piece 
    has been placed for the given [player] at the column in [object_phrase]. 
    The new state is the same as the current state if the move is invalid. *)
let place_piece state object_phrase player = 
  try 
    let col = col_from_phrase object_phrase in

    (* TODO: allow different game pieces to be played *)
    let piece = Game_mechanics.create_piece "normal" player in
    let move_result = Game_mechanics.move state col piece in
    match move_result with
    | Game_mechanics.Valid new_state -> 
      (check_win_condition new_state player col);
      new_state
    | Game_mechanics.Invalid -> raise (Failure "")
  with
  | Failure _ -> 
    Display.pretty_print_string("Invalid column number. Please try again.");
    state

(** [rec play_game state] recursively asks for player input one step at a time 
    and handles all possible commands after a game has been started. This 
    includes [help], [print], [place], and [save]. If a command is empty or 
    malformed, an explanation is printed and another command is prompted for. *)
let rec play_game state = 
  let curr_player = Game_mechanics.get_player_turn state in
  let command = 
    (read_line (print_endline "Enter a command (type \"help\" if you need \
                               it)")) in
  try
    match Command.parse command with 
    | Command.Help ->
      Display.print_help ();
      play_game state
    | Command.Print ->
      Display.print_board state;
      play_game state
    | Command.Save object_phrase -> 
      let saved = save_handler state object_phrase in
      if not saved then play_game state else quit_game ()
    | Command.Place object_phrase ->
      let new_state = place_piece state object_phrase curr_player in
      Display.print_start_turn new_state;
      play_game new_state
    | _ -> raise Command.Malformed
  with
  | Command.Empty -> 
    Display.pretty_print_string "Empty command given. Please try again.";
    play_game state
  | Command.Malformed ->
    Display.pretty_print_string "Invalid command provided. Please try again.";
    play_game state

(** [rec game_setup ()] recursively asks for player input to either [start] the
    game or [load] a game file and play. If a command is empty, malformed, or a
    failure, an explanation is printed and another command is prompted for. *)
let rec game_setup () = 
  let command = 
    (read_line (print_endline "Please type \"start\" to start the game, or \
                               \"load [filename]\" to load a previously saved \
                               game.")) in
  try
    match Command.parse command with 
    | Command.Start -> 
      let game_state = start_game in
      Display.pretty_print_string "Game state started!";
      Display.print_start_turn game_state;
      play_game game_state
    | Command.Load object_phrase -> 
      let game_state = load_handler object_phrase in
      Display.pretty_print_string "Game state loaded!";
      Display.print_start_turn game_state;
      play_game game_state
    | _ -> raise Command.Malformed
  with
  | Command.Empty -> 
    Display.pretty_print_string "Empty command given. Please try again.";
    game_setup ()
  | Command.Malformed -> 
    Display.pretty_print_string "Malformed command given. Please try again.";
    game_setup ()
  | Failure str -> 
    Display.pretty_print_string(String.concat "" 
                                  ["Game failed to load at file "; str; 
                                   ", please try again."]);
    game_setup ()

(** [main ()] prints a welcome message and runs the game setup. *)
let main () =
  ANSITerminal.(print_string [blue] "\nWelcome to Connect 4!\n");
  game_setup ()

(* Runs the game. *)
let () = main ()
