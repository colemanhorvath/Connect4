let load_from_phrase object_phrase =
  match object_phrase with 
  | [] -> raise Command.Malformed
  | h::_ -> Save.load h

let rec play_game state = 
  let curr_player = Game_mechanics.get_player_turn state in
  print_endline(String.concat "" ["Player "; string_of_int curr_player; ", your move."]);
  (* TODO: instead of asking a column, allow printing, saving, displaying and help? *)
  let col_string = (read_line (print_endline "What column would you like to place your piece?")) in
  try 
    (* raises failure if not a valid int *)
    let col = int_of_string col_string in
    (* TODO: allow different game pieces to be played *)
    let piece = Game_mechanics.create_piece "normal" curr_player in
    let move_result = Game_mechanics.move state col piece in
    match move_result with
    | Game_mechanics.Valid new_state ->
      begin
        match (Game_mechanics.check_status new_state curr_player col) with
        | Game_mechanics.Win win_player -> 
          print_endline(String.concat "" ["Congrats Player "; string_of_int win_player; ", you won!"]);
        | Game_mechanics.Draw ->
          print_endline("Game over! There is a Draw.");
        | Game_mechanics.Play ->
          play_game new_state
      end
    | Game_mechanics.Invalid -> raise (Failure "")
  with
    | Failure _ -> 
      print_endline("Invalid empty column number provided. Please try again.");
      play_game state

let rec game_setup () = 
  let command = (read_line (print_endline "Please type \"start\" to start the game, or \"load [filename]\" to load a previously saved game.")) in
  try
    match Command.parse command with 
    | Command.Start -> 
      (* TODO: allow different game sizes or player numbers *)
      let game_state = Game_mechanics.start_game 7 7 2 in
      print_endline "Game state started!";
      play_game game_state
    | Command.Load object_phrase -> 
      let load_state = load_from_phrase object_phrase in
      begin
        match load_state with 
        | Save.Load_Success game_state ->
          print_endline "Game state loaded!";
          play_game game_state
        | Save.Load_Failure str -> 
          print_endline(String.concat "" ["Game failed to load at file"; str; ", please try again."]);
          game_setup ()
      end
    | _ -> raise Command.Malformed
  with
    | Command.Empty -> 
      print_endline "Empty command given. Please try again.";
      game_setup ()
    | Command.Malformed -> 
      print_endline "Malformed command given. Please try again.";
      game_setup ()

let main () =
  (* TODO help message? *)
  (* Display.welcome_message ()  *)
  (* settings_message *)
  (* print_endline "Please type \"Start\" to start the game, or Load [filename] to load a previously saved game."
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name *)
  
  (* ANSITerminal.(resize 100 100); *)
  ANSITerminal.(print_string [blue] "\nWelcome to Connect 4!\n");
  game_setup ()

(* Runs the game. *)
let () = main ()


  (* try
    match command with 
    | Print ->
    | Start -> Game_Mechanics.start_game 7 7 2
    | Place object_phrase ->
    | Save object_phrase ->
    | Load object_phrase ->
  with
    | Empty ->
    | Malformed -> *)
