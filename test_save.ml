open OUnit2
open Save
open Game_mechanics


let make_load_test 
    (name : string)
    (input : string)
    (expected_output: Game_mechanics.t) = 
  name >:: (fun _ -> 
      let load_attempt = Save.load input in 
      let loaded_state = 
        match load_attempt with 
        | Load_Failure s -> 
          (Game_mechanics.load_game 0 0 0 [] 0 0 [] 0 [] false false)
        | Load_Success st -> st in 
      let is_correct = loaded_state = expected_output in
      assert_equal is_correct true ~printer:string_of_bool)

let make_to_json_test
    (name : string)
    (state : Game_mechanics.t)
    (expected_output : string) = 
  name >:: (fun _ -> 
      let json_string = Game_mechanics.to_json_string state in 
      assert_equal json_string expected_output ~printer:(fun x->x))

let make_save_test
    (name : string)
    (state : Game_mechanics.t)
    (filename : string)
    (expected_output : string) = 
  name >:: (fun _ ->
      let saved = Save.save state filename in 
      let f = 
        match saved with
        | Save_Failure _ -> "failed save"
        | Save_Success s -> s in 
      if f = "failed save" then assert_equal f expected_output else
        let ic = open_in f in 
        try let saved_json = input_line ic in
          close_in ic;
          assert_equal saved_json expected_output
        with e -> close_in_noerr ic; raise e)

let new_game_standard = 
  Game_mechanics.start_game 7 7 2 4 [ANSITerminal.yellow; ANSITerminal.red] 1
let new_game_9x9 = 
  Game_mechanics.start_game 9 9 2 4 [ANSITerminal.yellow; ANSITerminal.red] 1
let test_game_board = [
  [Normal 1]; 
  [Normal 1; Normal 2]; 
  [Normal 2]; 
  [Normal 1];
  [];
  [];
  []
]
let test_sps = [
  [1;1;1;1];
  [1;1;1;1]
]
let test_game = load_game 2 7 7 test_game_board 1 4 [ANSITerminal.yellow; ANSITerminal.red] 2 test_sps false false
let blank_game = load_game 0 0 0 [] 0 0 [] 0 [] false false 
let custom_game_board = [
  [Normal 1];
  [Anvil 1];
  [Force 2; Normal 2];
  [Normal 1];
  [Normal 3; Wall];
  [Normal 3];
  [Force 1];
  [Bomb 2]
]
let custom_sps = [
  [0;1;1;0];
  [1;1;0;0];
  [1;0;1;1]
]
let customized_game = load_game 3 8 8 custom_game_board 2 5 [ANSITerminal.green; ANSITerminal.yellow; ANSITerminal.red] 2 custom_sps true false

let new_game_standard_string = "{\"num_players\":\"2\",\"rows\":\"7\",\"cols\":\"7\",\"gameboard\":[[],[],[],[],[],[],[]],\"player_turn\":\"0\",\"connect_num\":\"4\",\"colors\":[\"Yellow\",\"Red\"],\"game_mode\":\"1\",\"special_pieces\":[[\"0\",\"0\",\"0\",\"0\"],[\"0\",\"0\",\"0\",\"0\"]],\"is_player_forced\":\"false\",\"is_awaiting_bomb\":\"false\"}"
let new_game_9x9_string = "{\"num_players\":\"2\",\"rows\":\"9\",\"cols\":\"9\",\"gameboard\":[[],[],[],[],[],[],[],[],[]],\"player_turn\":\"0\",\"connect_num\":\"4\",\"colors\":[\"Yellow\",\"Red\"],\"game_mode\":\"1\",\"special_pieces\":[[\"0\",\"0\",\"0\",\"0\"],[\"0\",\"0\",\"0\",\"0\"]],\"is_player_forced\":\"false\",\"is_awaiting_bomb\":\"false\"}"
let test_game_string = "{\"num_players\":\"2\",\"rows\":\"7\",\"cols\":\"7\",\"gameboard\":[[{\"type\":\"Normal\",\"player\":\"1\"}],[{\"type\":\"Normal\",\"player\":\"1\"},{\"type\":\"Normal\",\"player\":\"2\"}],[{\"type\":\"Normal\",\"player\":\"2\"}],[{\"type\":\"Normal\",\"player\":\"1\"}],[],[],[]],\"player_turn\":\"1\",\"connect_num\":\"4\",\"colors\":[\"Yellow\",\"Red\"],\"game_mode\":\"2\",\"special_pieces\":[[\"1\",\"1\",\"1\",\"1\"],[\"1\",\"1\",\"1\",\"1\"]],\"is_player_forced\":\"false\",\"is_awaiting_bomb\":\"false\"}"
let customized_game_string = "{\"num_players\":\"3\",\"rows\":\"8\",\"cols\":\"8\",\"gameboard\":[[{\"type\":\"Normal\",\"player\":\"1\"}],[{\"type\":\"Anvil\",\"player\":\"1\"}],[{\"type\":\"Force\",\"player\":\"2\"},{\"type\":\"Normal\",\"player\":\"2\"}],[{\"type\":\"Normal\",\"player\":\"1\"}],[{\"type\":\"Normal\",\"player\":\"3\"},{\"type\":\"Wall\",\"player\":\"0\"}],[{\"type\":\"Normal\",\"player\":\"3\"}],[{\"type\":\"Force\",\"player\":\"1\"}],[{\"type\":\"Bomb\",\"player\":\"2\"}]],\"player_turn\":\"2\",\"connect_num\":\"5\",\"colors\":[\"Green\",\"Yellow\",\"Red\"],\"game_mode\":\"2\",\"special_pieces\":[[\"0\",\"1\",\"1\",\"0\"],[\"1\",\"1\",\"0\",\"0\"],[\"1\",\"0\",\"1\",\"1\"]],\"is_player_forced\":\"true\",\"is_awaiting_bomb\":\"false\"}"


let load_tests = [
  make_load_test "load a new standard game" "new_game_standard.json" 
    new_game_standard;
  make_load_test "load a new 9x9 game" "new_game_9x9.json" new_game_9x9;
  make_load_test "load a game in progress" "test.json" test_game;
  make_load_test "load a heavily customized game" "customized_game.json" 
    customized_game;
  make_load_test "load a file that doesn't exist" "missing.json" blank_game;
  make_load_test "load a non-json" "README.md" blank_game;
  make_load_test "load a non-conforming json" "incorrect.json" blank_game
]

let to_json_tests = [
  make_to_json_test "Get a JSON string of a new standard game" new_game_standard
    new_game_standard_string;
  make_to_json_test "JSON string of a new 9x9 game" new_game_9x9
    new_game_9x9_string;
  make_to_json_test "JSON string of a game in progress" test_game
    test_game_string;
  make_to_json_test "JSON string of a heavily customized game" customized_game
    customized_game_string
]

let save_tests = [
  make_save_test "Save a new standard game to test1.json" new_game_standard
    "test1.json" new_game_standard_string;
  make_save_test "Save a new 9x9 game to test2.json" new_game_9x9
    "test2.json" new_game_9x9_string;
  make_save_test "Save a game in progress to test3.json" test_game
    "test3.json" test_game_string;
  make_load_test "Load a game after it is saved" "test3.json" test_game
]

let suite =
  "test suite for save"  >::: List.flatten [
    load_tests;
    to_json_tests;
    save_tests
  ]

let _ = run_test_tt_main suite