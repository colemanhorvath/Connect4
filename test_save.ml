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
        | Load_Failure s -> (Game_mechanics.load_game 0 0 0 [] 0 0)
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

let new_game_standard = Game_mechanics.start_game 7 7 2
let new_game_9x9 = Game_mechanics.start_game 9 9 2
let test_game_board = [
  [Normal 1]; 
  [Normal 1; Normal 2]; 
  [Normal 2]; 
  [Normal 1];
  [];
  [];
  []
]
let test_game = load_game 2 7 7 test_game_board 1 5
let blank_game = load_game 0 0 0 [] 0 0

let new_game_standard_string = "{\"num_players\":\"2\",\"rows\":\"7\",\"cols\":\"7\",\"gameboard\":[[],[],[],[],[],[],[]],\"player_turn\":\"0\",\"total_moves\":\"0\"}"
let new_game_9x9_string = "{\"num_players\":\"2\",\"rows\":\"9\",\"cols\":\"9\",\"gameboard\":[[],[],[],[],[],[],[],[],[]],\"player_turn\":\"0\",\"total_moves\":\"0\"}"
let test_game_string = "{\"num_players\":\"2\",\"rows\":\"7\",\"cols\":\"7\",\"gameboard\":[[{\"type\":\"Normal\",\"player\":\"1\"}],[{\"type\":\"Normal\",\"player\":\"1\"},{\"type\":\"Normal\",\"player\":\"2\"}],[{\"type\":\"Normal\",\"player\":\"2\"}],[{\"type\":\"Normal\",\"player\":\"1\"}],[],[],[]],\"player_turn\":\"1\",\"total_moves\":\"5\"}"

let load_tests = [
  make_load_test "load a new standard game" "new_game_standard.json" 
    new_game_standard;
  make_load_test "load a new 9x9 game" "new_game_9x9.json" new_game_9x9;
  make_load_test "load a game in progress" "test.json" test_game;
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
    test_game_string
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