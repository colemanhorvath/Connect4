open OUnit2
open Command

(** do we need this? *)
let rec print_st_list lst = 
  match lst with
  | [] -> ()
  | h::t -> print_string h; print_newline(); print_st_list t; ()

(** [make_win_test name input expected_output] 
    makes an OUnit test named [name] that assert the quality of 
    [expected_output] with [parse input], where the exception Empty 
    will be Save ["emp ty"] and the exception Malformed will be 
    Save ["mal formed"]  *)
let make_parse_test
    (name: string)
    (input: string)
    (expected_output: command) = 
  name >:: (fun _ ->
      let com = try parse input with
        | Empty -> Save ["emp ty"]
        | Malformed -> Save ["mal formed"] in
      assert_equal com expected_output)

let parse_tests = [
  make_parse_test "parse print" "print" Print;
  make_parse_test "parse start" "start" Start;
  make_parse_test "parse help" "help" Help;
  make_parse_test "parse help me" "help me" (Save ["mal formed"]);
  make_parse_test "parse save test.json" "save test.json" (Save ["test.json"]);
  make_parse_test "parse place 5" "place 5" (Place ["5"]);
  make_parse_test "parse place" "place" (Save ["mal formed"]);
  make_parse_test "parse load test.json" "load test.json" (Load ["test.json"]);
  make_parse_test "print" "print" Print;
  make_parse_test "parse print something" "print something" 
    (Save ["mal formed"]);
  make_parse_test "parse save test dot json" "save test dot json" 
    (Save ["test"; "dot"; "json"]);
  make_parse_test "parse save" "save" (Save ["mal formed"]);
  make_parse_test "parse spaced out command" "  save   a     thing"
    (Save ["a"; "thing"]);
  make_parse_test "parse empty string" "" (Save ["emp ty"]);
  make_parse_test "parse potato" "potato" (Save ["mal formed"]);
  make_parse_test "parse settings" "settings" (Settings);
  make_parse_test "parse Settings" "potato" (Save ["mal formed"]);
  make_parse_test "parse quit" "quit" Quit;
  make_parse_test "parse exit" "exit" Exit;
  make_parse_test "parse exit game" "exit game" (Save ["mal formed"]);
  make_parse_test "parse hand me" "hand me" (Save ["mal formed"]);
  make_parse_test "parse hand" "hand" Hand;
]

let suite =
  "test suite for command"  >::: List.flatten [
    parse_tests
  ]

let _ = run_test_tt_main suite