open OUnit2
open Game_mechanics

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ format h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (format h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_list_list lst acc] pretty-prints list [lst], using pp_list
    to pretty-print each element of [lst], and storing the resulting
    printing string in acc. *)
let rec pp_list_list lst acc = 
  match lst with 
  | [] -> "[ " ^ acc ^ " ]"
  | x :: xs -> pp_list_list xs (acc ^ (pp_list x))

(** [print_board state] prints the gameboard in state *)
let print_board state = 
  Format.fprintf Format.std_formatter "%s\n" 
    (pp_list_list (get_gameboard state) "")



(** [make_move current_state col] makes a move adding a piece to col *)
let make_move current_state col =
  let player_turn = get_player_turn current_state in 
  let piece = create_piece "normal" player_turn in 
  let new_state = move current_state col piece in 
  match new_state with 
  | Valid moved -> moved
  | Invalid -> current_state

(** [test_moves state move_lst] makes the moves in move_lst *)
let rec test_moves state move_lst = 
  match move_lst with 
  | [] -> state
  | x :: xs -> let new_state = make_move state x in 
    test_moves new_state xs

(** [make_win_test name start_state moves_list player col expected_output] 
    makes an OUnit test named [name] where the moves in [moves_list] are made
    starting with [start_state] to create a game_state that is used to 
    assert the quality of [expected_output] with 
    [check_win game_state player col] *)
let make_win_test
    (name: string)
    (start_state)
    (moves_list)
    (player)
    (col)
    (expected_output: bool) = 
  let game_state = test_moves start_state moves_list in 
  name >:: (fun _ ->
      assert_equal (check_win game_state player col) expected_output)

(** [make_draw_test name start_state moves_list expected_output] 
    makes an OUnit test named [name] where the moves in [moves_list] are made
    starting with [start_state] to create a game_state that is used to 
    assert the quality of [expected_output] with 
    [check_draw game_state player col] *)
let make_draw_test
    (name: string)
    (start_state)
    (moves_list)
    (expected_output: bool) = 
  let game_state = test_moves start_state moves_list in 
  name >:: (fun _ ->
      assert_equal (check_draw game_state) expected_output)

(** [get_piece_name piece_letter] gets the full name of the piece of type
    [piece_letter] *)
let get_piece_name piece_letter = 
  match piece_letter with 
  | "n" -> "normal"
  | "a" -> "anvil"
  | "w" -> "wall"
  | "b" -> "bomb"
  | "f" -> "force"
  | _ -> "normal"

(** [make_special_move current_state col piece_type] makes a move adding a 
    piece of type [piece_type] to [col] *)
let make_special_move current_state col piece_type=
  let player_turn = get_player_turn current_state in 
  let piece = create_piece (get_piece_name piece_type) player_turn in 
  let new_state = move current_state col piece in 
  match new_state with 
  | Valid moved -> moved
  | Invalid -> Format.fprintf Format.std_formatter "invalid\n"; current_state


(** [test_special_moves state moves] makes the moves in move_lst 
    using the (move,piece) tuples to place [piece] in the col move *)
let rec test_special_moves state moves = 
  match moves with 
  | [] -> state
  | x :: xs -> let new_state = make_special_move state (fst x) (snd x) in 
    test_special_moves new_state xs 

(** [make_special_win_test name start_state moves_list player col 
    expected_output] 
    makes an OUnit test named [name] where the (move,piece) tuple moves in 
    [moves_list] are made starting with [start_state] to create a game_state 
    that is used to assert the quality of [expected_output] with 
    [check_win game_state player col] *)
let make_special_win_test
    (name: string)
    (start_state)
    (moves_list)
    (player)
    (col)
    (expected_output: bool) = 
  let game_state = test_special_moves start_state moves_list in 
  name >:: (fun _ ->
      assert_equal (check_win game_state player col) expected_output)


(** used for printing, same as test_special_moves *)
let rec test_special_moves2 state moves = 
  match moves with 
  | [] -> state
  | x :: xs -> let new_state = make_special_move state (fst x) (snd x) in 
    print_board new_state; test_special_moves2 new_state xs 

(* for printing the gameboard for test, same as make_special_win_test *)
let make_special_win_test2
    (name: string)
    (start_state)
    (moves_list)
    (player)
    (col)
    (expected_output: bool) = 
  let game_state = test_special_moves2 start_state moves_list in 
  name >:: (fun _ ->
      assert_equal (check_win game_state player col) expected_output)


let start = start_game 6 7 2 4 [ANSITerminal.yellow; ANSITerminal.red] 1;;

let special_start = start_game 6 7 2 4 [ANSITerminal.yellow; ANSITerminal.red] 2;;

let small_special_start = start_game 5 5 3 3 [ANSITerminal.yellow; ANSITerminal.red] 2;;

let win_tests = [
  make_win_test "col win" start [4;4;3;5;5;2;2;4;4;2;6;5;5;7;7;2;6;1;3;2;3;4;3] 1 3 true;
  make_win_test "col2 win" start [3;4;3;7;3;2;3] 1 3 true;
  make_win_test "col3 win" start [4;4;3;5;2;5;3;5;4;6;4;1;2;7;6;7;1;6;4;5] 2 5 true;
  make_win_test "col4 win" start [4;4;3;6;1;2;4;5;7;4;4;7;6;4;5;5;5;6;3;7;2;6;3;1;3] 1 3 true;
  make_win_test "col5 win" start [4;4;5;4;7;4;1;4] 2 4 true;
  make_win_test "col6 win" start [1;2;1;2;1;2;1] 1 1 true;
  make_win_test "row win" start [2;2;3;3;4;4;5] 1 5 true;
  make_win_test "row2 win" start [1;7;2;6;3;5;4] 1 4 true;
  make_win_test "row3 win" start [4;4;5;5;3;3;6] 1 6 true;
  make_win_test "row4 win" start [3;4;3;5;4;6;5;3;6] 1 6 true;
  make_win_test "row5 win" start [4;3;5;4;6;3;7] 1 7 true;
  make_win_test "row6 win" start [4;4;3;5;7;5;2;3;1] 1 1 true;
  make_win_test "row7 win" start [4;4;3;5;1;3;2] 1 2 true;
  make_win_test "row8 win" start [4;4;5;3;6;7;6;2;2;1;2;1;4;4;1;3;4;1;1;1;3] 1 3 true;
  make_win_test "row9 win" start [4;4;3;6;7;7;5;2;3;5;1;6] 2 6 true;
  make_win_test "row10 win" start [4;4;7;5;5;3;6;5;4;4;7;6;2;3;1;4;6;2;6;1] 2 1 true;
  make_win_test "lr diag win" start [1;2;2;3;4;3;3;4;5;4;4] 1 4 true;
  make_win_test "lr2 diag win" start [4;5;5;6;7;6;6;7;1;7;7] 1 7 true;
  make_win_test "lr3 diag win" start [1;4;4;6;5;5;5;6;7;6;6;2;3] 1 3 true;
  make_win_test "lr4 diag win" start [4;5;6;7;4;5;5;6;7;6;6;7;1;7;7] 1 7 true;
  make_win_test "lr5 diag win" start [3;1;3;4;6;4;4;6;6;5;6;5;5;7;6;3;5] 1 5 true;
  make_win_test "lr6 diag win" start [3;4;5;6;4;5;3;4;3;3] 2 3 true;
  make_win_test "lr7 diag win" start [4;4;3;7;4;5;6;6;5;7;2;1;3;2;1;5;1;1;5] 1 5 true;
  make_win_test "lr8 diag win" start [4;4;3;5;2;1;4;3;5;2;1;4;3;5;2;1;3;3;2;2;4] 1 4 true;
  make_win_test "lr9 diag win" start [4;4;3;5;2;1;4;3;5;2;1;4;7;3] 2 3 true;
  make_win_test "rl diag win" start [1;1;1;1;2;4;2;2;3;3] 2 3 true;
  make_win_test "rl2 diag win" start [2;4;5;3;4;5;4;3;3;2;3;2;4;4;2] 1 2 true;
  make_win_test "rl3 diag win" start [3;4;7;6;5;5;5;7;6;3;3;3;3;4;7;4;4] 1 4 true;
  make_win_test "rl4 diag win" start [3;3;4;7;4;1;3;7;5;2;2;2;2] 1 2 true;
  make_win_test "rl5 diag win" start [6;5;4;3;5;7;6;4;5;4;4;3;3;3;3] 1 3 true;
  make_win_test "rl6 diag win" start [4;4;4;4;3;7;1;2;7;1;3;5;4;3;3;2;5;5;3;7;3;5;6] 1 6 true;
  make_win_test "rl7 diag win" start [4;4;3;1;5;4;4;6;1;6;1;2;5;6;4;4;6;6;1;2;3;5;2] 1 2 true;
  make_win_test "rl8 diag win" start [4;4;3;5;2;1;4;3;5;2;1;3;4;3;1;4;3;2;2;4;5;1;5;5;5;3;6;6;6;2;2] 1 2 true;
  make_win_test "rl9 diag win" start [4;4;3;5;2;1;4;3;5;2;1;3;4;3;3;7;3;6;5;6;1;7;6;2;2;2;2] 1 2 true;
  make_win_test "no win" start [3;4;5;6;1;2] 2 2 false;
  make_win_test "no2 win" start [3;2;1;3;4;52;23;5;53;4232;2] 2 2 false;
  make_win_test "no3 win" start [3;2;1;3;3;2;1;5;6;2;7;3;1;5;6] 1 6 false;
  make_win_test "no4 win" start [4;4;4;1;1;5;3;4;2;7;6;2] 2 2 false;
  make_win_test "no5 win" start [4;4;1;6;2;4;2;5;2;2;2;4;2;7;6;3] 2 3 false;
  make_win_test "no6 win" start [4;4;4;1;7;1;6;5;3;4;1;2;7;4;2;5] 2 5 false;
  make_win_test "no7 win" start [4;4;2;5;2;3;6;4;4;6;5;2;7] 1 7 false;
  make_win_test "no8 win" start [4;4;3;5;2;1;4;3;5;2;1;3;4;3;5;3;4;4;6;2;2;2;1;1;2;3;5;5;5;1;1;7;6;6;6;6;7;6;7;7;7;7] 2 7 false;
  make_win_test "no9 win" start [1;1;2;2;1;1;1;1;3;2] 1 2 false;
]

let draw_tests = [
  make_draw_test "no draw" start [1;1;2;2;1;1;1;1;3;2] false;
  make_draw_test "no draw2" start [4;4;3;5;2;1;4;3;5;2;1;3;4;3;1;4;3;2;2;4;5;1;5;5;5;3;6;6;6;2;2] false;
  make_draw_test "draw" start [4;4;3;5;2;1;4;3;5;2;1;3;4;3;5;3;4;4;6;2;2;2;1;1;2;3;5;5;5;1;1;7;6;6;6;6;7;6;7;7;7;7] true;
]

let special_win_tests = [
  make_special_win_test "no special pieces" start [(1,"n");(1,"n");(2,"n");(2,"n");(1,"n");(1,"n");(1,"n");(1,"n");(3,"n");(2,"n")] 1 2 false;
  make_special_win_test "anvil no win" special_start [(1,"n");(1,"n");(1,"a");(2,"n");(1,"n")] 1 1 false;
  make_special_win_test "wall no win" special_start [(1,"n");(1,"n");(1,"w");(2,"n");(1,"n")] 1 1 false;
  make_special_win_test "normal no win" small_special_start [(1,"n");(2,"n");(1,"n");(1,"n");(2,"n");(1,"n");(2,"n")] 1 2 false;
  make_special_win_test "anvil win" small_special_start [(1,"n");(2,"n");(1,"n");(1,"n");(2,"n");(1,"a");(2,"n");(3,"n");(1,"n");(3,"n");(3,"n");(1,"n")] 1 2 false;
  make_special_win_test "anvil p2 win" small_special_start [(1,"n");(2,"n");(1,"n");(1,"n");(2,"n");(1,"a");(2,"n");(3,"n");(1,"n");(1,"n");(4,"n")] 2 4 true; 
  make_special_win_test "wall p2 win" small_special_start [(1,"n");(1,"n");(1,"n");(2,"w");(1,"n");(2,"n");(3,"n");(4,"n");(3,"n")] 2 3 true;
  make_special_win_test "wall p1 win" small_special_start [(1,"n");(1,"n");(1,"w");(5,"n");(2,"n");(2,"n");(4,"n");(3,"n")] 1 3 true;
  (* make_special_win_test2 "wall p1 win" small_special_start [(1,"n");(1,"n");(1,"w");(5,"n");(2,"n");(2,"n");(4,"n");(3,"n")] 1 3 true; *)
  (* make_special_win_test "force no win" special_start [(1,"n");(1,"n");(1,"f");(2,"n");(1,"n")] 1 1 false; *)
]

let suite =
  "test suite for command"  >::: List.flatten [
    win_tests;
    draw_tests;
    special_win_tests;
  ]

let _ = run_test_tt_main suite


