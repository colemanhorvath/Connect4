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

let start = start_game 6 7 2;;


(** [make_move current_state col] makes a move adding a piece to col *)
let make_move current_state col =
  let player_turn = get_player_turn current_state in 
  let piece = create_piece "normal" player_turn in 
  let new_state = move current_state col piece in 
  match new_state with 
  | Valid moved -> moved
  | Invalid -> Format.fprintf Format.std_formatter "invalid\n"; current_state

(** [print_board state] prints the gameboard in state *)
let print_board state = 
  Format.fprintf Format.std_formatter "%s\n" 
    (pp_list_list (get_gameboard state) "")

(** [test_moves state move_lst] makes the moves in move_lst and 
    prints the gameboard *)
let rec test_moves state move_lst = 
  match move_lst with 
  | [] -> state
  | x :: xs -> let new_state = make_move state x in 
    print_board new_state; test_moves new_state xs


let end_state1 = test_moves start [1;1;2;2;1;1;1;1;3;2]
let win = check_win end_state1 1 2
let () = assert (not win)
let () = Format.fprintf Format.std_formatter "%B\n\n" win


let col_win = test_moves start [1;2;1;2;1;2;1]
let win = check_win col_win 1 1
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let col_win = test_moves start [3;4;3;7;3;2;3]
let win = check_win col_win 1 3
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win


let row_win = test_moves start [2;2;3;3;4;4;5]
let win = check_win row_win 1 5
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let row_win = test_moves start [1;7;2;6;3;5;4]
let win = check_win row_win 1 4
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let row_win = test_moves start [4;4;5;5;3;3;6]
let win = check_win row_win 1 6
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win


let lr_diag_win = test_moves start [1;2;2;3;4;3;3;4;5;4;4]
let win = check_win lr_diag_win 1 4
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let lr_diag_win = test_moves start [4;5;5;6;7;6;6;7;1;7;7]
let win = check_win lr_diag_win 1 7
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let lr_diag_win = test_moves start [4;5;6;7;4;5;5;6;7;6;6;7;1;7;7]
let win = check_win lr_diag_win 1 7
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let lr_diag_win = test_moves start [1;4;4;6;5;5;5;6;7;6;6;2;3]
let win = check_win lr_diag_win 1 3
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let lr_diag_win = test_moves start [3;1;3;4;6;4;4;6;6;5;6;5;5;7;6;3;5]
let win = check_win lr_diag_win 1 5
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win


let rl_diag_win = test_moves start [1;1;1;1;2;4;2;2;3;3]
let win = check_win rl_diag_win 2 3
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let rl_diag_win = test_moves start [3;4;7;6;5;5;5;7;6;3;3;3;3;4;7;4;4]
let win = check_win rl_diag_win 1 4
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let rl_diag_win = test_moves start [3;3;4;7;4;1;3;7;5;2;2;2;2]
let win = check_win rl_diag_win 1 2
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let rl_diag_win = test_moves start [6;5;4;3;5;7;6;4;5;4;4;3;3;3;3]
let win = check_win rl_diag_win 1 3
let () = assert win
let () = Format.fprintf Format.std_formatter "%B\n\n" win


let no_win = test_moves start [3;4;5;6;1;2]
let win = check_win no_win 2 2
let () = assert (not win)
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let no_win = test_moves start [3;2;1;3;4;52;23;5;53;4232;]
let win = check_win no_win 2 2
let () = assert (not win)
let () = Format.fprintf Format.std_formatter "%B\n\n" win

let no_win = test_moves start [3;2;1;3;3;2;1;5;6;2;7;3;1;5;6]
let win = check_win no_win 1 6
let () = assert (not win)
let () = Format.fprintf Format.std_formatter "%B\n\n" win

(*
let new_move = make_move new_move 1

let () = print_board new_move
*)

(*
  let piece1 = create_piece "normal" 1;;

  let move1 = move start 1 piece1;;

  let () = 
  match move1 with 
  | Valid moved -> Format.fprintf Format.std_formatter "%s\n" (pp_list_list (get_gameboard moved) "")
  | Invalid -> Format.fprintf Format.std_formatter "Error\n"

  let piece2 = create_piece "normal" 2;;

  let move2 = 
  match move1 with 
  | Valid moved -> move moved 1 piece2
  | Invalid -> move1


  let () = 
  match move2 with 
  | Valid move -> Format.fprintf Format.std_formatter "%s\n" (pp_list_list (get_gameboard move) "")
  | Invalid -> Format.fprintf Format.std_formatter "Error\n"

*)
