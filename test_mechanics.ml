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

let rec pp_list_list lst acc = 
  match lst with 
  | [] -> "[ " ^ acc ^ " ]"
  | x :: xs -> pp_list_list xs (acc ^ (pp_list x))

let start = start_game 5 7 2;;



let make_move current_state col =
  let player_turn = get_player_turn current_state in 
  let piece = create_piece "normal" player_turn in 
  let new_state = move current_state col piece in 
  match new_state with 
  | Valid moved -> moved
  | Invalid -> Format.fprintf Format.std_formatter "invalid\n"; current_state

let print_board state = 
  Format.fprintf Format.std_formatter "%s\n" 
    (pp_list_list (get_gameboard state) "")


let rec test_moves state move_lst = 
  match move_lst with 
  | [] -> state
  | x :: xs -> let new_state = make_move state x in 
    print_board new_state; test_moves new_state xs


let end_state = test_moves start [1;1;2;2;1;1;1;1;3;2]
let () = Format.fprintf Format.std_formatter "\n"


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
