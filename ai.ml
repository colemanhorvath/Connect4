open Game_mechanics

(** [winning_move st player i] is Some column if there if by placing a piece
    in column [i] [player] would win. Otherwise it is None. If multiple 
    winning moves are possible, the leftmost is chosen*)
let rec winning_move st player i: int option =
  if i > snd (get_dimensions st) then None else 
    let move_state = move st i (Normal player) in 
    match move_state with
    | Valid new_state -> (if check_win new_state player i then Some i 
                          else winning_move st player (i+1))
    | Invalid -> winning_move st player (i+1)

(** [find_next_valid_move st i mid] returns [i] if the column is not full
    otherwise it returns the column from [st] closest to [mid] but no 
    closer than [i] that is not full*)
let rec find_next_valid_move st i mid = 
  let move_attempt = move st i (Normal 2) in 
  match move_attempt with 
  | Valid new_state -> i
  | Invalid -> 
    let offset = 2*(Int.abs (mid-i)) in 
    let new_col = if i >= mid then i-(1 + offset) else i+offset in 
    find_next_valid_move st new_col mid

(** [check_3s st] is the next column in which the AI will place their piece
    given that neither the AI nor player can win the next turn but is close to
    connecting 3 pieces. *)
let check_3s st = 
  let connect_3_st = change_connect_num st 3 in 
  let player_3_in_a_row = winning_move connect_3_st 1 1 in 
  let cols = snd(get_dimensions st) in
  match player_3_in_a_row with 
  | Some col -> col
  | None -> find_next_valid_move st ((cols + 1) / 2) ((cols + 1) / 2)

let next_move st =
  let ai_win = winning_move st 2 1 in 
  match ai_win with
  | Some col -> col
  | None -> 
    begin 
      let player_win = winning_move st 1 1 in 
      match player_win with 
      | Some col -> col
      | None -> check_3s st
    end