(** [print_board state] pretty prints the connect 4 board based on [state]. *)
val print_board: Game_mechanics.t -> unit

(** [print_start_turn state] prints the text that appears at the state of a 
    turn of the game, mainly the board and current player of [state]. *)
val print_start_turn: Game_mechanics.t -> unit

(** [print_help mode] prints all of the commands that can be used after a game 
    has been started for the given mode. *)
val print_help: int -> unit

(** [print_hand player] prints all of the special pieces that player currently
    has. *)
val print_hand: Game_mechanics.t -> int -> unit

(** [pretty_print_string str] prints a newline before and after a string. *)
val pretty_print_string: string -> unit