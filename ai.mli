(**
   This module provides the functionality of a computer second player for 
   standard connect four. 
*)

(** [next_move st] is the number of the column the AI will place its next 
    piece in given that the game is currently in [st]*)
val next_move: Game_mechanics.t -> int