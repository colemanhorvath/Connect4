(** The type of the game pieces. *)
type piece

(** The type representing the board for the game.
    The board is represented like so 
    a board with 5 rows and 7 cols would have a list with 7 entries,
    each one representing a different column
    [
    [col1]
    [col2]
    [col3]
    [col4]
    [col5]
    [col6]
    [col7]
    ]
    Note: index 0 represents column 1 of the gameboard, index 1 col 2, etc.
    each new piece is added to the beginning of the column list
    so the piece in the first row is at the highest index
    and the topmost piece in a column is the first element of the col list
*)
type board = piece list list

(** The type of the game state. *)
type t 

(** The type representing the result of an attempted move. *)
type move_result =  Valid of t | Invalid

(** [start_game rows cols players] is the initial state of the game
    with a [rows] x [cols] board and [players] number of players. *)
val start_game: int -> int -> int -> t

(** [create_piece piece_type player] creates an instance of type piece 
    with piece_type player *)
val create_piece: string -> int -> piece

(** [move state col piece] is [Valid new_state] if the placing [piece] in
    [col] is a valid move, where new_state represents the updated state of
    the game with the new move, and [Invalid] otherwise  *)
val move: t -> int -> piece -> move_result

(** [get_gameboard state] is the gameboard in [state] *)
val get_gameboard: t -> board

(** [get_player_turn state] is the player who can make the next move *)
val get_player_turn: t -> int

(** [format p] is a printing function to format the printing of pieces
    for debugging purposes *)
val format: piece -> string

(** [check_win state player col] is true if state has a win and 
    false otherwise *)
val check_win : t -> int -> int -> bool

(** [check_draw state ] is true if the game board is full and nobody has won and 
    false otherwise *)
val check_draw : t -> bool