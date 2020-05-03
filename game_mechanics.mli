(** The type of the game pieces. *)
type piece = 
  | None
  | Normal of int
  | Anvil of int
  | Wall
  | Bomb of int 
  | Force of int

(** The type of game status (win, draw, or still being played). *)
type status = 
  | Play
  | Draw
  | Win of int

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

(** [load_game players rows cols board turn moves] is the state of a game with
    a [rows] x [cols] board currently on the players turn indicated by [turn]
    where [moves] have been taken and they have resulted in the pieces in 
    [board]. *)
val load_game: int -> int -> int -> board -> int -> int -> int -> string list -> int -> t

(** [start_game rows cols players] is the initial state of the game
    with a [rows] x [cols] board and [players] number of players. *)
val start_game: int -> int -> int -> int -> string list -> int -> t

(** [create_piece piece_type player] creates an instance of type piece for
    [player] of type [piece_type] *)
val create_piece: string -> int -> piece

(** [move state col piece] is [Valid new_state] if the placing [piece] in
    [col] is a valid move, where new_state represents the updated state of
    the game with the new move, and [Invalid] otherwise  *)
val move: t -> int -> piece -> move_result

(** [bomb state row col] is [Valid new_state] without the piece at [row],
    [col], where new_state represents the updated state of the game without 
    that piece and the column moved down. If there is not a piece at [row], 
    [col], is [Invalid]. *)
val bomb: t -> int -> int -> move_result

(** [get_gameboard state] is the gameboard in [state] *)
val get_gameboard: t -> board

(** [get_player_turn state] is the player who can make the next move *)
val get_player_turn: t -> int

(** [format p] is a printing function to format the printing of pieces
    for debugging purposes *)
val format: piece -> string

(** [to_json_string st] is the json string representation of [st]. *)
val to_json_string: t -> string 

(** [check_win state player col] is true if [state] has a win from player 
    [player] from playing in column [col] and false otherwise *)
val check_win : t -> int -> int -> bool

(** [check_draw state] is true if the game board is full and nobody has won and 
    false otherwise *)
val check_draw : t -> bool

(** [check_status state player col] is a [status] representing if the game has 
    been won by [player] after placing a piece in column [col], if the game 
    has resulted in a draw, or if the game is still being played. *)
val check_status: t -> int -> int -> status