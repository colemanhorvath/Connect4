(** Type [save_result] represents the result of attempting a save command/
    [Save_Success s] indicates the file was successfully saved to the file 
    named [s]
    [Save-Failure s] indicates the file was not able to be saved with input
    string [s]*)
type save_result = 
  | Save_Success of string
  | Save_Failure of string

(** Type [load_result] represents the result of attempting a load command/
    [Load_Success state] indicates the game was successfully loaded to a game
    with state [state]
    [Load-Failure s] indicates the file was not able to be loaded from input
    string [s]*)
type load_result = 
  | Load_Success of Game_mechanics.t
  | Load_Failure of string 

(** [save st filename] is the result of attempting to save a game in state [st]
    to a file with the name [filename].*)
val save: Game_mechanics.t -> string -> save_result

(** [load filename] is the result of attempting to load a game from a file
    named [filename]*)
val load: string -> load_result