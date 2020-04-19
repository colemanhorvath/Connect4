(** Type [object_phrase] is a list of the words to specify the objects
    of the user's input command. *)
type object_phrase = string list

(** Type [command] represents the command the user has supplied as input.*)
type command = 
  | Print
  | Place of object_phrase
  | Save of object_phrase
  | Load of object_phrase

(** Exception [Empty] is raised if a command is empty*)
exception Empty

(** Exception [Malformed] is raised if a command does not follow proper syntax*)
exception Malformed

(** [parse input] is the command that is specified by the user's input string
    [input]. 
    Raises [Empty] if [input] is empty or contains no non-filler characters
    Raises [Malformed] if [input] is not a valid command.*)
val parse: string -> command