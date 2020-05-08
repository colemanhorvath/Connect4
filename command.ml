type object_phrase = string list

type command = 
  | Print
  | Start
  | SinglePlayer
  | ToggleAI
  | Settings
  | Help
  | Hand
  | Quit
  | Exit
  | Place of object_phrase
  | Save of object_phrase
  | Load of object_phrase

exception Empty
exception Malformed

(** [parse_helper inputs obj_phrase] creates a list of strings in obj_phrase
    without the spaces *)
let rec parse_helper inputs obj_phrase =
  match inputs with 
  | [] -> List.rev obj_phrase
  | h::t -> if h = "" then parse_helper t obj_phrase else
      parse_helper t (h::obj_phrase)

let parse input = 
  let words = String.split_on_char ' ' 
      (String.lowercase_ascii (String.trim input)) in 
  match words with 
  | [] -> raise Empty
  | h::t ->
    match h,t with 
    | "", _ -> raise Empty 
    | "print", [] -> Print
    | "start", [] -> Start
    | "singleplayer", [] -> SinglePlayer
    | "toggle", [] -> ToggleAI
    | "settings", [] -> Settings
    | "help", [] -> Help
    | "hand", [] -> Hand
    | "quit", [] -> Quit
    | "exit", [] -> Exit
    | "place", x when x != [] -> Place (parse_helper x [])
    | "save", x when x != [] -> Save (parse_helper x [])
    | "load", x when x != [] -> Load (parse_helper x [])
    | _ -> raise Malformed
