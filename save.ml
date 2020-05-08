open Yojson.Basic.Util
open Game_mechanics
open Printf

type save_result = 
  | Save_Success of string
  | Save_Failure of string

type load_result = 
  | Load_Success of Game_mechanics.t
  | Load_Failure of string 

let save st filename = 
  let json_string = Game_mechanics.to_json_string st in 
  try (let oc = open_out filename in 
       fprintf oc "%s" json_string;
       close_out oc;
       Save_Success filename) with
  | _ -> Save_Failure filename 

(** [to_piece json] is the game piece encoded in [json] *)
let to_piece json = 
  let piece_type = json |> member "type" |> to_string in 
  let player = json |> member "player" |> to_string |> int_of_string in 
  match piece_type with 
  | "Normal" -> Game_mechanics.Normal player
  | "Bomb" -> Game_mechanics.Bomb player
  | "Anvil" -> Game_mechanics.Anvil player 
  | "Force" -> Game_mechanics.Force player 
  | "Wall" -> Game_mechanics.Wall
  | _ -> Game_mechanics.None

(** [map_cols json] is the list of pieces in [json] which encodes one column
    of a gameboard]*)
let map_cols json = 
  json |> to_list |> List.map to_piece 

(** [to_style s] is the ANSITerminal.style represented by [s]*)
let to_style s = 
  match s with 
  | "Red" -> ANSITerminal.red
  | "Green" -> ANSITerminal.green
  | "Yellow" -> ANSITerminal.yellow
  | "Blue" -> ANSITerminal.blue
  | "Magenta" -> ANSITerminal.magenta
  | "Cyan" -> ANSITerminal.cyan
  | _ -> failwith "Illegal Color"

(** [map_colors json] is the list of colors of the players as encoded in 
    [json]*)
let map_colors json = 
  json |> to_string |> to_style

(** [map_sps json] is the int list representation of the player's special
    pieces encoded in [json]*)
let map_sps json =
  json |> to_list |> List.map to_string |> List.map int_of_string

(** [from_json json] is the game state as is encoded in [json].*)
let from_json json = 
  let num_players = json |> member "num_players" 
                    |> to_string |> int_of_string in 
  let rows = json |> member "rows" 
             |> to_string |> int_of_string in 
  let cols = json |> member "cols" 
             |> to_string |> int_of_string in 
  let board = json |> member "gameboard" 
              |> to_list |> List.map map_cols in 
  let turn = json |> member "player_turn"
             |> to_string |> int_of_string in 
  let connect_num = json |> member "connect_num" 
                    |> to_string |> int_of_string in 
  let colors = json |> member "colors" 
               |> to_list |> List.map map_colors in 
  let mode = json |> member "game_mode"
             |> to_string |> int_of_string in 
  let sps = json |> member "special_pieces"
            |> to_list |> List.map map_sps in 
  let force = json |> member "is_player_forced"
              |> to_string |> bool_of_string in 
  let bomb = json |>member "is_awaiting_bomb"
             |> to_string |> bool_of_string in
  let ai = json |> member "ai_active"
           |> to_string |> bool_of_string in  
  Game_mechanics.load_game num_players rows cols board turn connect_num colors
    mode sps force bomb ai

let load filename = 
  let json = try Yojson.Basic.from_file filename with
    | _ -> Yojson.Basic.(`String "not a valid json") in
  match json with 
  | (Yojson.Basic.(`String s)) when s = "not a valid json" -> 
    Load_Failure filename
  | _ -> (try Load_Success (from_json json) with 
      | _ -> Load_Failure filename)
