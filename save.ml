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
  if piece_type = "Normal" then (Game_mechanics.Normal player) else
  if piece_type = "Bomb" then (Game_mechanics.Bomb player) else
    Game_mechanics.None

(** [map_cols json] is the list of pieces in [json] which encodes one column
    of a gameboard]*)
let map_cols json = 
  json |> to_list |> List.map to_piece 

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
  let total_moves = json |> member "total_moves"
                    |> to_string |> int_of_string in 
  Game_mechanics.load_game num_players rows cols board turn total_moves

let load filename = 
  let json = try Yojson.Basic.from_file filename with
    | _ -> Yojson.Basic.(`String "not a valid json") in
  match json with 
  | (Yojson.Basic.(`String s)) when s = "not a valid json" -> 
    Load_Failure filename
  | _ -> (try Load_Success (from_json json) with 
      | _ -> Load_Failure filename)
