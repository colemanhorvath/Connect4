type save_result = 
  | Save_Success of string
  | Save_Failure of string

type load_result = 
  | Load_Success of Game_mechanics.t
  | Load_Failure of string 


let save st filename = failwith "unimplemented"

let load filename = 
  let json = try Yojson.Basic.from_file filename with
    | _ -> Load_Failure filename in 
