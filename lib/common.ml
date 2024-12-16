(* common.ml - Shared types and constants *)
let default_port = 6060

type request =
  | Get of string
  | Set of string * string
  | Expand of string
  | List
  | Reload
  | Quit

type response =
  | Value of string
  | KeyList of string list
  | Ok
  | NotFound
  | Error of string
