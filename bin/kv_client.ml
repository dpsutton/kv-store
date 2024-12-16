(* bin/client.ml - Client executable *)
let usage =
  "Usage: client [get <key> | set <key> <value> | expand <phrase> | list]"

let () =
  match Array.to_list Sys.argv with
  | [ _; "get"; key ] -> (
      match Kv.Client.get_value key with
      | Ok value -> print_endline value
      | Error msg ->
          prerr_endline msg;
          exit 1)
  | [ _; "expand"; phrase ] -> (
      match Kv.Client.interpolate phrase with
      | Ok value -> print_endline value
      | Error msg ->
          prerr_endline msg;
          exit 1)
  | [ _; "set"; key; value ] -> (
      match Kv.Client.set_value key value with
      | Ok _ -> exit 0
      | Error msg ->
          prerr_endline msg;
          exit 1)
  | [ _; "list" ] -> (
      match Kv.Client.list_keys () with
      | Ok keys -> print_endline keys
      | Error msg ->
          prerr_endline msg;
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 1
