let usage = "Usage: kv-server [-port PORT] [-file INITIAL_VALUES_FILE]"

let port = ref Kv.Common.default_port
let init_file = ref None

let speclist = [
  ("-port", Arg.Int (fun p -> port := p), "Port number (default: 6060)");
  ("-file", Arg.String (fun f -> init_file := Some f), "File containing initial key-value pairs");
  ]

let parse_values f =
  if not (Sys.file_exists f) then
    (Printf.printf "Warning: File %s does not exist\n" f; [])
  else
    let is_comment = String.starts_with ~prefix:"#" in
    (* blows up if = is at the end *)
    let split s = match String.index_opt s '=' with
        Some index -> [String.sub s 0 index;
                       String.sub s (index + 1) (String.length s - (index + 1))]
      | None -> []
    in
    let get_kvs acc line =
      match (is_comment line, split line) with
      | (false, [key; value]) -> (String.trim key, String.trim value) :: acc
      | _ -> acc
    in
    let pairs = In_channel.with_open_text f (In_channel.fold_lines get_kvs []) in
    List.rev pairs

let () =
  Arg.parse speclist (fun _ -> ()) usage;

  let initial_values = match !init_file with
    | Some f -> parse_values f
    | None -> []
  in

  Kv.Server.start_server ~port:!port initial_values
