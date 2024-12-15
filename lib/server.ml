(* lib/server.ml *)
open Common

let store_table = Hashtbl.create 100

(* Walk through strings looking for {{identifier}}, `fetch`s it,
   inflates it, and carries on. *)
let rec inflate s fetch =
  let pattern = Str.regexp "{{\\([^}]+\\)}}" in
  let rec replace index s =
    try
      let _ = Str.search_forward pattern s index in
      let identifier = Str.matched_group 1 s in
      let (index', s') = match fetch identifier with
          Some value ->
           let usage = Str.regexp ("{{" ^ identifier ^ "}}") in
           (index, Str.global_replace usage (inflate value fetch) s)
        (* if there's no replacement, just advance and leave it as a literal *)
        | None -> (Str.match_end (), s)
      in replace index' s'
    with Not_found -> s
  in replace 0 s

let to_braces s =
  let pattern = (Str.regexp "~\\([a-zA-Z][a-zA-Z0-9_-]*\\)") in
  let rec replace index =
    try
      let _ = Str.search_forward pattern s index in
      let var = Str.matched_group 1 s in
      let this = String.sub s index (Str.match_beginning() - index) in
      let after = replace (Str.match_end()) in
      this ^ "{{" ^ var ^ "}}" ^ after
    with Not_found ->
      String.sub s index (String.length s - index)
  in replace 0

module Store = struct
  let init l = List.iter (fun (k, v) -> Hashtbl.replace store_table k v) l
  let get key =
    let raw = Hashtbl.find_opt store_table key in
    match raw with
      Some raw_value -> Some (inflate raw_value (Hashtbl.find_opt store_table))
    | None -> None
  let interpolate s = inflate (to_braces s) (Hashtbl.find_opt store_table)
  let set key value = Hashtbl.replace store_table key value
  let list () = Hashtbl.fold (fun k _ acc -> k :: acc) store_table []
end

(* add a default arg here for an initial sequence of key value pairs
   from a file and add them with Hashtbl.replace_seq *)
let start_server ?(port=default_port) initial_values =
  let addr = Unix.inet_addr_loopback in
  let sockaddr = Unix.ADDR_INET(addr, port) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket sockaddr;
  Unix.listen socket 5;
  Printf.printf "Server listening on port %d\n%!" port;

  Store.init initial_values;
  while true do
    let (client_sock, _) = Unix.accept socket in
    let ic = Unix.in_channel_of_descr client_sock in
    let oc = Unix.out_channel_of_descr client_sock in

    try
      let request = input_value ic in
      let response = match request with
        | Get key ->
            (match Store.get key with
            | Some value -> Value value
            | None -> NotFound)
        | Expand phrase -> Value (Store.interpolate phrase)
        | Set (key, value) ->
            Store.set key value;
            Printf.printf "Stored: %s = %s\n%!" key value;
            Ok
        | List ->
            KeyList (Store.list ())
        | Quit ->
            Ok
      in
      output_value oc response;
      flush oc;
    with e ->
      Printf.printf "Error handling request: %s\n%!" (Printexc.to_string e);

    Unix.close client_sock
  done
