(* lib/server.ml *)
open Common

let store_table = Hashtbl.create 100

let rec inflate s fetch =
  let pattern = Str.regexp "{{\\([^}]+\\)}}" in
  let rec find_matches pos acc =
    try
      let _ = Str.search_forward pattern s pos in
      let identifier = Str.matched_group 1 s in
      let acc' = match fetch identifier with
          Some value -> ("{{" ^ identifier ^ "}}", inflate value fetch) :: acc
        | None -> acc in
      find_matches (Str.match_end()) acc'
    with Not_found ->
      List.rev acc
  in
  let values = find_matches 0 [] in
  let rec replace_all s values =
    match values with
      (pat, value) :: values' ->
       replace_all (Str.global_replace (Str.regexp pat) value s) values'
    | [] -> s
  in replace_all s values

module Store = struct
  let init l = List.iter (fun (k, v) -> Hashtbl.replace store_table k v) l
  let get key =
    let raw = Hashtbl.find_opt store_table key in
    match raw with
      Some raw_value -> Some (inflate raw_value (Hashtbl.find_opt store_table))
    | None -> None
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
