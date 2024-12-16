(* lib/server.ml *)
open Common
module StringSet = Set.Make (String)

let store_table = Hashtbl.create 100

(* Walk through strings looking for {{identifier}}, `fetch`s it,
   inflates it, and carries on. *)

let all_matches re s =
  let rec iter s index acc =
    try
      let _ = Str.search_forward re s index in
      let identifier = Str.matched_group 1 s in
      iter s (Str.match_end ()) (identifier :: acc)
    with Not_found -> List.rev acc
  in
  iter s 0 []

type replacement = Replace of string * string | Noop of string

let replace_all s replacements =
  let rec iter s rs =
    match rs with
    | Replace (id, rep) :: rs ->
        Str.global_replace (Str.regexp ("{{" ^ id ^ "}}")) rep (iter s rs)
    | Noop _id :: rs -> iter s rs
    | [] -> s
  in
  iter s replacements

let inflate s fetch =
  let rec inflate_with_seen seen s =
    s
    |> all_matches (Str.regexp "{{\\([^}]+\\)}}") (* Get all template vars *)
    |> List.filter (fun id -> not (StringSet.mem id seen))
    (* Remove seen *)
    |> function
    | [] -> s
    | ids ->
        let replacements =
          ids
          |> List.map (fun id ->
                 fetch id
                 |> Option.map (fun template ->
                        let seen' = StringSet.add id seen in
                        Replace (id, inflate_with_seen seen' template))
                 |> Option.value ~default:(Noop id))
        in
        replace_all s replacements
  in
  inflate_with_seen StringSet.empty s

let to_braces s =
  let pattern = Str.regexp "~\\([a-zA-Z][a-zA-Z0-9_-]*\\)" in
  let rec replace index =
    try
      let _ = Str.search_forward pattern s index in
      let var = Str.matched_group 1 s in
      let this = String.sub s index (Str.match_beginning () - index) in
      let after = replace (Str.match_end ()) in
      this ^ "{{" ^ var ^ "}}" ^ after
    with Not_found -> String.sub s index (String.length s - index)
  in
  replace 0

module Store = struct
  let init l = List.iter (fun (k, v) -> Hashtbl.replace store_table k v) l

  let get key =
    let raw = Hashtbl.find_opt store_table key in
    match raw with
    | Some raw_value -> Some (inflate raw_value (Hashtbl.find_opt store_table))
    | None -> None

  let interpolate s = inflate (to_braces s) (Hashtbl.find_opt store_table)
  let set key value = Hashtbl.replace store_table key value
  let list () = Hashtbl.fold (fun k _ acc -> k :: acc) store_table []
end

(* add a default arg here for an initial sequence of key value pairs
   from a file and add them with Hashtbl.replace_seq *)
let start_server ?(port = default_port) initial_values =
  let addr = Unix.inet_addr_loopback in
  let sockaddr = Unix.ADDR_INET (addr, port) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket sockaddr;
  Unix.listen socket 5;
  Printf.printf "Server listening on port %d\n%!" port;

  Store.init initial_values;
  while true do
    let client_sock, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr client_sock in
    let oc = Unix.out_channel_of_descr client_sock in

    try
      let request = input_value ic in
      let response =
        match request with
        | Get key -> (
            match Store.get key with
            | Some value -> Value value
            | None -> NotFound)
        | Expand phrase -> Value (Store.interpolate phrase)
        | Set (key, value) ->
            Store.set key value;
            Printf.printf "Stored: %s = %s\n%!" key value;
            Ok
        | List -> KeyList (Store.list ())
        | Quit -> Ok
      in
      output_value oc response;
      flush oc
    with e ->
      Printf.printf "Error handling request: %s\n%!" (Printexc.to_string e);

      Unix.close client_sock
  done
