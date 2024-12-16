(* lib/client.ml *)
open Common

exception Server_error of string

let connect ?(host = Unix.inet_addr_loopback) ?(port = default_port) () =
  let sockaddr = Unix.ADDR_INET (host, port) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket sockaddr;
  socket

let send_request socket request =
  let ic = Unix.in_channel_of_descr socket in
  let oc = Unix.out_channel_of_descr socket in
  output_value oc request;
  flush oc;
  let response = input_value ic in
  Unix.close socket;
  (* Close the socket after we're done *)
  match response with
  | Value v -> Result.ok v
  | KeyList keys -> Result.ok (String.concat "\n" keys)
  | Ok -> Result.ok "OK"
  | NotFound -> Result.error "Key not found"
  | Error msg -> Result.error msg

let get_value ?host ?(port = default_port) key =
  let socket = connect ?host ~port () in
  send_request socket (Get key)

let interpolate ?host ?(port = default_port) phrase =
  let socket = connect ?host ~port () in
  send_request socket (Expand phrase)

let set_value ?host ?(port = default_port) key value =
  let socket = connect ?host ~port () in
  send_request socket (Set (key, value))

let list_keys ?host ?(port = default_port) () =
  let socket = connect ?host ~port () in
  send_request socket List
