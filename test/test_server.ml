open OUnit2
(* open Base *)

(* module Server = Kv.Server *)

let test_expansion _ =
  let fetch s = match s with
      "SIMPLE" -> Some "a"
    | "COMPLEX" -> Some "{{PART}} and {{PARCEL}}"
    | "PART" -> Some "b"
    | "PARCEL" -> Some "c"
    | _ -> None
  in
  assert_equal (Kv.Server.inflate "expand {{SIMPLE}}" fetch) "expand a";
  assert_equal (Kv.Server.inflate "expand {{COMPLEX}}" fetch) "expand b and c";
  assert_equal (Kv.Server.inflate "expand {{not-found}}" fetch) "expand {{not-found}}"


let expansion_tests = "expansion_tests" >::: [
  "simple" >:: test_expansion;
    ]

let () = run_test_tt_main expansion_tests
