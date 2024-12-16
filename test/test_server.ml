open OUnit2

let test_expansion _ =
  let fetch s =
    match s with
    | "SIMPLE" -> Some "a"
    | "COMPLEX" -> Some "{{PART}} and {{PARCEL}}"
    | "PART" -> Some "b"
    | "PARCEL" -> Some "c"
    | _ -> None
  in
  assert_equal "expand a" (Kv.Server.inflate "expand {{SIMPLE}}" fetch);
  assert_equal "expand b and c" (Kv.Server.inflate "expand {{COMPLEX}}" fetch);
  assert_equal "expand {{not-found}}"
    (Kv.Server.inflate "expand {{not-found}}" fetch)

let test_to_interpolation _ =
  assert_equal (Kv.Server.to_braces "~expand ~vars") "{{expand}} {{vars}}"

let test_infinite_expansion _ =
  let fetch s =
    match s with
    | "A" -> Some "{{B}}"
    | "B" -> Some "expanded {{A}}"
    | _ -> None
  in
  assert_equal "expanded {{A}}" (Kv.Server.inflate "{{A}}" fetch)

let test_all_matches _ =
  let re = Str.regexp "{{\\([^}]+\\)}}" in
  assert_equal (Kv.Server.all_matches re "{{a}} {{b}} {{c}}") [ "a"; "b"; "c" ];
  assert_equal
    (Kv.Server.all_matches re "foo {{db}} bar {{url}}")
    [ "db"; "url" ]

let expansion_tests =
  "expansion_tests"
  >::: [
         "simple" >:: test_expansion;
         "expansion" >:: test_to_interpolation;
         "no-infinite" >:: test_infinite_expansion;
         "all-matches" >:: test_all_matches;
       ]

let () = run_test_tt_main expansion_tests
