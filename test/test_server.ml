open OUnit2
module StringSet = Set.Make(String)

let empty = StringSet.empty

let test_expansion _ =
  let fetch s = match s with
      "SIMPLE" -> Some "a"
    | "COMPLEX" -> Some "{{PART}} and {{PARCEL}}"
    | "PART" -> Some "b"
    | "PARCEL" -> Some "c"
    | _ -> None
  in
  assert_equal (Kv.Server.inflate "expand {{SIMPLE}}" fetch empty) "expand a";
  assert_equal (Kv.Server.inflate "expand {{COMPLEX}}" fetch empty) "expand b and c";
  assert_equal (Kv.Server.inflate "expand {{not-found}}" fetch empty) "expand {{not-found}}"

let test_to_interpolation _ =
  assert_equal (Kv.Server.to_braces "~expand ~vars") "{{expand}} {{vars}}"

let test_infinite_expansion _ =
  let fetch s = match s with
      "A" -> Some "{{B}}"
    | "B" -> Some "expanded {{A}}"
    | _ -> None
  in
  assert_equal (Kv.Server.inflate "{{A}}" fetch (StringSet.add "A" empty)) "{{A}}";
  assert_equal (Kv.Server.inflate "{{A}}" fetch empty) "expanded {{A}}"

let test_all_matches _ =
  let re = Str.regexp "{{\\([^}]+\\)}}" in
  assert_equal (Kv.Server.all_matches "{{a}} {{b}} {{c}}" re) ["a"; "b"; "c"];
  assert_equal (Kv.Server.all_matches "foo {{db}} bar {{url}}" re) ["db"; "url"]

let expansion_tests = "expansion_tests" >::: [
      "simple" >:: test_expansion;
      "expansion" >:: test_to_interpolation;
      "no-infinite" >:: test_infinite_expansion;
      "all-matches" >:: test_all_matches
    ]

let () = run_test_tt_main expansion_tests
