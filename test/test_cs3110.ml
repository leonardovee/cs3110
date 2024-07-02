open OUnit2
open Abc.Maps

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let bindings_test name output input =
  name >:: fun _ ->
  assert_equal output (AssocListMap.bindings input) ~cmp:cmp_set_like_lists

let lst1 = [ (3110, "fun") ]
let lst2 = [ (3110, "fun"); (2110, "OO") ]

let assoc_tests =
  let open AssocListMap in
  [
    bindings_test "empty has no bindings" [] empty;
    bindings_test "singleton list has 1 binding" lst1 (of_list lst1);
    bindings_test "list with 2 bindings" lst2 (of_list lst2);
  ]

let suite = "maps suite" >::: assoc_tests
let _ = run_test_tt_main suite
