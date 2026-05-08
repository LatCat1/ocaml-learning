open OUnit2
open Exercises3

(* Product tests *)
let product_tests = "test suite for product" >::: [
    "empty" >:: (fun _ -> assert_equal 1 (product []));
    "singleton" >:: (fun _ -> assert_equal 2 (product [2]));
    "two_elements" >:: (fun _ -> assert_equal 12 (product [3; 4]));
]

let _ = run_test_tt_main product_tests

(* Fifth tests *)

let fifth_tests = "test suite for fifth" >::: [
    "too_short" >:: (fun _ -> assert_equal 0 (fifth []));
    "five_elements" >:: (fun _ -> assert_equal 5 (fifth [1 ; 2 ; 3 ; 4 ; 5]));
]

let _ = run_test_tt_main fifth_tests

(* reversesorted tests *)

let reversesorted_tests = "test suite for reversesorted" >::: [
    "empty" >:: (fun _ -> assert_equal [] (reversesorted []));
    "five_elements" >:: (fun _ -> assert_equal [5 ; 4 ; 3 ; 2 ; 1] (reversesorted [3 ; 2 ; 1 ; 4 ; 5]));
]

let _ = run_test_tt_main reversesorted_tests

(* list_max tests *)

let list_max_tests = "test suite for list_max" >::: [
    "empty" >:: (fun _ -> assert_raises (Failure "empty") (fun _ -> list_max []));
    "finds_max" >:: (fun _ -> assert_equal 3 (list_max [1; 3; 2]));
]

let _ = run_test_tt_main list_max_tests
