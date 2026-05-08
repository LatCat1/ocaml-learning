open OUnit2
open Exercises4

(* add_row_vectors tests *)
let add_row_vectors_tests = "test suite for add_row_vectors" >::: [
    "empty" >:: (fun _ -> assert_equal [] (add_row_vectors [] [])) ;
    "nonempty" >:: (fun _ -> assert_equal [4 ; 6] (add_row_vectors [1 ; 2] [3 ; 4])) ;
]

let _ = run_test_tt_main add_row_vectors_tests 

(* add_matrices tests *)
let add_matrices_tests = "test suite for add_matrices" >::: [
    "empty" >:: (fun _ -> assert_equal [] 
        (add_matrices [] [])) ;
    "nonempty" >:: (fun _ -> assert_equal [ [ 6 ; 8] ; [ 10 ; 12 ] ]
        (add_matrices [ [1 ; 2] ; [3 ; 4]] [ [5 ; 6] ; [7 ; 8]])) ;
]

let _ = run_test_tt_main add_matrices_tests 

(* multiply_matrices tests *)
let multiply_matrices_tests = "test suite for multiply_matrices" >::: [
    "empty" >:: (fun _ -> assert_equal [] 
        (multiply_matrices [] [])) ;
    "nonempty" >:: (fun _ -> assert_equal [ [ 19 ; 22 ] ; [ 43 ; 50 ]]
        (multiply_matrices [ [1 ; 2] ; [3 ; 4]] [ [5 ; 6] ; [7 ; 8]])) ;
]

let _ = run_test_tt_main multiply_matrices_tests 
