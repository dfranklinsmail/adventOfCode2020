open OUnit2
open Day8B

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
    "compute instructions for test data" >:: (fun _ -> assert_equal 8 (compute "testA.data")); 
    "compute instructions for real data" >:: (fun _ -> assert_equal 1984 (compute "input.data"));
]


let _ = run_test_tt_main tests