open OUnit2
open Day9A

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
    "the wrong number is for test data" >:: (fun _ -> assert_equal 127 (findWrongNumber "test.data" 5));
    "the wrong number is for real data" >:: (fun _ -> assert_equal 1504371145 (findWrongNumber "input.data" 25));
]


let _ = run_test_tt_main tests