open OUnit2
open Day9B

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
    "get right number min function test" >:: (fun _ -> assert_equal 3 (getRightNumber [30;45;3;44;77;14] 4 0 Stdlib.min));
    "get right number max function test" >:: (fun _ -> assert_equal 77 (getRightNumber [30;45;3;44;77;14] 4 0 Stdlib.max));
    "the get answer function test" >:: (fun _ -> assert_equal 80 (getAnswer [30;45;3;44;77;14]));
    "the wrong number is for test data" >:: (fun _ -> assert_equal 62 (getLongestSequence "test.data" 127));
    "the wrong number is for real data" >:: (fun _ -> assert_equal 183278487 (getLongestSequence "input.data" 1504371145));
]


let _ = run_test_tt_main tests