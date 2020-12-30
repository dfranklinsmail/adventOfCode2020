open OUnit2
open Day8A

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
    "accumulator at loop test data" >:: (fun _ -> assert_equal 5 (whenLoops "testA.data")); 
     "accumulator at loop test data" >:: (fun _ -> assert_equal 2003 (whenLoops "input.data")); 
]


let _ = run_test_tt_main tests