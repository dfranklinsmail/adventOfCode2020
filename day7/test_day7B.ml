open OUnit2
open Day7B
open List
open Printf

(*TEST SUITE*)
let tests = "test suite for day 7 part B from Advent of Code 2020 " >::: [
    "how many bags in test" >:: (fun _ -> assert_equal 126 (howManyBags "testB.data" ["shiny";"gold"])); 
    "how many bags in test" >:: (fun _ -> assert_equal 3805 (howManyBags "input.data" ["shiny";"gold"]));
]


let _ = run_test_tt_main tests