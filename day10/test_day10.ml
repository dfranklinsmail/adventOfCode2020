open OUnit2
open Day10A
open Day10B


let read_lines filename = 
    let iChannel = open_in filename in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> 
                let aNumber = Int64.to_int (Int64.of_string s) in
                       loop (aNumber::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let sizeOfOptionalNumbers numbers =
    let compareInts a b = b -a in
    let sortedNumbers = List.sort compareInts numbers in
    let size = (List.length (findOptionalNumbers sortedNumbers (List.length sortedNumbers) 1 [])) in
        Printf.printf "the size is %d\n" size;
        size

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
     "get jolt differences for test " >:: (fun _ -> assert_equal 220 (processNumbers (read_lines "test.data")));
    "get jolt differences for real " >:: (fun _ -> assert_equal 1625 (processNumbers (read_lines "input.data"))); 
    "get optional numbers for test">:: (fun _ -> assert_equal 3 (sizeOfOptionalNumbers  [16;10;15;5;1;11;7;19;6;12;4]));
    "get adapter arrangments for test " >:: (fun _ -> assert_equal 8.0 (calculateArrangments [16;10;15;5;1;11;7;19;6;12;4] []));
   
   
   "get adapter arrangments for real " >:: (fun _ -> assert_equal 3100448333024.0 (calculateArrangments (read_lines "input.data") [1])); 
    "get adapter arrangments for test " >:: (fun _ -> assert_equal 19208.0 (calculateArrangments (read_lines "test.data") [1]));
]

let _ = run_test_tt_main tests