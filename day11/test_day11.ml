open OUnit2
open Day11A


let read_lines filename = 
    let iChannel = open_in filename in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
    "get nearest neibours" >:: (fun _ -> assert_equal 0 (List.length (getNeighbours (read_lines "test.data") 100 0)));
    "get nearest neibours" >:: (fun _ -> assert_equal 3 (List.length (getNeighbours (read_lines "test.data") 0 0)));
    "get nearest neibours" >:: (fun _ -> assert_equal 8 (List.length (getNeighbours (read_lines "test.data") 2 3)));
    (* "get unocupied seat count" >:: (fun _ -> assert_equal 71 ((new seatstate (read_lines "test.data"))#getUnocupiedSeatsCount));
    "get unocupied seat count" >:: (fun _ -> assert_equal true ((new seatstate (read_lines "test.data"))#equals ((new seatstate (read_lines "test.data"))#getSeatingArea)));
    "get unocupied seat count" >:: (fun _ -> assert_equal 0 ((new seatstate (read_lines "test.data"))#move#getUnocupiedSeatsCount));
    "get unocupied seat count" >:: (fun _ -> assert_equal 71 (findUnocupiedSeats (read_lines "test.data"))); *)
]

let _ = run_test_tt_main tests