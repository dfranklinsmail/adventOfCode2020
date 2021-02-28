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

let rec printNeighbours n = 
    match n with
    | [] -> Printf.printf "\n"
    | h::t -> Printf.printf "%c" h; printNeighbours t

let testNeighbours x y file =    
    let n = (getNeighbours (read_lines file) x y) in
        printNeighbours n;
    let answer = List.length n in
        Printf.printf "the number of neighbours is %d\n" answer;
        answer

let testMove x = 
    let seatCountAfterMove = ((new seatstate (read_lines "test.data"))#move#move#getUnocupiedSeatsCount) in
        Printf.printf "the seat count after move is %d\n" seatCountAfterMove;
        seatCountAfterMove

(*TEST SUITE*)
let tests = "test suite for day 8 part A from Advent of Code 2020 " >::: [
  (*  
    "get seating area test" >:: (fun _ -> assert_equal true ((new seatstate (read_lines "test.data"))#equals ((new seatstate (read_lines "test.data"))#getSeatingArea)));
    "test equlas seating area, compare with empty" >:: (fun _ -> assert_equal false (equalsSeatingArea (read_lines "test.data") []));
    "get unocupied seat count test" >:: (fun _ -> assert_equal 71 ((new seatstate (read_lines "test.data"))#getUnocupiedSeatsCount));
    "test equlas seating area, compare with empty reverse " >:: (fun _ -> assert_equal false (equalsSeatingArea [] (read_lines "test.data")));
    "test equlas seating area, compare the same seating area" >:: (fun _ -> assert_equal true (equalsSeatingArea (read_lines "test.data") (read_lines "test.data")));
    "test equlas seating area, compare the same seating area" >:: (fun _ -> assert_equal false (equalsSeatingArea (read_lines "test.data") ((new seatstate (read_lines "test.data"))#move#getSeatingArea)));
    "get unocupied seat count after move test" >:: (fun _ -> assert_equal 28 (testMove 1));
    "get nearest neibours invalid column" >:: (fun _ -> assert_equal 0 (testNeighbours 5 100 "test.data"));
    "get nearest neibours invalid row" >:: (fun _ -> assert_equal 0 (testNeighbours 100 0 "test.data"));
    "get nearest neibours top left" >:: (fun _ -> assert_equal 3 (testNeighbours 0 0 "test.data"));
    "get nearest neibours top middle" >:: (fun _ -> assert_equal 5 (testNeighbours 0 5 "test.data"));
    "get nearest neibours top right" >:: (fun _ -> assert_equal 3 (testNeighbours 0 9 "test.data"));
    "get nearest neibours middle left" >:: (fun _ -> assert_equal 5 (testNeighbours 3 0 "test.data"));
    "get nearest neibours middle right" >:: (fun _ -> assert_equal 5 (testNeighbours 3 9 "test.data"));
    "get nearest neibours middle middle" >:: (fun _ -> assert_equal 8 (testNeighbours 3 3 "test.data"));
    "get nearest neibours bottom left" >:: (fun _ -> assert_equal 3 (testNeighbours 9 0 "test.data"));
    "get nearest neibours bottom right" >:: (fun _ -> assert_equal 3 (testNeighbours 9 9 "test.data"));
   
    "no ocupied adjacent test, yes" >:: (fun _ -> assert_equal false (noOccupiedAdjacent ['#'; '#'; '#'; '#'; '#'; '#'; '#'])); 
    "no ocupied adjacent test, yes just one" >:: (fun _ -> assert_equal false (noOccupiedAdjacent ['#'; 'L'; 'L'; 'L'; 'L'; 'L'; 'L'])); 
    "no ocupied adjacent test, no" >:: (fun _ -> assert_equal true (noOccupiedAdjacent ['L'; 'L'; 'L'; 'L'; 'L'; 'L'; 'L'])); 
 
    "is adjacent seats ocupied test, yes" >:: (fun _ -> assert_equal true (toManyAdjacentOccupied ['#'; '#'; '#'; '#'; '#'; '#'; '#'])); 
    "is adjacent seats ocupied test, yes just enough" >:: (fun _ -> assert_equal true (toManyAdjacentOccupied ['#'; '#'; '#'; '#'; 'L'; 'L'; 'L'])); 
    "is adjacent seats ocupied test, no not enough" >:: (fun _ -> assert_equal false (toManyAdjacentOccupied ['L'; 'L'; 'L'; 'L'; '#'; '#'; '#'])); 
    "is adjacent seats ocupied test, no" >:: (fun _ -> assert_equal false (toManyAdjacentOccupied ['L'; 'L'; 'L'; 'L'; 'L'; 'L'; 'L'])); 
    
    "get nearest neibours top middle" >:: (fun _ -> assert_equal 5 (testNeighbours 9 5 "test2.data"));
    "get unocupied seat count" >:: (fun _ -> assert_equal 37 (findUnocupiedSeats (read_lines "test.data"))); 
   *)
    "get unocupied seat count THE REAL DEAL" >:: (fun _ -> assert_equal 909 (findUnocupiedSeats (read_lines "input.data"))); 
]
let _ = run_test_tt_main tests