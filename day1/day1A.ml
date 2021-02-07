open Printf
open Int32
open List

let rec searchForPair number numbers index =
    if length numbers = 0 || length numbers < index +1 then
        -1
    else 
        let currentNumber = nth numbers index in
        if currentNumber + number != 2020 then
            searchForPair number numbers (index+1)
        else begin 
            printf "result: %d\n" (currentNumber * number);
            currentNumber
        end

let file = "input.data"

let read_lines = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> 
                let aNumber = to_int (of_string s) in
                    let _ = searchForPair aNumber acc 0 in
                       loop (aNumber::acc)
        | None -> close_in iChannel; [] in
    loop []