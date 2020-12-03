open Printf
open String
open Char
open Int32

let rec numberSum numAsString =
   
    if length numAsString = 0
    then 0
    else (code (get numAsString 0) - 48)
        + numberSum (String.sub numAsString 1 ((length numAsString ) -1))

let numberTest someNumber =  
    if numberSum someNumber  = 20
    then to_int (of_string someNumber) else 1



let file = "input.data"

let read_lines = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> printf "line: %d\n" (numberTest s);numberTest s * loop []
        | None -> close_in iChannel; 1 in
    loop []

let () = printf "Results: %d\n" (read_lines)