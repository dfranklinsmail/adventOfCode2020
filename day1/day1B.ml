open Printf
open Int32
open List

let rec findSingle value list =
    match list with
    | [] -> []
    | h::t -> 
        if (value + h) = 2020
                then [h]
                else findSingle value t

let rec findDouble value list =
    match list  with
    | [] -> []
    | h::t -> 
            printf "trying: %d and %d with total %d\n" value h (value+h);
            let sum = value + h in
                if sum > 2020
                then findDouble value t 
                else 
                    let result = findSingle (sum) t in
                    match result with
                        | [] -> findDouble value t 
                        | [s] -> [value; h; s]

let rec findTriple list =
    match list with
    | [] -> []
    | h::t ->
        printf "In finding triple with %d\n " h;
        let result = findDouble h t in 
                match result with 
                    | [] -> findTriple t
                    | s -> s

let file = "input.data"

let read_lines = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop ((to_int (of_string s))::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let () = printf "Size of list is: %d\n" (length read_lines)

let rec multiplyAndSum listOfNumbers = 
    match listOfNumbers with
    | [] -> 1
    | h::t -> printf "the number is %d\n" h;h * multiplyAndSum t

let () = printf "Result is: %d\n" (multiplyAndSum [5;10;20])
let () = printf "Result is: %d\n" (multiplyAndSum (findTriple read_lines))