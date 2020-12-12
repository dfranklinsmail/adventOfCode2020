open Printf
open List
open String

let file = "input.data"

let groups = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop currentPassport acc = 
        match try_read () with
        | Some s -> 
                if (String.length s) = 0 then
                    loop "" (currentPassport::acc)
                else
                    loop (currentPassport^" "^s) acc
        | None -> close_in iChannel; List.rev (currentPassport::acc) in
    loop "" []

let () = printf "Number of groups is: %d\n" (List.length groups)

let rec contains aList aValue =
    match aList with
    | [] -> false
    | h::t -> if h = aValue then
                true
            else contains t aValue

let rec computeUniquenessSize aString index acc = 
    if String.length aString = index then
        List.length acc
    else let x = String.get aString index in 
     if x = ' ' || contains acc x then
        computeUniquenessSize aString (index+1) acc
    else
        computeUniquenessSize aString (index+1) (x::acc)

let rec computeTotalUniqueAnswers groups = 
    match groups with
    | [] -> 0
    | h::t -> computeTotalUniqueAnswers t + (computeUniquenessSize h 0 [])



let () = printf "total questions answered is: %d\n" (computeTotalUniqueAnswers groups)