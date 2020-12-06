open Printf
open String
open List

let file = "input.data"

let read_lines = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let () = printf "Size of list is: %d\n" (length read_lines)

let rec numberOfTrees list skipFlag index =
    match list with
    | [] -> 0
    | h::t -> 
        if skipFlag = true
        then
            if index <0 then
                0 + numberOfTrees t true (index+1)
            else
                0 + numberOfTrees t false index
        else let newIndex = (index+1)mod(String.length h) in
                if (h.[newIndex] = '#')
                then
                    1 + numberOfTrees t true newIndex
                else 
                    0 + numberOfTrees t true newIndex
            

let () = printf "Result is: %d\n" (numberOfTrees read_lines true (-1))