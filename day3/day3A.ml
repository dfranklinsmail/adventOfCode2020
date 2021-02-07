open Printf
open List
open String

let file = "input.data"
let flag = true

let numberOfTrees = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc index = match try_read () with
        | Some s -> 
                let newIndex = (index+3)mod(length s) in
                if s.[newIndex] = '#'
                then 
                    loop (acc+1) newIndex
                else 
                    loop acc newIndex
           
        | None -> close_in iChannel; acc in
    loop 0 (-3)

let () = printf "Number of trees is: %d\n" numberOfTrees