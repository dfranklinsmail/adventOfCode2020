open Printf
open String
open Float
open List

let file = "input.data"

let rec calculate s index low high =
    let c = (get s index) in
        if c = 'F' || c = 'L'
        then if (index + 1) = String.length s
            then low
            else calculate s (index+1) low (floor ((low+.high)/.2.0))
        else if (index + 1) = String.length s
            then high
            else calculate s (index+1) (ceil ((low+.high)/.2.0)) high


let calculateID s =
    (calculate (String.sub s 0 7) 0 0.0 127.0) *. 8.0 +. (calculate (String.sub s 7 3) 0 0.0 7.0)

let ids = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> 
                loop ((calculateID s)::acc)
        | None -> close_in iChannel; acc in
    loop []

let () = printf "The ids size is: %d\n" (List.length ids)

let rec getSeat ids last =
    match ids with 
    | [] -> 0.0
    | s::t -> if (last +. 2.0) = s 
        then (last +. 1.0)
        else getSeat t s

let () = printf "my seat id is: %f\n" (getSeat (List.sort Float.compare ids) 0.0)