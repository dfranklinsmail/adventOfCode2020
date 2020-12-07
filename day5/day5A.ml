open Printf
open String
open Float

let file = "input.data"

let rec calculate s index low high =
    if (index + 1) = length s 
    then let c = (get s index) in
        if c = 'F' || c = 'L'
        then low
        else high
    else let c = (get s index) in
        if c = 'F' || c = 'L'
        then calculate s (index+1) low (floor ((low+.high)/.2.0))
        else calculate s (index+1) (ceil ((low+.high)/.2.0)) high


let calculateID s =
    (* printf "the ticket string is %s\n" s;
    printf "the row is is %f\n" (calculate (String.sub s 0 7) 0 0.0 127.0);
    printf "the col is is %f\n" (calculate (String.sub s 7 3) 0 0.0 7.0);
    printf "the id is is %f\n" ((calculate (String.sub s 0 7) 0 0.0 127.0) *. 8.0 +. (calculate (String.sub s 7 3) 0 0.0 7.0));
    *)
    (calculate (String.sub s 0 7) 0 0.0 127.0) *. 8.0 +. (calculate (String.sub s 7 3) 0 0.0 7.0)

let numberOfTrees = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop highest = match try_read () with
        | Some s -> 
                let id = calculateID s in
                if id > highest
                then loop id
                else loop highest
        | None -> close_in iChannel; highest in
    loop 0.0

let () = printf "The highest id is: %f\n" numberOfTrees