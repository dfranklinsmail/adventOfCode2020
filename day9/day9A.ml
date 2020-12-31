
let read_lines filename = 
    let iChannel = open_in filename in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> 
                let aNumber = Int64.to_int (Int64.of_string s) in
                       loop (aNumber::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let rec findNumber numbers first last aNumber =
(* Printf.printf "finding number %d\n" aNumber; *)
if first > last 
then
    false
else
    if (List.nth numbers first) = aNumber 
    then
        true
    else 
        findNumber numbers (first+1) last aNumber

let rec contains numbers first last aNumber =
    if first = last 
    then
        false
    else
        let firstNum = List.nth numbers first in
        if (findNumber numbers (first+1) last (aNumber - firstNum)) 
        then
            true
        else 
            contains numbers (first+1) last aNumber


let rec processList numbers sizeOfNumbers bufferSize index =  
    (* Printf.printf "the size of the numbers is %d the index is %d\n" sizeOfNumbers index; *)
    if  index >= sizeOfNumbers
    then
        -12345
    else
        let next = (List.nth numbers index) in
            if (contains numbers (index-bufferSize) (index-1) next)
            then
                processList numbers sizeOfNumbers bufferSize (index+1)
            else
                next

let findWrongNumber filename bufferSize = 
    let numbers = read_lines filename in
    let answer = processList numbers (List.length numbers) bufferSize bufferSize in
        Printf.printf "the answer is %d\n" answer; answer