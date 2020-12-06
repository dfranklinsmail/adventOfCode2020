open Printf
open Int32
open List
open String

let rec isValidPassword min max char password index count = 
    if (String.length password) = index then
    begin
        count >= min && count <= max
    end
    else
        if password.[index] = char then
            isValidPassword min max char password (index+1) (count+1)
        else 
            isValidPassword min max char password (index+1) count

let isValid s =
    let arguments = split_on_char ' ' s in
        let minMax = split_on_char '-' (nth arguments 0) in
            let searchChar = get (nth arguments 1) 0 in
                isValidPassword (to_int (of_string (nth minMax 0))) (to_int (of_string (nth minMax 1))) searchChar (nth arguments 2) 0 0



let file = "input.data"

let vps = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop validPasswords = match try_read () with
        | Some s -> 
                if isValid s = true
                    then
                        loop (s::validPasswords)
                    else loop validPasswords
        | None -> close_in iChannel; validPasswords in
    loop []

let () = printf "Number of valid passwords is: %d\n" (List.length vps)
