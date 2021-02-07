open Printf
open List
open String

let file = "input.data"
let flag = true

let passports = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop currentPassport acc = 
        match try_read () with
        | Some s -> 
                printf "The string is %s\n" s;
                if (String.length s) = 0 then
                    loop "" (currentPassport::acc)
                else
                    loop (currentPassport^" "^s) acc
        | None -> close_in iChannel; List.rev (currentPassport::acc) in
    loop "" []

let () = printf "Number of passports is: %d\n" (List.length passports)

let rec countRequiredFields fields =
    match fields with
    | [] -> 0
    | h::t -> 
        let pair = String.split_on_char ':' h in
            if (nth pair 0) = "byr" || (nth pair 0) = "iyr" || (nth pair 0) = "eyr" || (nth pair 0) = "hgt"
                || (nth pair 0) = "hcl" || (nth pair 0) = "ecl" || (nth pair 0) = "pid"
            then
                1+(countRequiredFields t)
            else 
                countRequiredFields t

let isValidPassport p = 
    let kvs = String.split_on_char ' ' p in
        if (countRequiredFields kvs = 7) then
            true
        else false
        

let rec getValidPassports passports = 
    match passports with
    | [] -> []
    | h::t -> 
        if (isValidPassport h = true) then
            h::(getValidPassports t)
        else 
            getValidPassports t

let () = printf "Number of passports is: %d\n" (List.length (getValidPassports passports))