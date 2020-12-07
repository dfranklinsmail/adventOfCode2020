open Printf
open List
open String
open Int32
open Char

let file = "input.data"
let flag = true

let passports = 
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

let () = printf "Number of passports is: %d\n" (List.length passports)

(* four digits; at least min and at most max *)
let validRange range min max =
    let rangeNumber = (to_int (of_string range)) in
        if rangeNumber >= min && rangeNumber <= max 
        then true
        else false
(*
a number followed by either cm or in:

    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
*)
let validHeight height = 
    let units = String.sub height (String.length height-2) 2 in
        if units = "cm" then
            let quantity = String.sub height 0 (String.length height-2) in
                if (validRange quantity 150 193) = true
                then true
                else false 
        else if units = "in" then
             let quantity = String.sub height 0 (String.length height-2) in
                if (validRange quantity 59 76) = true
                then true
                else false 
        else false

(* exactly one of: amb blu brn gry grn hzl oth *)
let validEyeColour hairColour = 
    if hairColour = "amb" || hairColour = "blu" || hairColour = "brn" 
       || hairColour = "gry" || hairColour = "grn" || hairColour = "hzl" || hairColour = "oth"
    then true
    else false 

let rec hasValidCharacters chars index = 
    if String.length chars = index then true
    else 
        let code = (Char.code (String.get chars index)) in
            if (code >= 97 && code <= 122)
                || (code >= 48 && code <= 57)
            then (hasValidCharacters chars (index+1))
            else false

(* a # followed by exactly six characters 0-9 or a-f *)
let validHairColour eyeColour =
    if (String.length eyeColour = 7)
    then if (String.sub eyeColour 0 1) = "#" 
        then (hasValidCharacters (String.sub eyeColour 1 6) 0)
        else false
    else false

(* can be converted to an int*)
let isValidNumber id = 
    try int_of_string id |> ignore; 
        true with Failure _ -> false

(* a nine-digit number, including leading zeroes *)
let validID id =
    if (String.length id = 9) && (isValidNumber id)
    then true
    else false


let rec countRequiredFields fields =
    match fields with
    | [] -> 0
    | h::t -> 
        let pair = String.split_on_char ':' h in
            if ((nth pair 0) = "byr" && (validRange (nth pair 1) 1920 2002)) 
                || ((nth pair 0) = "iyr" && (validRange (nth pair 1) 2010 2020 )) 
                || ((nth pair 0) = "eyr" && (validRange (nth pair 1) 2020 2030)) 
                || ((nth pair 0) = "hgt" && (validHeight (nth pair 1))) 
                || ((nth pair 0) = "hcl" && (validHairColour (nth pair 1)))  
                || ((nth pair 0) = "ecl" && (validEyeColour (nth pair 1)))  
                || ((nth pair 0) = "pid" && (validID (nth pair 1))) 
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