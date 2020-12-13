open Printf
open List
open String

let file = "input.data"

let groups = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop currentPassportGroup acc = 
        match try_read () with
        | Some s -> 
                if (String.length s) = 0 then
                    loop [] (currentPassportGroup::acc)
                else
                    loop (s::currentPassportGroup) acc
        | None -> close_in iChannel; currentPassportGroup::acc in
    loop [] []

let () = printf "Number of groups is: %d\n" (List.length groups)


let rec computeCommonSet aString common index newCommon = 
    if String.length aString = index then
        newCommon
    else let x = String.get aString index in 
        if x = ' ' || not (String.contains common x)  then
            computeCommonSet aString common (index+1) newCommon
        else
            computeCommonSet aString common (index+1) ((String.make 1 x)^newCommon)

let rec computeCommonAnswers aList commonAnswers =
    match aList with
    | [] -> commonAnswers
    | h::t -> computeCommonAnswers t (computeCommonSet h commonAnswers 0 "")

let computeCommonAnswersSize groupAnswers =
    match groupAnswers with
    | [] -> 0
    | h::t -> String.length (computeCommonAnswers t h)

let rec computeTotalCommonAnswers groups = 
    match groups with
    | [] -> 0
    | h::t -> computeTotalCommonAnswers t + computeCommonAnswersSize h 



let () = printf "total questions answered is: %d\n" (computeTotalCommonAnswers groups)