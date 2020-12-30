let rec findRule rules style colour = 
    match rules with
    | [] -> []
    | h::t -> let splitRule = (String.split_on_char ' ' h) in
                if ((List.nth splitRule 0) = style) && ((List.nth splitRule 1) = colour)
                then 
                    splitRule
                else 
                    findRule t style colour


let rec processRule rules rule length index sum =
    if index < length then
        let multiplier = (Int32.to_int (Int32.of_string (List.nth rule index))) in 
        let style = (List.nth rule (index+1)) in
        let colour = (List.nth rule (index+2)) in
        let newSum = sum * multiplier in
            newSum + (calculateHowManyBags rules [style;colour] newSum) + (processRule rules rule length (index+4) sum)
    else
        0

and calculateHowManyBags rules bagName sum =
    let styleOfBag = (List.nth bagName 0) in
    let colourOfBag = (List.nth bagName 1) in
    let rule = (findRule rules styleOfBag colourOfBag) in
    let length = (List.length rule) in
        if rule = [] || length < 8
        then
            0
        else
            processRule rules rule length 4 sum

(* let file = "input.data" *)
let read_lines file = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let howManyBags filename bagName = 
    let rules = read_lines filename in
    let answer =  (calculateHowManyBags rules bagName 1) in
        Printf.printf "the answer is %d\n" answer;
        answer


