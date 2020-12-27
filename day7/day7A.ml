open List
open String

let getBagName aRule = 
    let firstBlankIndex = String.index aRule ' ' in
        let secondBlankIndex = String.index_from aRule (firstBlankIndex+1) ' ' in
            let bagName = String.sub aRule 0 secondBlankIndex in
                bagName

let rec containsBag ruleLength rules index styleOfBag colourOfBag = 
   if index < ruleLength then
        if ((List.nth rules index = styleOfBag) && (List.nth rules (index+1) = colourOfBag)) then 
            true
        else 
            containsBag ruleLength rules (index+4) styleOfBag colourOfBag
    else 
        false

(* returns a list of size 2 with the style and colour of the parent bag or an empty list*)
let getParentBagName aRule bagNames = 
    let rules = String.split_on_char ' ' aRule in 
        let ruleLength = (List.length rules) in
        let styleOfBag = (List.nth bagNames 0) in
        let  colourOfBag = (List.nth bagNames 1) in 
            if (containsBag ruleLength rules 5 styleOfBag colourOfBag) 
            then
                [(List.nth rules 0); (List.nth rules 1)]
            else []

let equals bagNameA bagNameB = 
    (List.nth bagNameA 0) = (List.nth bagNameB 0) && (List.nth bagNameA 1) = (List.nth bagNameB 1)

let rec containsBagName bagNames aBagName =
    match bagNames with
    | [] -> false
    | h::t ->
        if (equals aBagName h) then
            true
        else
            containsBagName t aBagName

let rec findMatchingBags rules bagName = 
    match rules with
    | [] -> []
    | h::t -> let parentBagName = (getParentBagName h bagName) in
                let restOfTheNames = (findMatchingBags t bagName) in
                    if List.length parentBagName > 0 && List.length restOfTheNames > 0 then
                        List.append restOfTheNames [parentBagName]
                    else if List.length parentBagName > 0 then 
                        [parentBagName]
                    else if List.length restOfTheNames > 0  then
                        restOfTheNames
                    else []


(* let file = "input.data" *)
let read_lines file = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []


let rec containsBagName bagName otherBagNames =
    match otherBagNames with
    | [] -> false
    | h::t ->
            if equals h bagName then
                true
            else containsBagName bagName t

let rec addNames originalListOfNames newListOfNames = 
    match newListOfNames with 
    | [] -> originalListOfNames
    | h::t -> if (containsBagName h originalListOfNames)
                then
                    addNames originalListOfNames t  
                else
                    addNames (List.append originalListOfNames [h]) t

let rec findAllMatchingBags rules bagNames acc =    
        match bagNames with
        | [] -> acc
        | h::t ->  if  (List.length h = 0) || (containsBagName h acc) 
                    then
                        findAllMatchingBags rules t acc
                    else let newMatchingBagNames = (findMatchingBags rules h) in
                        if (List.length newMatchingBagNames >0) 
                        then
                            findAllMatchingBags rules (addNames t newMatchingBagNames) (List.append acc [h])
                        else 
                            findAllMatchingBags rules t (List.append acc [h])