
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

(* decides if the series of numbers when summed is equal to another number *)
let rec isContiguous numbers aNum =
    match numbers with
    | [] -> []
    | h::t -> 
            let newNum = aNum - h in
                if newNum = 0 
                    then
                        [h]
                    else if newNum > 0
                        then
                        let series = isContiguous t newNum in
                            if (List.length series > 0)
                            then
                                h::series
                            else 
                                []
                    else
                       []

let rec getRightNumber numbers lastIndex index f=
    if index=lastIndex 
    then 
        f (List.nth numbers index) (List.nth numbers (index+1))
    else 
        f (List.nth numbers index) (getRightNumber numbers lastIndex (index+1) f)

(* returns the sum of the smallest and largest number in the given list *)
let getAnswer numbers = 
    let lastIndex = ((List.length numbers)-2) in
    let lowest = getRightNumber numbers lastIndex 0 Stdlib.min in
    let highest = getRightNumber numbers lastIndex 0 Stdlib.max in
        lowest + highest
                     
let rec processList numbers aNum =
    match numbers with
    | [] -> -1
    | h::t -> let contiguous = isContiguous t aNum in
                if (List.length contiguous) > 0 
                then
                    getAnswer contiguous
                else
                    processList t aNum


let getLongestSequence filename aNum = 
    let numbers = read_lines filename in
    let answer = processList numbers aNum in
        Printf.printf "the answer is %d\n" answer; answer