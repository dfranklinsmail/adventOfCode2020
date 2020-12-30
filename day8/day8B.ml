

let rec contains ints aInt =
match ints with 
| [] -> false
| h::t -> if h = aInt 
    then true
    else contains t aInt

let increment number incrementor =
    let operator = (String.get incrementor 0) in
    let value = (String.sub incrementor 1 ((String.length incrementor)-1)) in
    (* Printf.printf "the new value is %s\n" value; *)
    let intValue = Int32.to_int (Int32.of_string value) in
        if operator = '-'
        then number - intValue
        else number + intValue

let incrementBox box incrementor =
    (* Printf.printf "--> "; *)
    match box with
    | [] -> (* Printf.printf "die\n"; *)[]
    | h::t -> [increment h incrementor]

let join numberBoxA numberBoxB =
    if (List.length numberBoxA) > 0
    then
        numberBoxA
    else
        numberBoxB

let rec computedValue instructions maxIndex index indexes switched =
    if index >= maxIndex 
    then [0]
    else if (contains indexes index)
    then 
        []
    else begin
        let newIndexes = List.append indexes [index] in
        let instruction = (List.nth instructions index) in 
        (* Printf.printf "the new instruction is %s\n" instruction; *)
        (* may need to check for out of bounds *)
        let aOfInstruction = String.split_on_char ' ' instruction in
        let optCode = (List.nth aOfInstruction 0) in
        let value = (List.nth aOfInstruction 1) in
        if  optCode = "nop" 
        then
            if switched 
            then 
                computedValue instructions maxIndex (index+1) newIndexes switched
            else
                join (*nop*)(computedValue instructions maxIndex (index+1) newIndexes switched) (*nmp*)(computedValue instructions maxIndex (increment index value) newIndexes true)
        else 
            if optCode = "jmp" 
            then
                if switched 
                then
                    computedValue instructions maxIndex (increment index value) newIndexes switched
                else
                    join (*nop*)(computedValue instructions maxIndex (index+1) newIndexes true) (*nmp*)(computedValue instructions maxIndex (increment index value) newIndexes switched)
            else
                incrementBox (computedValue instructions maxIndex (index+1) newIndexes switched) value
    end


let read_lines file = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let getAnswer box =
if List.length box > 0 
then List.nth box 0 
else -12345

let compute filename =
    let instructions = read_lines filename in
    let answer = getAnswer (computedValue instructions (List.length instructions) 0 [] false) in
    Printf.printf "the answer is %d\n" answer; answer
       
       
