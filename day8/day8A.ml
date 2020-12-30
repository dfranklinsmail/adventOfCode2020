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

let rec processInstructions instructions index indexes =
    if (contains indexes index)
    then 
        0
    else begin
        let newIndexes = List.append indexes [index] in
        let instruction = (List.nth instructions index) in 
        (* Printf.printf "the new instruction is %s\n" instruction; *)
        (* may need to check for out of bounds *)
        let aOfInstruction = String.split_on_char ' ' instruction in
        let optCode = (List.nth aOfInstruction 0) in
        if  optCode = "nop" 
        then
            processInstructions instructions (index+1) newIndexes
        else 
            let value = (List.nth aOfInstruction 1) in
            if optCode = "jmp" 
            then
                processInstructions instructions (increment index value) newIndexes 
            else
                increment (processInstructions instructions (index+1) newIndexes) value
    end


let read_lines file = 
    let iChannel = open_in file in
    let try_read () =
        try Some (input_line iChannel) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s::acc)
        | None -> close_in iChannel; List.rev acc in
    loop []

let whenLoops filename = 
    let instructions = read_lines filename in
    let answer = processInstructions instructions 0 [] in
    (* Printf.printf "the answer is %d\n" answer; *)answer
        