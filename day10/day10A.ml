
let rec calculateJolts numbers last diff1 diff3 =
    match numbers with
    | [] -> diff1 * diff3 (* empty list, no more numbers return the sum of diff1 diff3 *)
    | h::t -> let diff = (h - last) in
        if (diff = 1)
        then
            calculateJolts t h (diff1+1) diff3
        else if (diff = 3 )
        then
            calculateJolts t h diff1 (diff3+1)
        else
            calculateJolts t h diff1 diff3

let compareInts a b =
    a-b

let processNumbers numbers =
    let sortedNumbers = List.sort compareInts numbers in
    let answer = calculateJolts sortedNumbers 0 0 1 in
        Printf.printf "the answer is %d\n" answer; answer