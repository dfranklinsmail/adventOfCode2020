open Printf
open Int32
open List

let rec searchForPair number numbers index =
    if length numbers = 0 || length numbers < index +1 then
        -1
    else 
        let currentNumber = nth numbers index in
        if currentNumber + number != 2020 then
            searchForPair number numbers (index+1)
        else begin 
            printf "result: %d\n" (currentNumber * number);
            currentNumber
        end

let file = "input.data"
