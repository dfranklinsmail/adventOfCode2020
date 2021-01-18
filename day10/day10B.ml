let rec findOptionalNumbers numbers length index acc = 
    if index + 1 = length 
    then
        acc
    else if (List.nth numbers (index - 1)) - (List.nth numbers (index + 1)) <= 3
    then
        let found = (List.nth numbers index) in
        findOptionalNumbers numbers length (index+1) (List.append acc [found])
    else 
        findOptionalNumbers numbers length (index+1) acc

let rec contains aList aValue =
    match aList with
    | [] -> false
    | h::t -> if h = aValue then
                true
            else contains t aValue

let rec calculateWays numbers length index sum =
    if index = length 
    then
        sum
    else let current = (List.nth numbers index) in
        if (contains numbers (current+1)) && (contains numbers (current+2))
        then
            calculateWays numbers length (index+1) (sum+. 3.0 *.sum/.4.0)
        else
            calculateWays numbers length (index+1) (sum+.sum)

let calculateArrangments numbers acc =
    let compareInts a b = b - a in
    let sortedNumbers = List.sort compareInts numbers in
    let optionals = findOptionalNumbers sortedNumbers (List.length sortedNumbers) 1 acc in
    (* printList sortedNumbers; *)
    let answer = calculateWays optionals (List.length optionals) 0 1.0 in
        Printf.printf "the answer is %f\n" answer; answer