
let rec equalsSeatingArea seatingArea anotherSeatingArea =
match (seatingArea, anotherSeatingArea) with
    | [], [] -> Printf.printf "hit the end\n";true
    | [], _ -> false
    | _, [] -> false
    | (h::t), (hh::tt) -> Printf.printf "comparing %s with %s\n" h hh;
        if String.equal h hh 
        then
            equalsSeatingArea t tt
        else false

let rec countUnocupiedSeatsInRow row rowLength index =
    if index = rowLength 
    then 0
    else if (String.get row index) = '#'
    then 1 + countUnocupiedSeatsInRow row rowLength (index+1)
    else countUnocupiedSeatsInRow row rowLength (index+1)

let rec countUnocupiedSeats seatingArea =
    match seatingArea with
    | [] -> 0
    | h::t -> countUnocupiedSeats t + (countUnocupiedSeatsInRow h (String.length h) 0)

let getNeiboursHelperHelper row rowLength colIndex = 
    if (colIndex < rowLength) && (colIndex >= 0)
    then
       [(String.get row colIndex)]
    else
      []

let getNeighboursHelper seatingArea rowIndex colIndex includeMe = 
    let numRows = List.length seatingArea in
    if rowIndex < numRows && rowIndex >= 0 then
        let row = List.nth seatingArea rowIndex in
        let rowLen = String.length row in
            if includeMe 
            then
                List.append (getNeiboursHelperHelper row rowLen (colIndex - 1)) (List.append (getNeiboursHelperHelper row rowLen colIndex) (getNeiboursHelperHelper row rowLen (colIndex + 1)))
            else
                List.append (getNeiboursHelperHelper row rowLen (colIndex - 1)) (getNeiboursHelperHelper row rowLen (colIndex + 1))
    else 
        []

(* Find the 8 neighbouring seats next to the given location *)
let getNeighbours seatingArea rowIndex colIndex =
   List.append (List.append (getNeighboursHelper seatingArea (rowIndex-1) colIndex true) (getNeighboursHelper seatingArea rowIndex colIndex false)) (getNeighboursHelper seatingArea (rowIndex+1) colIndex true)

let rec count charList listLength char index =      
    if listLength <= index
    then 0
    else
        let currentChar = (List.nth charList index) in
        if currentChar = char
        then 1 + (count charList listLength char (index+1))
        else count charList listLength char (index+1)

let noOccupiedAdjacent neighbours = 
    let countOfOccupied = (count neighbours (List.length neighbours) '#' 0) in
    let answer = countOfOccupied = 0 in
        answer

let toManyAdjacentOccupied neighbours = 
    (count neighbours (List.length neighbours) '#' 0) >= 4


let rec createNewRow oldRow seatingArea rowLength rowIndex colIndex newRow =
    if rowLength = colIndex
    then newRow
    else 
        let seat = (String.get oldRow colIndex) in
        let neighbours = (getNeighbours seatingArea rowIndex colIndex) in
        if seat = 'L' && (noOccupiedAdjacent neighbours) then
            createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ "#")
        else if seat = '#' && (toManyAdjacentOccupied neighbours) then
           createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ "L")
        else
            createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ (String.make 1 seat))

let rec applyRules remaingRowsToApplyRules originalSeatingArea index =
    match remaingRowsToApplyRules with
    | [] -> []
    | h::t -> (createNewRow h originalSeatingArea (String.length h) index 0 "")::(applyRules t originalSeatingArea (index+1))


class seatstate (seatingArea : string list) =
    object (self)
        method move =
            (* logic to simulate people arriving and leaving *)
            new seatstate (applyRules seatingArea seatingArea 0)

        method getUnocupiedSeatsCount =
            (* logic for calculating how many empty seats there are in the seating area *)
            countUnocupiedSeats seatingArea

        method getSeatingArea = seatingArea

        method equals otherSeatingArea =
            (* determines if the giving seating state is equal to my seating area *)
            equalsSeatingArea seatingArea otherSeatingArea
       
    end

let rec simulateSeating seatState count =
    Printf.printf "\nsimulating seating area with size %d\n" (List.length seatState#getSeatingArea);
    let newSeatState = seatState#move in
    if seatState#equals newSeatState#getSeatingArea
    then
        newSeatState
    else 
        simulateSeating newSeatState (count+1)

let findUnocupiedSeats input = 
    let seatState = new seatstate input in
    let steadyState = simulateSeating seatState 0 in
    let answer = steadyState#getUnocupiedSeatsCount in
        Printf.printf "the answer is %d\n" answer; answer