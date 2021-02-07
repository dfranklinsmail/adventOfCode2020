
let rec equalsSeatingArea seatingArea anotherSeatingArea =
             match (seatingArea, anotherSeatingArea) with
            | [], [] -> true
            | [], _ -> false
            | _, [] -> false
            | (h::t), (hh::tt) -> if String.equal h hh 
                                then
                                    equalsSeatingArea t tt
                                else false

let rec countUnocupiedSeatsInRow row length index =
    if index = length 
    then 0
    else if (String.get row index) = 'L'
    then 1 + countUnocupiedSeatsInRow row length (index+1)
    else countUnocupiedSeatsInRow row length (index+1)

let rec countUnocupiedSeats seatingArea =
    match seatingArea with
    | [] -> 0
    | h::t -> countUnocupiedSeats t + (countUnocupiedSeatsInRow h (String.length h) 0)


let getNeighboursHelper seatingArea rowIndex colIndex includeMe = 
    if rowIndex < (List.length seatingArea) then
        let row = List.nth seatingArea rowIndex in
            [(List.nth row colIndex)]
    else 
        []

(* Find the 8 neighbouring seats next to the given location *)
let getNeighbours seatingArea rowIndex colIndex =
    (getNeighboursHelper seatingArea (rowIndex-1) colIndex false)::(getNeighboursHelper seatingArea rowIndex colIndex false)::[(getNeighboursHelper seatingArea (rowIndex+1) colIndex false)]

let rec noOccupiedAdjacent seatingArea rowIndex colIndex = 
    let neighbours = getNeighbours seatingArea rowIndex colIndex in
        List.length neighbours > 1

let rec adjacentOccupiedSeatCount seatingArea rowIndex colIndex = 
    let neighbours = getNeighbours seatingArea rowIndex colIndex in
        List.length neighbours

let rec createNewRow oldRow seatingArea rowLength rowIndex colIndex newRow =
    if rowLength = colIndex
    then newRow
    else let seat = (String.get oldRow colIndex) in
        if seat = 'L' && (noOccupiedAdjacent seatingArea rowIndex colIndex) then
            createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ "#")
        else if seat = '#' && (adjacentOccupiedSeatCount seatingArea rowIndex colIndex) >= 4 then
            createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ "L")
        else
            createNewRow oldRow seatingArea rowLength rowIndex (colIndex+1) (newRow ^ (String.make 1 seat))

let rec applyRules seatingArea index =
    match seatingArea with
    | [] -> []
    | h::t -> (createNewRow h seatingArea (String.length h) index 0 "")::(applyRules t (index+1))


class seatstate (seatingArea : string list) =
    object (self)
        method move =
            (* logic to simulate people arriving and leaving *)
            new seatstate (applyRules seatingArea 0)

        method getUnocupiedSeatsCount =
            (* logic for calculating how many empty seats there are in the seating area *)
            countUnocupiedSeats seatingArea

        method getSeatingArea = seatingArea

        method equals seatingArea =
            (* determines if the giving seating state is equal to my seating area *)
            equalsSeatingArea seatingArea seatingArea
       
    end

let rec simulateSeating seatState =
    let newSeatState = seatState#move in
    if seatState#equals newSeatState#getSeatingArea
    then
        newSeatState
    else 
        simulateSeating newSeatState

let findUnocupiedSeats input = 
    let seatState = new seatstate input in
    let steadyState = simulateSeating seatState in
    let answer = steadyState#getUnocupiedSeatsCount in
        Printf.printf "the answer is %d\n" answer; answer