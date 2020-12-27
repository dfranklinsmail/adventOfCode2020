open OUnit2
open Day7A
open List
open Printf

(*HELPER FUNCTIONS*)
let lineCount fileName = 
    length (read_lines fileName)

let sizeOfBagMatches fileName = 
    let size = length (findMatchingBags (read_lines fileName) ["shiny";"gold"]) in
        size

let rec printBagNames bagNames = 
    match bagNames with 
    | [] -> printf "the end"
    | h::t -> printf "the bag style is %s and colour %s\n"(List.nth h 0) (List.nth h 1);
            printBagNames t

(* let sizeOfAllMatchingBags fileName = 
    let foundBagNames = (findAllMatchingBags (read_lines fileName) [["shiny";"gold"]] []) in
    printBagNames foundBagNames;
    let size = length foundBagNames in
        printf "the size is %d\n" size; size
    *)
let sizeOfAllMatchingBags fileName = 
    let foundBagNames = (findAllMatchingBags (read_lines fileName) [["shiny";"gold"]] []) in
    let size = length foundBagNames in
        printf "the size is %d\n" size; size

(*TEST SUITE*)
let tests = "test suite for day 7 part A from Advent of Code 2020 " >::: [
    
    "parent not found test" >:: (fun _ -> assert_equal 0 (List.length (getParentBagName "vibrant bronze bags contain 1 drab gray bag, 2 faded gray bags." ["shiny";"gold"])));
    "parent found test" >:: (fun _ -> assert_equal 2 (List.length (getParentBagName "vibrant bronze bags contain 1 drab gray bag, 2 faded gray bags." ["drab";"gray"])));

    "test data lc" >:: (fun _ -> assert_equal 9 (lineCount "test.data"));
    "input data lc" >:: (fun _ -> assert_equal 594 (lineCount "input.data"));
    
    "getBagName test" >:: (fun _ -> assert_equal "shiny gold" (getBagName "shiny gold bags contain")); 
   
    "equals test" >:: (fun _ -> assert_equal true (equals ["shiny";"gold"] ["shiny";"gold"]));
    "eaquas test" >:: (fun _ -> assert_equal false (equals ["dull";"brown"] ["shiny";"gold"]));
   
    "containsBagName test empty" >:: (fun _ -> assert_equal false (containsBagName [] []));
    "containsBagName test empty with input" >:: (fun _ -> assert_equal false (containsBagName ["shiny";"gold"] []));
    "containsBagName test not equal" >:: (fun _ -> assert_equal false (containsBagName ["shiny";"gold"] [["dull";"brown"] ]));
    "containsBagName test equal" >:: (fun _ -> assert_equal true (containsBagName ["shiny";"gold"] [["shiny";"gold"]]));
    "containsBagName test equal with others" >:: (fun _ -> assert_equal true (containsBagName ["shiny";"gold"] [["dull";"brown"];["faded";"blue"];["shiny";"gold"]]));
    
    "findMatchingBags test" >:: (fun _ -> assert_equal 2 (sizeOfBagMatches "test.data"));

   "findAllMatchingBags test" >:: (fun _ -> assert_equal 5 (sizeOfAllMatchingBags "test.data"));

   "findAllMatchingBags real" >:: (fun _ -> assert_equal 122 (sizeOfAllMatchingBags "input.data"));
]

let _ = run_test_tt_main tests