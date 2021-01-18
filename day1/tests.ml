open Day1

open Alcotest


let result = read_lines;

(* Section #2 *)
module To_test = struct
    let sum () =
        let x = Day1.numberSum "1234" in
            x = 10

end

(* Section #3 *)

let numberSum () = Alcotest.(check boolean) "sum" true (To_test.sum ())

(* Section #4 *)
let test_set = 
[ "numberSum", `Slow, numberSum]

(* Section #5 *)

let () =
  Alcotest.run "Day1"
    [ "advent day 1", test_set;]
