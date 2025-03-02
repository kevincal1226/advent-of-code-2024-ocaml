open Printf
open String

(* part 1 *)
let rec read_input acc =
    try 
    let a = split_on_char ' ' (read_line ()) in 
    let a' = List.map int_of_string a in
    read_input (a' :: acc)
    with End_of_file -> acc

let input = read_input [] ;;

let rec is_safe safety lst =
    match List.length lst with
        | 1 -> safety
        | 2 -> safety
        | _ -> let diff1 = (List.nth lst 0) - (List.nth lst 1) in
    let diff2 = (List.nth lst 1) - (List.nth lst 2) in
        is_safe (abs diff1 >= 1 && abs diff1 <= 3 && abs diff2 >= 1 && abs diff2 <= 3 && diff1*diff2 > 0 && safety) (List.tl lst) ;;

printf "day 2 part 1 output: %i\n" (List.length (List.filter (fun x -> is_safe true x) input));;

(* part 2 *)

printf "day 2 part 2 output: %i\n" (List.length (List.filter (fun l -> List.length (List.filter (fun x -> is_safe true x) (List.map (fun idx -> List.filteri (fun i tmp -> i != idx) l) (List.init (List.length l) Fun.id))) > 0) (input))) ;;
