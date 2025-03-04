open Printf
open String

(* part 1 *)
let insert_pair tbl key value =
    match Hashtbl.find_opt tbl key with
    | None -> let temp = Hashtbl.create 1 in Hashtbl.add temp value 1; Hashtbl.add tbl key temp
    | Some r -> Hashtbl.add r value 1
;;

let rec read_input orders updates =
    try
        let line = read_line () in
        match String.length line with
        | 5 -> let split = String.split_on_char '|' line
            |> Array.of_list
            |> Array.map int_of_string in
            insert_pair orders split.(0) split.(1);
            read_input orders updates
        | _ -> let split = String.split_on_char ',' line
            |> Array.of_list
            |> Array.map int_of_string in
            read_input orders (split :: updates)
    with End_of_file -> orders, updates |> Array.of_list;;

let rec check_valid_update orders l r update =
    if l = Array.length update then
        update.(Array.length update / 2)
    else if r = Array.length update then
        check_valid_update orders (l+1) (l+2) update
    else
        match Hashtbl.find_opt orders update.(r) with
        |None -> check_valid_update orders l (r+1) update
        |Some tbl -> match Hashtbl.find_opt tbl update.(l) with
            |None -> check_valid_update orders l (r+1) update
            |Some _ -> 0
;;

let orders, updates = read_input (Hashtbl.create 100) [];;

updates
|> Array.map (fun f -> check_valid_update orders 0 1 f)
|> Array.fold_left (+) 0
|> printf "day 5 part 1 output: %i\n";;

(* part 2 *)
let rec fix_update orders l r fixed update =
    if l = Array.length update then
        if fixed then update.(Array.length update / 2) else 0
    else if r = Array.length update then
        fix_update orders (l+1) (l+2) fixed update
    else
        match Hashtbl.find_opt orders update.(r) with
        |None -> fix_update orders l (r+1) fixed update
        |Some tbl -> match Hashtbl.find_opt tbl update.(l) with
            |None -> fix_update orders l (r+1) fixed update
            |Some _ -> let tmp = update.(l) in
                update.(l) <- update.(r);
                update.(r) <- tmp;
                fix_update orders l (r+1) true update
;;

updates
    |> Array.map (fun f -> fix_update orders 0 1 false f)
    |> Array.fold_left (+) 0
    |> printf "day 5 part 2 output: %i\n"
