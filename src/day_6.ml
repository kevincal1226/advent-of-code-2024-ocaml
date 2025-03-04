(* part 1 *)
let dirs = [|[|-1;0|]; [|0;1|]; [|1;0|]; [|0;-1|]|];;

let rec read_input curr_row start_row start_col grid =
    try
    let line = read_line ()
        |> String.to_seq
        |> Array.of_seq in
    match Array.find_index (fun s -> s = '^') line with
        | None -> read_input (curr_row+1) start_row start_col (line :: grid)
        | Some found_col -> read_input (curr_row+1) curr_row found_col (line :: grid)
with End_of_file -> start_row, start_col, grid |> List.rev |> Array.of_list
;;

let start_row, start_col, grid = read_input 0 0 0 [];;

let rec trace_path curr_row curr_col curr_dir dirs grid path =
    if curr_row < 0 || curr_col < 0 || curr_row >= Array.length grid || curr_col >= Array.length grid.(0) then
        Hashtbl.length path
    else
        if grid.(curr_row).(curr_col) = '#' then
            let new_dir = (succ curr_dir) mod Array.length dirs in
            let new_row = (curr_row - dirs.(curr_dir).(0) + dirs.(new_dir).(0)) in
            let new_col = (curr_col - dirs.(curr_dir).(1) + dirs.(new_dir).(1)) in
            trace_path new_row new_col new_dir dirs grid path
        else
            let () = if not (Hashtbl.mem path (curr_row, curr_col)) then Hashtbl.add path (curr_row, curr_col) 1; in
            trace_path (curr_row + dirs.(curr_dir).(0)) (curr_col + dirs.(curr_dir).(1)) curr_dir dirs grid path
;;

let path = Hashtbl.create 1 ;;
trace_path start_row start_col 0 dirs grid path
|> Printf.printf "day 6 part 1 output: %i\n";;

(* part 2 *)

let rec find_cycle curr_row curr_col curr_dir dirs grid path =
    if curr_row < 0 || curr_col < 0 || curr_row >= Array.length grid || curr_col >= Array.length grid.(0) then
        0
    else
        if grid.(curr_row).(curr_col) = '#' then
            let new_dir = (succ curr_dir) mod Array.length dirs in
            let new_row = (curr_row - dirs.(curr_dir).(0) + dirs.(new_dir).(0)) in
            let new_col = (curr_col - dirs.(curr_dir).(1) + dirs.(new_dir).(1)) in
            find_cycle new_row new_col new_dir dirs grid path
        else
            match Hashtbl.mem path (curr_row, curr_col, curr_dir) with
            |true -> 1
            |false -> Hashtbl.add path (curr_row, curr_col, curr_dir) 1;
                find_cycle (curr_row + dirs.(curr_dir).(0)) (curr_col + dirs.(curr_dir).(1)) curr_dir dirs grid path
;;

grid
    |> Array.mapi (fun row_idx row -> row
        |> Array.mapi (fun col_idx _ -> 
            match grid.(row_idx).(col_idx) with
        |'.' -> let ret = begin
                grid.(row_idx).(col_idx) <- '#';
            let cycle = find_cycle start_row start_col 0 dirs grid (Hashtbl.create 1) in grid.(row_idx).(col_idx) <- '.';
                cycle
                end in ret
        |_ -> 0)
    |> Array.fold_left (+) 0)
    |> Array.fold_left(+) 0
|> Printf.printf "day 6 part 2 output: %i\n";;
