open Printf
open String

(* part 1 *)
let string_to_list s =
    List.init (String.length s) (String.get s);;

let rec read_input acc =
    try
    (read_line ()
            |> string_to_list
            |> Array.of_list) :: acc
        |> read_input
with End_of_file -> Array.of_list acc ;;

let (input: char array array) = read_input [] ;;

let num_rows = Array.length input;;
let num_cols = Array.length (input.(0));;

let xmas = [|'X'; 'M'; 'A'; 'S'|];;

let dirs = [|[|0;1|]; [|0;-1|]; [|1;0|]; [|-1;0|]; [|1;1|]; [|1;-1|]; [|-1;1|]; [|-1;-1|]|] ;;

let rec find_xmas (grid: char array array) (idx: int) (row: int) (col: int) (changes: int array) =
    if Array.length xmas = idx then
        1
    else if row < 0 || col < 0 || row >= num_rows || col >= num_cols || grid.(row).(col) != xmas.(idx) then
        0
    else
        find_xmas grid (idx+1) (row+changes.(0)) (col+changes.(1)) changes
;;

input
|> Array.mapi (fun row_idx row  -> row
        |> Array.mapi (fun col_idx c ->
        match c with
        | 'X' -> dirs
                |> Array.map (find_xmas input 0 row_idx col_idx)
                |> Array.fold_left (+) 0
        | _ -> 0)
    |> Array.fold_left (+) 0 )
|> Array.fold_left (+) 0
|> printf "day 4 part 1 output: %i\n"
;;

(* part 2 *)
let find_mas_in_x (grid: char array array) (row: int) (col: int) =
    if row = 0 || col = 0 || row = num_rows-1 || col = num_cols-1 then
        0
    else
        match ((grid.(row-1).(col-1) == 'M' || grid.(row-1).(col-1) == 'S') &&
        (grid.(row+1).(col-1) == 'M' || grid.(row+1).(col-1) == 'S') &&
        (grid.(row-1).(col+1) == 'M' || grid.(row-1).(col+1) == 'S') &&
        (grid.(row+1).(col+1) == 'M' || grid.(row+1).(col+1) == 'S') &&
        (grid.(row-1).(col-1) != grid.(row+1).(col+1)) &&
        (grid.(row-1).(col+1) != grid.(row+1).(col-1))) with
        |true -> 1
        |false -> 0;;

input
|> Array.mapi (fun row_idx row  -> row
        |> Array.mapi (fun col_idx c ->
        match c with
            |'A' -> col_idx
                    |> (find_mas_in_x input row_idx)
            | _ -> 0)
    |> Array.fold_left (+) 0 )
|> Array.fold_left (+) 0
|> printf "day 4 part 1 output: %i\n"
;;
