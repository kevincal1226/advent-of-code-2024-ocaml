(* part 1 *)

let rec read_input grid =
  try
    let line =
      read_line () |> String.to_seq |> Seq.map int_of_char |> Seq.map (( + ) (-int_of_char '0')) |> Array.of_seq
    in
    read_input (line :: grid)
  with
  | End_of_file -> grid |> List.rev |> Array.of_list
;;

let grid = read_input []
let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let rec find_nines prev row col nines =
  try
    if grid.(row).(col) != prev + 1
    then ()
    else if grid.(row).(col) = 9
    then Hashtbl.replace nines (row, col) 1
    else dirs |> List.iter (fun (dx, dy) -> find_nines grid.(row).(col) (row + dx) (col + dy) nines)
  with
  | Invalid_argument _ -> ()
;;

grid
|> Array.mapi (fun row_idx row ->
  row
  |> Array.mapi (fun col_idx v ->
    match v with
    | 0 ->
      let tbl = Hashtbl.create 1 in
      dirs |> List.iter (fun (dx, dy) -> find_nines grid.(row_idx).(col_idx) (row_idx + dx) (col_idx + dy) tbl);
      Hashtbl.length tbl
    | _ -> 0)
  |> Array.fold_left ( + ) 0)
|> Array.fold_left ( + ) 0
|> Printf.printf "day 10 part 1 output: %i\n"

(* part 2 *)

let rec find_nines prev row col nines =
  try
    if grid.(row).(col) != prev + 1
    then ()
    else if grid.(row).(col) = 9
    then nines := succ !nines
    else dirs |> List.iter (fun (dx, dy) -> find_nines grid.(row).(col) (row + dx) (col + dy) nines)
  with
  | Invalid_argument _ -> ()
;;

grid
|> Array.mapi (fun row_idx row ->
  row
  |> Array.mapi (fun col_idx v ->
    match v with
    | 0 ->
      let nines = ref 0 in
      dirs |> List.iter (fun (dx, dy) -> find_nines grid.(row_idx).(col_idx) (row_idx + dx) (col_idx + dy) nines);
      !nines
    | _ -> 0)
  |> Array.fold_left ( + ) 0)
|> Array.fold_left ( + ) 0
|> Printf.printf "day 10 part 2 output: %i\n"
