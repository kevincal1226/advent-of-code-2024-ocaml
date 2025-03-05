open Printf
open String

(* part 1 *)

let rec read_input input =
  try
    let line = read_line () in
    let split = String.split_on_char ':' line in
    let target = List.nth split 0 |> int_of_string in
    let eq = List.nth split 1 |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> Array.of_list in
    read_input ((target, eq) :: input)
  with
  | End_of_file -> input
;;

let input = read_input []

let rec backtrack target curr arr idx =
  try backtrack target (curr + arr.(idx)) arr (succ idx) || backtrack target (curr * arr.(idx)) arr (succ idx) with
  | Invalid_argument _ -> target = curr
;;

input
|> List.filter_map (fun (target, eq) -> if backtrack target eq.(0) eq 1 then Some target else None)
|> List.fold_left ( + ) 0
|> printf "day 7 part 1 output: %i\n"

(* part 2 *)

let offset (n : float) = 10. ** (1. +. Float.floor (log10 n)) |> int_of_float

let rec backtrack target curr arr idx =
  try
    backtrack target (curr + arr.(idx)) arr (succ idx)
    || backtrack target (curr * arr.(idx)) arr (succ idx)
    || backtrack target ((curr * (float_of_int arr.(idx) |> offset)) + arr.(idx)) arr (succ idx)
  with
  | Invalid_argument _ -> target = curr
;;

input
|> List.filter_map (fun (target, eq) -> if backtrack target eq.(0) eq 1 then Some target else None)
|> List.fold_left ( + ) 0
|> printf "day 7 part 1 output: %i\n"
