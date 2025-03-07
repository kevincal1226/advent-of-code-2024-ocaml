(* part 1 *)

type block_type =
  | File of int * int
  | Free_space of int * int

let make_block idx size = if idx mod 2 = 0 then File (idx / 2, size) else Free_space (-1, size)

let single_input, chunk_input =
  let line = read_line () in
  ( line
    |> String.to_seq
    |> Seq.map int_of_char
    |> Seq.map (fun v -> v - int_of_char '0')
    |> Seq.fold_lefti
         (fun acc idx x ->
            Seq.ints 0
            |> Seq.take x
            |> Seq.fold_left (fun acc2 i -> if idx mod 2 = 0 then (idx / 2) :: acc2 else -1 :: acc2) acc)
         []
    |> List.rev
    |> Array.of_list
  , line
    |> String.to_seq
    |> Seq.map int_of_char
    |> Seq.map (fun v -> v - int_of_char '0')
    |> Seq.mapi make_block
    |> Array.of_seq )
;;

let rec get_checksum arr arr_idx checksum =
  try
    match arr.(arr_idx) with
    | v when v >= 0 -> get_checksum arr (succ arr_idx) (checksum + (v * arr_idx))
    | _ -> get_checksum arr (succ arr_idx) checksum
  with
  | Invalid_argument _ -> checksum
;;

let rec move_files arr l r =
  if l >= r
  then get_checksum arr 0 0
  else if arr.(l) >= 0
  then move_files arr (succ l) r
  else if arr.(r) < 0
  then move_files arr l (pred r)
  else (
    let () = arr.(l) <- arr.(r) in
    arr.(r) <- -1;
    move_files arr (succ l) (pred r))
;;

move_files single_input 0 (Array.length single_input - 1) |> Printf.printf "day 9 part 1 output: %i\n"

(* part 2 *)
let rec get_checksum arr arr_idx abs_idx checksum =
  try
    match arr.(arr_idx) with
    | Free_space (_, sz) -> get_checksum arr (succ arr_idx) (abs_idx + sz) checksum
    | File (id, sz) ->
      get_checksum arr (succ arr_idx) (abs_idx + sz) (checksum + (id * sz * ((2 * abs_idx) + sz - 1) / 2))
  with
  | Invalid_argument _ -> checksum
;;

let insert_after arr index value =
  let len = Array.length arr in
  let new_arr = Array.make (len + 1) value in
  Array.blit arr 0 new_arr 0 (index + 1);
  new_arr.(index + 1) <- value;
  Array.blit arr (index + 1) new_arr (index + 2) (len - index - 1);
  new_arr
;;

let rec move_files arr l r =
  if r < 0
  then get_checksum arr 0 0 0
  else if l >= r
  then move_files arr 0 (pred r)
  else (
    match arr.(l) with
    | File (_, _) -> move_files arr (succ l) r
    | Free_space (_, free_sz) ->
      (match arr.(r) with
       | Free_space (_, _) -> move_files arr 0 (pred r)
       | File (id, file_sz) ->
         if file_sz > free_sz
         then move_files arr (succ l) r
         else if file_sz = free_sz
         then (
           let () = arr.(l) <- File (id, file_sz) in
           arr.(r) <- Free_space (-1, file_sz);
           move_files arr 0 (pred r))
         else (
           let () = arr.(l) <- File (id, file_sz) in
           arr.(r) <- Free_space (-1, file_sz);
           move_files (insert_after arr l (Free_space (-1, free_sz - file_sz))) 0 r)))
;;

move_files chunk_input 0 (Array.length chunk_input - 1) |> Printf.printf "\nday 9 part 2 output: %i\n"
