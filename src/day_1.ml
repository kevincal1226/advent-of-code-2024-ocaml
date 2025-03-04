open Printf
open String

(* part 1 *)
let rec read_input acc1 acc2 =
  try
    let a = split_on_char ' ' (read_line ()) in
    read_input
      (int_of_string (List.nth a 0) :: acc1)
      (int_of_string (List.nth a 1) :: acc2)
  with
  | End_of_file -> acc1, acc2
;;

let a, b = read_input [] []
let a' = List.sort Stdlib.compare a
let b' = List.sort Stdlib.compare b;;

printf "%i\n" (List.fold_left ( + ) 0 (List.map2 (fun x y -> abs (x - y)) a' b'))

(* part 2 *)
let tbl = Hashtbl.create 0;;

List.iter
  (fun x ->
     Hashtbl.replace
       tbl
       x
       (try Hashtbl.find tbl x + 1 with
        | Not_found -> 1))
  b
;;

printf
  "%i\n"
  (List.fold_left
     ( + )
     0
     (List.map
        (fun x ->
           x
           *
           try Hashtbl.find tbl x with
           | Not_found -> 0)
        a))
