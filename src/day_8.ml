(* part 1 *)

let rec read_input antennas curr_row grid =
  try
    let line = read_line () in
    line
    |> String.iteri (fun col node ->
      match Hashtbl.find_opt antennas node with
      | None -> if node != '.' then Hashtbl.add antennas node [ [| curr_row; col |] ]
      | Some k -> Hashtbl.replace antennas node ([| curr_row; col |] :: k));
    read_input antennas (curr_row + 1) ((line |> String.to_seq |> Array.of_seq) :: grid)
  with
  | End_of_file -> antennas, grid |> List.rev |> Array.of_list
;;

let antennas, grid = read_input (Hashtbl.create 62) 0 []
let antinodes = Hashtbl.create 1

let get_antinodes p1 p2 =
  let x1 = p1.(0) in
  let y1 = p1.(1) in
  let x2 = p2.(0) in
  let y2 = p2.(1) in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let () =
    try
      let _ = grid.(x1 - dx).(y1 - dy) in
      Hashtbl.replace antinodes (x1 - dx, y1 - dy) 1
    with
    | Invalid_argument _ -> ()
  in
  try
    let _ = grid.(x2 + dx).(y2 + dy) in
    Hashtbl.replace antinodes (x2 + dx, y2 + dy) 1
  with
  | Invalid_argument _ -> ()
;;

antennas
|> Hashtbl.to_seq_values
|> Seq.iter (fun ants ->
  ants
  |> List.iteri (fun idx p1 ->
    let other_ants = List.drop (idx + 1) ants in
    other_ants |> List.iter (get_antinodes p1)))
;;

Hashtbl.length antinodes |> Printf.printf "day 8 part 1 output: %i\n"

(* part 2 *)

let antinodes = Hashtbl.create 1

let get_antinodes p1 p2 =
  let x1 = p1.(0) in
  let y1 = p1.(1) in
  let x2 = p2.(0) in
  let y2 = p2.(1) in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let cnt = ref 0 in
  let () =
    try
      while true do
        let _ = grid.(x1 - (dx * !cnt)).(y1 - (dy * !cnt)) in
        Hashtbl.replace antinodes (x1 - (dx * !cnt), y1 - (dy * !cnt)) 1;
        cnt := succ !cnt
      done
    with
    | Invalid_argument _ -> cnt := 0
  in
  try
    while true do
      let _ = grid.(x2 + (dx * !cnt)).(y2 + (dy * !cnt)) in
      Hashtbl.replace antinodes (x2 + (dx * !cnt), y2 + (dy * !cnt)) 1;
      cnt := succ !cnt
    done
  with
  | Invalid_argument _ -> ()
;;

antennas
|> Hashtbl.to_seq_values
|> Seq.iter (fun ants ->
  ants
  |> List.iteri (fun idx p1 ->
    let other_ants = List.drop (idx + 1) ants in
    other_ants |> List.iter (get_antinodes p1)))
;;

Hashtbl.length antinodes |> Printf.printf "day 8 part 2 output: %i\n"
