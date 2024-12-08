open Base
open Stdio

let compare_pairs (x1, y1) (x2, y2) =
  match Int.compare x1 x2 with
  | 0 -> Int.compare y1 y2
  | cmp -> cmp

let add_to_map map key value =
  Hashtbl.update map key ~f:(function
    | None -> [ value ]
    | Some lst -> value :: lst)

let is_valid matrixSize (x, y) = x >= 0 && x < matrixSize && y >= 0 && y < matrixSize

let calc_antinode propagate matrixSize (x, y) (ox, oy) =
  if x <> ox || y <> oy
  then (
    let rec find_nodes propagate matrixSize acc i (x, y) (ax, ay) =
      let antinode = x + (i * ax), y + (i * ay) in
      if is_valid matrixSize antinode && (propagate || i = 1)
      then find_nodes propagate matrixSize (antinode :: acc) (i + 1) (x, y) (ax, ay)
      else acc
    in
    find_nodes propagate matrixSize [] (if propagate then 0 else 1) (x, y) (x - ox, y - oy))
  else []

let solve propagate matrix =
  let matrixSize = Array.length matrix in
  let map = Hashtbl.create (module Char) in
  Array.iteri matrix ~f:(fun i row ->
    Array.iteri row ~f:(fun j elem ->
      if Char.( <> ) elem '.' then add_to_map map elem (i, j)));
  let antinodes =
    Hashtbl.fold ~init:[] map ~f:(fun ~key:_ ~data:lst acc ->
      acc
      @ List.fold lst ~init:[] ~f:(fun acc p1 ->
        acc
        @ List.fold ~init:[] lst ~f:(fun acc p2 ->
          acc @ calc_antinode propagate matrixSize p1 p2)))
  in
  List.dedup_and_sort antinodes ~compare:compare_pairs

let () =
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  let matrix =
    Array.init (List.length lines) ~f:(fun i -> List.nth_exn lines i |> String.to_array)
  in
  solve false matrix |> List.length |> printf "%d\n";
  solve true matrix |> List.length |> printf "%d\n"
