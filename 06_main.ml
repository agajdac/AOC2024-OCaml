open Base
open Stdio

let turn_right = function
  | `Up -> `Right
  | `Right -> `Down
  | `Down -> `Left
  | `Left -> `Up

let undo_move (x, y) = function
  | `Up -> x, y + 1
  | `Down -> x, y - 1
  | `Left -> x + 1, y
  | `Right -> x - 1, y

let is_cycle cycleMatrix (x, y) = function
  | `Up when cycleMatrix.(y).(x) land 0x1 > 0 -> true
  | `Down when cycleMatrix.(y).(x) land 0x2 > 0 -> true
  | `Left when cycleMatrix.(y).(x) land 0x4 > 0 -> true
  | `Right when cycleMatrix.(y).(x) land 0x8 > 0 -> true
  | _ -> false

let rec move matrix (x, y) direction cycleMatrix =
  let matrixSize = Array.length matrix in
  if x >= matrixSize || y >= matrixSize || x < 0 || y < 0
  then Some matrix
  else if Char.equal matrix.(y).(x) '#'
  then move matrix (undo_move (x, y) direction) (turn_right direction) cycleMatrix
  else if is_cycle cycleMatrix (x, y) direction
  then None
  else (
    matrix.(y).(x) <- 'Z';
    match direction with
    | `Up ->
      cycleMatrix.(y).(x) <- cycleMatrix.(y).(x) lor 0x1;
      move matrix (x, y - 1) `Up cycleMatrix
    | `Down ->
      cycleMatrix.(y).(x) <- cycleMatrix.(y).(x) lor 0x2;
      move matrix (x, y + 1) `Down cycleMatrix
    | `Left ->
      cycleMatrix.(y).(x) <- cycleMatrix.(y).(x) lor 0x4;
      move matrix (x - 1, y) `Left cycleMatrix
    | `Right ->
      cycleMatrix.(y).(x) <- cycleMatrix.(y).(x) lor 0x8;
      move matrix (x + 1, y) `Right cycleMatrix)

let get_cycleMatrix matrix =
  Array.init (Array.length matrix) ~f:(fun _ ->
    Array.init (Array.length matrix) ~f:(fun _ -> 0))

let find_cycle matrix (x, y) (ox, oy) =
  if x = ox && y = oy
  then false
  else (
    let matrix = Array.map (Array.copy matrix) ~f:Array.copy in
    matrix.(oy).(ox) <- '#';
    match move matrix (x, y) `Up (get_cycleMatrix matrix) with
    | None -> true
    | Some _ -> false)

let solve matrix (x, y) =
  let savedMatrix = Array.map (Array.copy matrix) ~f:Array.copy in
  let matrix = move matrix (x, y) `Up (get_cycleMatrix matrix) |> Option.value_exn in
  ( Array.fold matrix ~init:0 ~f:(fun acc row -> acc + Array.count row ~f:(Char.equal 'Z'))
  , Array.foldi matrix ~init:0 ~f:(fun i acc row ->
      acc + Array.counti row ~f:(fun j _ -> find_cycle savedMatrix (x, y) (i, j))) )

let find_guard matrix =
  Array.find_mapi matrix ~f:(fun y row ->
    Array.find_mapi row ~f:(fun x cell ->
      if Char.equal cell '^' then Some (x, y) else None))
  |> Option.value_exn

let () =
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  let matrix =
    Array.init (List.length lines) ~f:(fun i -> List.nth_exn lines i |> String.to_array)
  in
  let x, y = solve matrix (find_guard matrix) in
  printf "part 1: %d\npart 2: %d\n" x y
