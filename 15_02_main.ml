open Base
open Stdio

let sum_points matrix =
  Array.foldi matrix ~init:0 ~f:(fun i acc line ->
    acc
    + Array.foldi line ~init:0 ~f:(fun j acc elem ->
      acc + if Char.equal elem '[' then (i * 100) + j else 0))

let swap matrix (x1, y1) (x2, y2) =
  let temp = matrix.(x1).(y1) in
  matrix.(x1).(y1) <- matrix.(x2).(y2);
  matrix.(x2).(y2) <- temp

let direction_to_vector (x, y) = function
  | '<' -> x, y - 1
  | '>' -> x, y + 1
  | '^' -> x - 1, y
  | 'v' -> x + 1, y
  | _ -> assert false

let rec can_move (x, y) matrix direction =
  let newX, newY = direction_to_vector (x, y) direction in
  let objectOnNewPos = matrix.(newX).(newY) in
  match objectOnNewPos with
  | '.' -> true
  | '#' -> false
  | ('[' | ']') when Char.equal direction '<' || Char.equal direction '>' ->
    can_move (newX, newY) matrix direction
  | '[' ->
    can_move (newX, newY) matrix direction && can_move (newX, newY + 1) matrix direction
  | ']' ->
    can_move (newX, newY) matrix direction && can_move (newX, newY - 1) matrix direction
  | _ -> assert false

let rec move_box (x, y) matrix direction =
  let newX, newY = direction_to_vector (x, y) direction in
  let objectOnNewPos = matrix.(newX).(newY) in
  match objectOnNewPos with
  | '.' ->
    swap matrix (x, y) (newX, newY);
    true
  | ('[' | ']') when Char.equal direction '<' || Char.equal direction '>' ->
    if move_box (newX, newY) matrix direction
    then (
      swap matrix (x, y) (newX, newY);
      true)
    else assert false
  | '[' ->
    if
      move_box (newX, newY) matrix direction && move_box (newX, newY + 1) matrix direction
    then (
      swap matrix (x, y) (newX, newY);
      true)
    else assert false
  | ']' ->
    if
      move_box (newX, newY) matrix direction && move_box (newX, newY - 1) matrix direction
    then (
      swap matrix (x, y) (newX, newY);
      true)
    else assert false
  | _ -> assert false

let movePlayer_2 (x, y) matrix direction =
  match can_move (x, y) matrix direction with
  | true ->
    if move_box (x, y) matrix direction
    then direction_to_vector (x, y) direction
    else assert false
  | false -> x, y

let find_guard matrix =
  Array.find_mapi matrix ~f:(fun y row ->
    Array.find_mapi row ~f:(fun x cell ->
      if Char.equal cell '@' then Some (y, x) else None))
  |> Option.value_exn

let solve_2 matrix directions =
  ignore
  @@ List.fold directions ~init:(find_guard matrix) ~f:(fun acc direction ->
    movePlayer_2 acc matrix direction)

let () =
  (*data was prepared manually*)
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  let matrix =
    Array.init (List.length lines) ~f:(fun i -> List.nth_exn lines i |> String.to_array)
  in
  let directions =
    In_channel.read_all "moves.txt"
    |> String.filter ~f:(fun c ->
      Char.equal 'v' c || Char.equal '^' c || Char.equal '<' c || Char.equal '>' c)
    |> String.to_list
  in
  solve_2 matrix directions;
  printf "%d\n" @@ sum_points matrix
