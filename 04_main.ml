open Base
open Stdio

let directions =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let is_valid array direction x y =
  let arraySize = Array.length array in
  let xPos = x + (3 * fst direction) in
  let yPos = y + (3 * snd direction) in
  (xPos >= 0 && xPos < arraySize) && yPos >= 0 && yPos < arraySize

let is_matching array direction x y =
  let mChar = Char.equal array.(x + fst direction).(y + snd direction) 'M' in
  let aChar =
    Char.equal array.(x + (2 * fst direction)).(y + (2 * snd direction)) 'A'
  in
  let sChar =
    Char.equal array.(x + (3 * fst direction)).(y + (3 * snd direction)) 'S'
  in
  mChar && aChar && sChar

let part_1 array x y =
  List.count directions ~f:(fun direction ->
      is_valid array direction x y && is_matching array direction x y)

let is_valid_1 array x y =
  let arraySize = Array.length array in
  x > 0 && y > 0 && x < arraySize - 1 && y < arraySize - 1

let is_matching_1 array x y =
  let diagOne =
    Char.equal array.(x + 1).(y + 1) 'S'
    && Char.equal array.(x - 1).(y - 1) 'M'
    || Char.equal array.(x + 1).(y + 1) 'M'
       && Char.equal array.(x - 1).(y - 1) 'S'
  in
  let diagTwo =
    Char.equal array.(x - 1).(y + 1) 'S'
    && Char.equal array.(x + 1).(y - 1) 'M'
    || Char.equal array.(x - 1).(y + 1) 'M'
       && Char.equal array.(x + 1).(y - 1) 'S'
  in
  diagOne && diagTwo

let part_2 array x y =
  if is_valid_1 array x y && is_matching_1 array x y then 1 else 0

let solve matrix part char =
  Array.foldi matrix ~init:0 ~f:(fun i acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun j a elem ->
            if Char.equal elem char then a + part matrix i j else a))

let () =
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  let matrix =
    Array.init (List.length lines) ~f:(fun i ->
        List.nth_exn lines i |> String.to_array)
  in
  solve matrix part_1 'X' |> printf "%d\n";
  solve matrix part_2 'A' |> printf "%d\n"
