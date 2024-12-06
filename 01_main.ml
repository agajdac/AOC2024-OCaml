open Base
open Stdio

let parse_line line left right =
  let line = String.substr_replace_first line ~pattern:"   " ~with_:" " in
  match String.split_on_chars line ~on:[ ' ' ] with
  | [ a; b ] -> Int.of_string a :: left, Int.of_string b :: right
  | _ -> assert false

let parse lines =
  List.fold lines ~init:([], []) ~f:(fun (left, right) line -> parse_line line left right)

let solve lines =
  let left, right = parse lines in
  let left = List.sort left ~compare:Int.compare in
  let right = List.sort right ~compare:Int.compare in
  List.fold (List.zip_exn left right) ~init:0 ~f:(fun acc (leftElem, rightElem) ->
    acc + Int.abs (leftElem - rightElem))

let solve_2 lines =
  let left, right = parse lines in
  List.fold left ~init:0 ~f:(fun acc leftElem ->
    acc + (leftElem * List.count right ~f:(fun x -> x = leftElem)))

let () =
  let content = In_channel.read_all "data.txt" in
  let lines = String.split_lines content in
  solve lines |> printf "%d\n";
  solve_2 lines |> printf "%d\n"
