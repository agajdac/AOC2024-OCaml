open Base
open Stdio

let is_safe ?(remove_index = Int.max_value) line =
  let line = String.split_on_chars line ~on:[ ' ' ] in
  let line = List.map line ~f:Int.of_string in
  let line = List.filteri line ~f:(fun i _ -> i <> remove_index) in
  (fst
   @@ List.fold
        line
        ~init:(true, List.nth_exn line 0)
        ~f:(fun (acc, prev) curr -> Int.abs (curr - prev) <= 3 && acc, curr))
  && (List.is_sorted_strictly line ~compare:Int.descending
      || List.is_sorted_strictly line ~compare:Int.ascending)

let solve lines = List.count lines ~f:is_safe

let solve_2 lines =
  List.count lines ~f:(fun line ->
    List.existsi (String.split_on_chars line ~on:[ ' ' ]) ~f:(fun i _ ->
      is_safe ~remove_index:i line))

let () =
  let content = In_channel.read_all "data.txt" in
  let lines = String.split_lines content in
  solve lines |> printf "%d\n";
  solve_2 lines |> printf "%d\n"
