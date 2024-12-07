open Base
open Stdio

let parse line =
  match String.split_on_chars line ~on:[ ':' ] with
  | [ hd; tl ] ->
    ( Int.of_string hd
    , List.map (String.split_on_chars (String.strip tl) ~on:[ ' ' ]) ~f:Int.of_string )
  | _ -> assert false

let rec find_sum concat acc sum = function
  | _ when acc > sum -> false
  | [] -> sum = acc
  | hd :: tl ->
    find_sum concat (acc + hd) sum tl
    || find_sum concat (hd * if Int.equal acc 0 then 1 else acc) sum tl
    || (concat
        && find_sum concat (Int.of_string @@ Int.to_string acc ^ Int.to_string hd) sum tl
       )

let solve ?(concat = false) lines =
  let lines = List.map lines ~f:parse in
  List.fold lines ~init:0 ~f:(fun acc (sum, numbers) ->
    acc + if find_sum concat 0 sum numbers then sum else 0)

let () =
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  solve lines |> printf "%d\n";
  solve ~concat:true lines |> printf "%d\n"
