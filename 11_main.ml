open Base
open Stdio

let rec blink (stone : Int64.t) (blinks : Int64.t) : Int64.t =
  let len = String.length (Int64.to_string stone) in
  let half_len = len / 2 in
  let half_stone_str = String.sub (Int64.to_string stone) ~pos:0 ~len:half_len in
  let second_half_stone_str =
    String.sub (Int64.to_string stone) ~pos:half_len ~len:(len - half_len)
  in
  if Int64.equal blinks (Int64.of_int 1)
  then (
    match len % 2 with
    | 0 -> Int64.of_int 2
    | _ -> Int64.of_int 1)
  else if Int64.equal stone Int64.zero
  then blink Int64.one (Int64.pred blinks)
  else if len % 2 = 0
  then
    Int64.( + )
      (blink (Int64.of_string half_stone_str) (Int64.pred blinks))
      (blink (Int64.of_string second_half_stone_str) (Int64.pred blinks))
  else blink (Int64.( * ) stone (Int64.of_int 2024)) (Int64.pred blinks)

let solve stones blinks =
  List.fold stones ~init:(Int64.of_int 0) ~f:(fun acc stone ->
    Int64.( + ) acc @@ blink stone blinks)

let () =
  let lines = In_channel.read_all "data.txt" in
  let numbers = String.split_on_chars lines ~on:[ ' ' ] |> List.map ~f:Int64.of_string in
  solve numbers (Int64.of_int 25) |> Int64.to_string |> printf "%s\n"
