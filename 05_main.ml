open Base
open Stdio

let rec split_file acc = function
  | hd :: tl when String.equal hd "" -> acc, tl
  | hd :: tl -> split_file (hd :: acc) tl
  | _ -> assert false

let parse_rules rules =
  List.map rules ~f:(fun rule ->
    match String.split rule ~on:'|' with
    | [ a; b ] -> Int.of_string a, Int.of_string b
    | _ -> assert false)

let parse_updates updates =
  List.map updates ~f:(fun update ->
    List.map (String.split update ~on:',') ~f:Int.of_string)

let parse lines =
  let rules, updates = split_file [] lines in
  let rules = parse_rules rules in
  let updates = parse_updates updates in
  rules, updates

let is_ordering_correct rules update =
  not
  @@ List.existsi update ~f:(fun i num ->
    let rules = List.filter rules ~f:(fun (rule, _) -> Int.equal num rule) in
    List.exists rules ~f:(fun (_, y) ->
      match List.findi update ~f:(fun _ elem -> y = elem) with
      | Some (j, _) -> i > j
      | None -> false))

let rec fix_ordering rules update =
  match
    Array.foldi update ~init:None ~f:(fun i acc num ->
      let rules = List.filter rules ~f:(fun (rule, _) -> Int.equal num rule) in
      match acc with
      | Some _ -> acc
      | None ->
        List.fold_left rules ~init:None ~f:(fun acc (_, y) ->
          match Array.findi update ~f:(fun _ elem -> y = elem) with
          | Some (j, _) when i > j -> Some (i, j)
          | _ -> acc))
  with
  | Some (i, j) ->
    Array.swap update i j;
    fix_ordering rules update
  | None -> update

let solve lines =
  let rules, updates = parse lines in
  List.fold updates ~init:0 ~f:(fun acc update ->
    acc
    +
    if is_ordering_correct rules update
    then List.nth_exn update (List.length update / 2)
    else 0)

let solve_2 lines =
  let rules, updates = parse lines in
  let updates =
    List.filter updates ~f:(fun update -> not @@ is_ordering_correct rules update)
  in
  let updates = List.map updates ~f:Array.of_list in
  let updates = List.map updates ~f:(fun update -> fix_ordering rules update) in
  List.fold updates ~init:0 ~f:(fun acc update ->
    acc + Array.get update (Array.length update / 2))

let () =
  let lines = In_channel.read_all "data.txt" |> String.split_lines in
  printf "%d\n" @@ solve lines;
  printf "%d\n" @@ solve_2 lines
