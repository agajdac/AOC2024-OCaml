open Base
open Stdio

type mixed =
  | Bool of bool
  | Pair of int * int

let extract_numbers ?(ignore_dos = true) line =
  let regex =
    Re.(
      compile
      @@ alt
           [ seq [ str "mul("; group (rep1 digit); str ","; group (rep1 digit); str ")" ]
           ; str "don't()"
           ; str "do()"
           ])
  in
  let matches = Re.all regex line in
  let results =
    List.map matches ~f:(fun group ->
      match Re.Group.get_opt group 1 with
      | Some _ ->
        Pair (Int.of_string @@ Re.Group.get group 1, Int.of_string @@ Re.Group.get group 2)
      | None when String.equal "do()" (Re.Group.get group 0) -> Bool true
      | None when String.equal "don't()" (Re.Group.get group 0) -> Bool false
      | _ -> assert false)
  in
  fst
  @@ List.fold results ~init:(0, true) ~f:(fun (acc, is_enabled) group ->
    match group with
    | Bool is_enabled -> acc, is_enabled
    | Pair (x, y) -> (acc + if is_enabled || ignore_dos then x * y else 0), is_enabled)

let solve lines = extract_numbers lines
let solve_2 lines = extract_numbers ~ignore_dos:false lines

let () =
  let lines = In_channel.read_all "data.txt" in
  solve lines |> printf "%d\n";
  solve_2 lines |> printf "%d\n"
