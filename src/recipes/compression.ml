open! Core_kernel
module Map = Glossary.Map

type data =
  | Skip of int
  | Item of int
[@@deriving sexp]

let all =
  lazy (List.foldi Glossary.all ~init:Map.empty ~f:(fun data acc key -> Map.add_exn acc ~key ~data))

let increase = function
| None -> Some 1
| Some x -> Some (x + 1)

let ( << ), ( >> ), ( ||| ), ( &&& ) = ( lsl ), ( lsr ), ( lor ), ( land )

let merge (list : (Glossary.t * int) list) =
  let all = force all in
  let ingredients = List.fold list ~init:Map.empty ~f:(fun acc (key, data) -> Map.set acc ~key ~data) in
  Map.fold2 ingredients all ~init:([], None) ~f:(fun ~key:_ ~data ((acc_list, skip) as acc) ->
      match data with
      | `Left _ -> acc
      | `Both (0, _)
       |`Right _ ->
        acc_list, increase skip
      | `Both (qty, _) ->
        Item qty :: Option.value_map skip ~default:acc_list ~f:(fun x -> Skip x :: acc_list), None)
  |> fst
  |> List.rev

let compress (list : (Glossary.t * int) list) =
  let merged = merge list in

  (*
    Unicode-inspired binary encoding
    0....... :: skip
    .******* :: 0-128 how many to skip

    1....... :: item
    .0...... :: one byte
    .1...... :: 2 bytes
    ..****** ******** :: quantity
  *)
  let buf = Buffer.create 128 in
  List.iter merged ~f:(function
    | Skip x ->
      let n = Int.(x /% 127) in
      Fn.apply_n_times ~n (fun () -> Buffer.add_char buf (Char.of_int_exn 127)) ();
      Buffer.add_char buf (x &&& 127 |> Char.of_int_exn)
    | Item qty when qty <= 63 -> Buffer.add_char buf (qty ||| 128 |> Char.of_int_exn)
    | Item qty ->
      Buffer.add_char buf (qty >> 8 ||| 192 |> Char.of_int_exn);
      Buffer.add_char buf (qty &&& 255 |> Char.of_int_exn));
  (* let debug =
       Buffer.contents buf |> String.concat_map ~sep:"," ~f:(fun c -> Char.to_int c |> Int.to_string)
     in *)
  Buffer.contents buf |> Base64.encode_string ~pad:false

let decompress b64 =
  let process buffer len =
    let all = force all in
    let rec loop pos id acc =
      if pos >= len
      then acc
      else (
        match buffer.[pos] |> Char.to_int with
        | c when c <= 127 ->
          (* skip *)
          loop (pos + 1) (id + c) acc
        | c when c >= 192 ->
          (* 2 bytes *)
          let left = c &&& 63 << 8 in
          let right = buffer.[pos + 1] |> Char.to_int in
          let data = min (left ||| right) 999 in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 2) (id + 1) (Map.add_exn acc ~key ~data)
        | c ->
          (* 1 byte *)
          let data = min (c &&& 63) 999 in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 1) (id + 1) (Map.add_exn acc ~key ~data))
    in
    loop 0 0 Map.empty
  in
  match Base64.decode ~pad:false b64 with
  | Ok s -> Or_error.try_with (fun () -> process s (String.length s))
  | Error (`Msg s) -> Or_error.error_string s
