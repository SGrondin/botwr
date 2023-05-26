open! Core_kernel
module Map = Glossary.Map

type data =
  | Skip of int
  | Item of int
[@@deriving sexp]

let all =
  lazy (List.foldi Glossary.all ~init:Map.empty ~f:(fun data acc key -> Map.add_exn acc ~key ~data))

let ( << ), ( >> ), ( ||| ), ( &&& ) = ( lsl ), ( lsr ), ( lor ), ( land )

let merge (list : (Glossary.t * int) list) =
  let all = force all in
  let ingredients = List.fold list ~init:Map.empty ~f:(fun acc (key, data) -> Map.set acc ~key ~data) in
  Map.fold2 ingredients all ~init:([], 0) ~f:(fun ~key:_ ~data ((acc_list, skip) as acc) ->
      match data with
      | `Left _ -> acc
      | `Both (0, _)
       |`Right _ ->
        acc_list, skip + 1
      | `Both (qty, _) when skip = 0 -> Item qty :: acc_list, 0
      | `Both (qty, _) -> Item qty :: Skip skip :: acc_list, 0)
  |> fst
  |> List.rev

let compress ~max_hearts ~max_stamina (list : (Glossary.t * int) list) =
  let merged = merge list in
  let buf = Buffer.create 128 in
  (* Header:
     0, len, max_hearts, max_stamina
  *)
  Buffer.add_char buf (Char.of_int_exn 0);
  Buffer.add_char buf (Char.of_int_exn 2);
  Buffer.add_char buf (Char.of_int_exn max_hearts);
  Buffer.add_char buf (Char.of_int_exn max_stamina);

  (*
    Unicode-inspired binary encoding
    0....... :: skip
    .******* :: 0-127 how many to skip

    1....... :: item
    .0...... :: one byte
    .1...... :: 2 bytes
    ..****** ******** :: quantity
  *)
  List.iter merged ~f:(function
    | Skip x ->
      let rec drain = function
        | x when x < 127 -> Buffer.add_char buf (Char.of_int_exn x)
        | x ->
          Buffer.add_char buf (Char.of_int_exn 127);
          drain (x - 127)
      in
      drain x
    | Item qty when qty <= 63 -> Buffer.add_char buf (qty ||| 128 |> Char.of_int_exn)
    | Item qty ->
      Buffer.add_char buf (qty >> 8 ||| 192 |> Char.of_int_exn);
      Buffer.add_char buf (qty &&& 255 |> Char.of_int_exn));
  (* let debug =
       Buffer.contents buf |> String.concat_map ~sep:"," ~f:(fun c -> Char.to_int c |> Int.to_string)
     in
     print_endline debug; *)
  Buffer.contents buf |> Base64.encode_string ~pad:false

type t = {
  items: int Map.t;
  max_hearts: int option;
  max_stamina: int option;
}
[@@deriving sexp_of]

let empty = { items = Map.empty; max_hearts = None; max_stamina = None }

let decompress b64 =
  let process buffer len =
    let all = force all in
    let rec loop pos id ({ items; _ } as acc) =
      if pos >= len
      then acc
      else (
        match buffer.[pos] |> Char.to_int with
        | 0 when pos = 0 ->
          (* optional header *)
          let len = buffer.[pos + 1] |> Char.to_int in
          let header = Array.init len ~f:(fun i -> buffer.[i + 2] |> Char.to_int) in
          let acc =
            match header with
            | [| x; y |] -> { acc with max_hearts = Some x; max_stamina = Some y }
            | _ -> acc
          in
          loop (pos + 2 + Array.length header) id acc
        | c when c <= 127 ->
          (* skip *)
          loop (pos + 1) (id + c) acc
        | c when c >= 192 ->
          (* 2 bytes *)
          let left = c &&& 63 << 8 in
          let right = buffer.[pos + 1] |> Char.to_int in
          let data = min (left ||| right) 999 in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 2) (id + 1) { acc with items = Map.add_exn items ~key ~data }
        | c ->
          (* 1 byte *)
          let data = min (c &&& 63) 999 in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 1) (id + 1) { acc with items = Map.add_exn items ~key ~data })
    in
    loop 0 0 empty
  in
  match Base64.decode ~pad:false b64 with
  | Ok s -> Or_error.try_with (fun () -> process s (String.length s))
  | Error (`Msg s) -> Or_error.error_string s
