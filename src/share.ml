open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

type ingredients = (Recipes.Glossary.t * int) list [@@deriving sexp]

module Map = Recipes.Glossary.Map

type data =
  | Skip of int
  | Item of int
[@@deriving sexp]

let all =
  lazy (List.foldi Recipes.Glossary.all ~init:Map.empty ~f:(fun data acc key -> Map.set acc ~key ~data))

let increase = function
| None -> Some 1
| Some x -> Some (x + 1)

let compress (list : ingredients) =
  let all = force all in
  let ingredients = List.fold list ~init:Map.empty ~f:(fun acc (key, data) -> Map.set acc ~key ~data) in
  let merged =
    let merged, last =
      Map.fold2 ingredients all ~init:([], None) ~f:(fun ~key:_ ~data ((acc_list, skip) as acc) ->
          match data with
          | `Left _ -> acc
          | `Both (0, _)
           |`Right _ ->
            acc_list, increase skip
          | `Both (qty, _) ->
            Item qty :: Option.value_map skip ~default:acc_list ~f:(fun x -> Skip x :: acc_list), None)
    in
    Option.value_map last ~default:merged ~f:(fun x -> Skip x :: merged) |> List.rev
  in

  (*
    0....... :: skip
    .******* :: 0-128 how many to skip

    1....... :: item
    .0...... :: one byte
    .1...... :: 2 bytes
    ..****** ******** :: quantity
  *)
  let buf = Buffer.create 128 in
  List.iter merged ~f:(function
    | Skip x -> Buffer.add_char buf (x land 127 |> Char.of_int_exn)
    | Item qty when qty <= 63 -> Buffer.add_char buf (qty lor 128 |> Char.of_int_exn)
    | Item qty ->
      Buffer.add_char buf ((qty lsr 8) lor 192 |> Char.of_int_exn);
      Buffer.add_char buf (qty land 255 |> Char.of_int_exn));
  (* let debug =
       Buffer.contents buf |> String.concat_map ~sep:"," ~f:(fun c -> Char.to_int c |> Int.to_string)
     in *)
  Buffer.contents buf |> Base64.encode_string

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
          let left = (c land 63) lsl 8 in
          let right = buffer.[pos + 1] |> Char.to_int in
          let data = left lor right in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 2) (id + 1) (Map.add_exn acc ~key ~data)
        | c ->
          (* 1 byte *)
          let data = c land 63 in
          let key = Map.nth_exn all id |> fst in
          loop (pos + 1) (id + 1) (Map.add_exn acc ~key ~data)
      )
    in
    loop 0 0 Map.empty
  in
  match Base64.decode b64 with
  | Ok s -> Or_error.try_with (fun () -> process s (String.length s))
  | Error (`Msg s) -> Or_error.error_string s

let component (ingredients : (Recipes.Glossary.t * int) list Bonsai.Value.t) =
  let%sub clicker = Clicker.component 2.0 in
  return
  @@ let%map ingredients = ingredients
     and clicked, trigger_click = clicker in
     let handler _evt =
       let compressed = compress ingredients in
       let open Js_of_ocaml in
       let url =
         Url.Current.as_string
         |> Uri.of_string
         |> Fn.flip Uri.with_fragment (Some compressed)
         |> Uri.to_string
       in
       let promise = window##.navigator##.clipboard##writeText url in
       let _then = promise##_then (fun _ -> print_endline (sprintf "Copied '%s' to clipboard!" url)) in
       let _catch = promise##catch (fun err -> window##.console##error err) in
       trigger_click
     in
     if clicked
     then Node.h6 Attr.[ classes [ "d-inline-block"; "ms-2" ] ] [ Node.text "Copied!" ]
     else Icon.svg Clipboard ~container:Span Attr.[ class_ "ms-2"; on_click handler; style pointer ]
