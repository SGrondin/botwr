open! Core_kernel
open! Bonsai_web
open! Vdom
open Js_of_ocaml

let ls = Js_of_ocaml.Dom_html.window##.localStorage |> Js_of_ocaml.Js.Optdef.to_option

let keys () =
  Option.value_map ls ~default:[] ~f:(fun ls ->
      Js.object_keys ls
      |> Js.to_array
      |> Array.fold_right ~init:[] ~f:(fun jss acc -> Js.to_string jss :: acc))

let keys_array () =
  Option.value_map ls ~default:[||] ~f:(fun ls ->
      Js.object_keys ls |> Js.to_array |> Array.map ~f:Js.to_string)

let get_item key =
  let open Option.Let_syntax in
  let%bind ls = ls in
  let%map item = ls##getItem (Js.string key) |> Js.Opt.to_option in
  Js.to_string item

let parse_item name of_sexp =
  get_item name |> Option.bind ~f:(fun s -> Option.try_with (fun () -> Sexp.of_string_conv_exn s of_sexp))

let or_failed = Result.of_option ~error:"Could not access Local Storage"

let set_item ~key ~data =
  Option.map ls ~f:(fun ls -> ls##setItem (Js.string key) (Js.string data)) |> or_failed

let remove_item key = Option.map ls ~f:(fun ls -> ls##removeItem (Js.string key)) |> or_failed

let clear () = Option.iter ls ~f:(fun ls -> ls##clear)
