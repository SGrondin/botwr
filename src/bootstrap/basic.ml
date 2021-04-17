open! Core_kernel
open! Bonsai_web
open! Vdom

let merge_attrs attrs = List.concat_no_order attrs |> Attrs.merge_classes_and_styles

let window = Js_of_ocaml.Js.Unsafe.global

let pctvh x : Css_gen.Length.t = `Vh (Percent.of_percentage x)

let pctvw x : Css_gen.Length.t = `Vw (Percent.of_percentage x)

let width_between w mw = Css_gen.(width (pctvw w) @> max_width (`Percent (Percent.of_percentage mw)))

let bold_if bold = Attr.class_ @@ if bold then "fw-bold" else "fw-normal"

let multiline_text str =
  String.split str ~on:'\n'
  |> List.filter_map ~f:(function
       | "" -> None
       | s -> Some (Node.p [] [ Node.text s ]))

let plural s = function
| [ _ ] -> s
| []
 |_ ->
  sprintf "%ss" s

let v_center content = [ Node.div Attr.[ class_ "v-center" ] content ]

let unselectable = Css_gen.(user_select `None)

let pointer = Css_gen.(create ~field:"cursor" ~value:"pointer")

let add_if cond x ll = if cond then x :: ll else ll

let add_fold ~f x ll = List.fold x ~init:ll ~f:(fun acc x -> f x :: acc)

let add_opt opt ~f ll = Option.value_map opt ~default:ll ~f:(fun x -> f x :: ll)

let add_opt_id opt ll = Option.value_map opt ~default:ll ~f:(fun x -> x :: ll)

let add_list data ~f ll =
  match data with
  | [] -> ll
  | _ -> f data :: ll

let or_none opt = Option.value ~default:Node.none opt

let or_blank ~f = function
| Some s -> Node.text (f s)
| None -> Node.text ""

let ( *> ) x f = Option.value_map ~default:(Node.div [] []) ~f x
