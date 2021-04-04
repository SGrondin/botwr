open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap.Basic

let image_uri current_uri filename =
  let arr = Uri.path current_uri |> Filename.parts in
  List.fold_right arr
    ~init:([ "images"; filename ], 0)
    ~f:(fun x -> function
      | acc, (0 as i) -> acc, i + 1
      | acc, i -> x :: acc, i + 1)
  |> fst
  |> Filename.of_parts
  |> Uri.with_path current_uri

let render_switch ~update ~disabled ~id:id_ label is_on =
  let attrs =
    Attr.
      [
        classes [ "form-check-input"; "bg-secondary"; "border-secondary" ];
        type_ "checkbox";
        id id_;
        on_input (fun _ev _s -> update (not is_on));
      ]
    |> add_if is_on Attr.checked
    |> add_if disabled Attr.disabled
  in
  Node.div
    Attr.[ classes [ "form-check"; "form-switch"; "d-inline-flex" ] ]
    [
      Node.input attrs [];
      Node.label Attr.[ classes [ "form-check-label"; "ps-1"; "pe-2" ]; for_ id_ ] [ Node.text label ];
    ]
