open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let render ~id:id_ label ~handler is_on =
  let attrs =
    Attr.
      [
        classes [ "form-check-input"; "bg-secondary"; "border-secondary" ];
        type_ "checkbox";
        id id_;
        style unclickable;
      ]
    |> add_if is_on Attr.checked
  in
  Node.div
    Attr.[ classes [ "form-check"; "form-switch"; "d-inline-flex" ]; on_click handler ]
    [
      Node.input attrs [];
      Node.label
        Attr.
          [
            style Css_gen.(unclickable @> unselectable);
            classes [ "form-check-label"; "ps-1"; "pe-2" ];
            for_ id_;
          ]
        [ Node.text label ];
    ]

let initial name = Local_storage.parse_item name [%of_sexp: bool]

let component ~id ~label default =
  let default_model = initial id |> Option.value ~default in
  let%sub state = Bonsai.state [%here] (module Bool) ~default_model in
  return
  @@ let%map state, update = state in
     let render = render ~id label state in
     let update b =
       let _res = Local_storage.set_item ~key:id ~data:(sprintf !"%{sexp: bool}" b) in
       update b
     in
     state, render, update
