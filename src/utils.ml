open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap.Basic

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
      Node.label
        Attr.[ style unselectable; classes [ "form-check-label"; "ps-1"; "pe-2" ]; for_ id_ ]
        [ Node.text label ];
    ]
