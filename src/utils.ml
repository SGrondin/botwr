open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap.Basic

let render_switch ~id:id_ label ~handler is_on =
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
