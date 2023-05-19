open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

module type S = sig
  type t [@@deriving sexp, equal, enumerate]

  val to_string : t -> string
end

let component (type a) name here (m : (module S with type t = a)) ?node_mapper default ~aria =
  let module M = (val m) in
  let m_arrow = (module M : Bonsai_web.Bonsai.Arrow.Model with type t = a) in
  let default_model = Local_storage.parse_item name [%of_sexp: M.t] |> Option.value ~default in
  let%sub component = Bonsai.state here m_arrow ~default_model in
  return
  @@ let%map data, update_data = component in

     let node =
       let make_choice ~css_left ~css_right ~is_checked ~opt text =
         let handler _evt s =
           let updated = Sexp.of_string_conv_exn s [%of_sexp: M.t] in
           let _res = Local_storage.set_item ~key:name ~data:(sprintf !"%{sexp: M.t}" updated) in
           update_data updated
         in
         let text_value = sprintf !"%{sexp: M.t}" opt in
         let id_ = sprintf "%s-radio-%s" name text_value in
         let input_attrs =
           let s = name in
           Attr.
             [
               classes [ "form-check-input"; "mt-0" ];
               type_ "radio";
               name (sprintf "%s-radio" s);
               id id_;
               value text_value;
               on_change handler;
               create "aria-label" aria;
             ]
           |> add_if is_checked Attr.checked
         in
         Node.div
           Attr.[ class_ "input-group" ]
           [
             Node.div Attr.[ class_ "input-group-text"; style css_left ] [ Node.input input_attrs [] ];
             Node.div
               Attr.
                 [
                   classes [ "input-group-text"; "bg-white" ]; style Css_gen.(css_right @> width (`Em 16));
                 ]
               [ Node.label Attr.[ for_ id_ ] [ Node.text text ] ];
           ]
       in
       let last_option = List.length M.all - 1 in
       Node.div []
         (List.mapi M.all ~f:(fun i opt ->
              let field_left, field_right =
                match i with
                | 0 -> "border-bottom-left-radius", "border-bottom-right-radius"
                | i when i = last_option -> "border-top-left-radius", "border-top-right-radius"
                | _ -> "border-radius", "border-radius"
              in
              make_choice
                ~css_left:Css_gen.(create ~field:field_left ~value:"0")
                ~css_right:Css_gen.(create ~field:field_right ~value:"0")
                ~is_checked:(M.equal opt default_model) ~opt (M.to_string opt)))
     in
     data, Option.value_map node_mapper ~default:node ~f:(fun f -> f ~data node)
