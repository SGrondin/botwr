open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic
open Recipes

module Model = struct
  type t = int option [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Increment
    | Decrement
    | Decrement_by of int
    | Set          of int
    | Remove
  [@@deriving sexp]
end

let trigger_action map key action =
  Glossary.Map.find map key |> Option.value_map ~default:Event.Ignore ~f:(fun f -> f action)

let current_value ~model ~input =
  match model, input with
  | Some x, _
   |None, Some x ->
    x
  | None, None -> 0

let apply_action ~inject:_ ~schedule_event:_ input model : Action.t -> Model.t =
  let v = current_value ~model ~input in
  function
  | Increment -> Some (v + 1)
  | Decrement -> Some (max (v - 1) 0)
  | Decrement_by x -> Some (max (v - x) 0)
  | Set x -> Some (max x 0)
  | Remove -> Some 0

let quantity_node_id = "quantity-input"

let get_quantity_element () =
  let open Js_of_ocaml in
  Dom_html.getElementById_coerce quantity_node_id Dom_html.CoerceTo.input

let get_quantity () =
  let open Js_of_ocaml in
  get_quantity_element ()
  |> Option.bind ~f:(fun el -> Option.try_with (fun () -> el##.value |> Js.to_string |> Int.of_string))

let input_node ~update_selected ~update_state =
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      Option.iter (get_quantity_element ()) ~f:(fun el -> el##focus);
      Lwt.return_unit);

  let handler ev =
    let open Js_of_ocaml in
    match Js.Optdef.case ev##.key (fun () -> None) (fun jss -> Some (Js.to_string jss)) with
    | Some "Enter" ->
      let num = get_quantity () |> Option.value ~default:0 in
      Event.Many [ update_state (Action.Set num); update_selected None ]
    | Some "Escape" -> update_selected None
    | _ -> Event.Ignore
  in
  Node.input
    Attr.
      [
        type_ "tel";
        on_keydown handler;
        class_ "form-control";
        id quantity_node_id;
        style Css_gen.(width (`Em 4));
      ]
    []

let changes_quantity ~update_selected ~update_state is_selected item =
  Attr.
    [
      style pointer;
      style unselectable;
      class_ "has-handler";
      on_click (function
        | _ev when is_selected -> (
          match get_quantity () with
          | Some quantity -> Event.Many [ update_state (Action.Set quantity); update_selected None ]
          | None -> update_selected None
        )
        | _ev ->
          let evts = [ update_selected (Some item) ] in
          Event.Many evts);
    ]
  |> Attrs.merge_classes_and_styles

let svg ?(attrs = []) ~update_state x action =
  let attrs = merge_attrs [ attrs; Attr.[ on_click (fun _ev -> update_state action); style pointer ] ] in
  Icon.svg ~bold:true ~width:1.6 ~height:1.6 x attrs

let component ~inventory ~selected ~update_selected item =
  let is_selected =
    let%map item = item
    and selected = selected in
    [%equal: Glossary.t option] (Some item) selected
  in
  let input =
    let%map inventory = inventory
    and item = item in
    Glossary.Map.find inventory item
  in
  let%sub state =
    Bonsai.state_machine1 [%here] (module Model) (module Action) input ~default_model:None ~apply_action
  in
  return
  @@ let%map state, update_state = state
     and item = item
     and update_selected = update_selected
     and is_selected = is_selected
     and input = input in
     let changes_quantity = changes_quantity ~update_selected ~update_state is_selected item in
     let state = current_value ~model:state ~input in
     let node =
       let first_col =
         Node.div [] [ Node.create "img" (Attr.src (Glossary.to_img_src item) :: changes_quantity) [] ]
       in
       let second_col =
         let quantity =
           match is_selected with
           | true -> input_node ~update_state ~update_selected
           | false -> Node.div Attr.[ style unselectable ] [ Node.textf !"x%d" state ]
         in
         Node.div
           Attr.[ classes [ "d-flex"; "flex-column"; "justify-content-between"; "align-items-center" ] ]
           [ svg ~update_state Up Action.Increment; quantity; svg ~update_state Down Action.Decrement ]
       in
       let first_row =
         Node.div Attr.[ classes [ "d-flex"; "justify-content-around" ] ] [ first_col; second_col ]
       in
       let second_row =
         Node.div
           Attr.[ classes [ "d-flex"; "align-items-center" ] ]
           [
             svg ~update_state X Action.Remove;
             Node.span Attr.[ style unselectable ] [ Node.textf !"%{Glossary}" item ];
           ]
       in
       Node.div
         Attr.
           [
             style Css_gen.(width (`Em 11) @> height (`Em 11));
             classes [ "border"; "border-1"; "d-flex"; "flex-column"; "justify-content-around" ];
           ]
         [ first_row; second_row ]
     in
     (state, update_state, item), node
