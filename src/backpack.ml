open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Recipes

let no_decoration = Attr.style Css_gen.(text_decoration ~line:[ `None ] ())

type item_folder = {
  nodes: Node.t list;
  total: int;
  updates: (Glossary.t * (Card.Action.t -> Ui_event.t)) list;
  ingredients: (Glossary.t * int) list;
}

let category ~inventory ~selected ~update_selected ~show_all kind items =
  let%sub mapped =
    Bonsai.assoc
      (module String)
      items
      ~f:(fun _name data -> Card.component ~inventory ~selected ~update_selected data)
  in
  return
  @@ let%map kind = kind
     and mapped = mapped
     and show_all = show_all in
     let folded =
       String.Map.fold_right mapped ~init:{ nodes = []; total = 0; updates = []; ingredients = [] }
         ~f:(fun ~key:_ ~data ({ nodes; total; updates; ingredients } as acc) ->
           match data with
           | (0, _, _), _ when not show_all -> acc
           | (x, update, item), node ->
             {
               nodes = node :: nodes;
               total = total + x;
               updates = (item, update) :: updates;
               ingredients = (item, x) :: ingredients;
             })
     in
     match folded with
     | { total = 0; _ } when not show_all -> folded, Node.none
     | { nodes; _ } ->
       let title_ = Ingredient.Effect.Kind.to_string kind in
       let node =
         Node.div
           Attr.[ class_ "mb-4" ]
           [
             Node.h4
               Attr.[ class_ "ms-3"; id title_ ]
               [
                 Node.text title_; Icon.svg (Icon.of_kind kind) ~width:2.0 ~height:2.0 ~container:Span [];
               ];
             Node.div Attr.[ classes [ "row"; "row-cols-auto" ] ] nodes;
             Node.a
               Attr.[ href "#top"; no_decoration ]
               [
                 Icon.svg Arrow_up ~width:1.5 ~height:1.5 ~container:Span [];
                 Node.text "Scroll to the top";
               ];
           ]
       in
       folded, node

let jump_to_node =
  let links =
    List.filter_map Ingredient.Effect.Kind.all ~f:(function
      | Nothing -> None
      | k ->
        Node.a
          Attr.[ href (sprintf "#%s" (Ingredient.Effect.Kind.to_string k)); no_decoration ]
          [ Icon.svg (Icon.of_kind k) ~width:2.0 ~height:2.0 ~container:Span Attr.[ class_ "ps-2" ] ]
        |> Option.return)
  in
  let message = Node.text "Jump to... " in
  Node.div Attr.[ class_ "my-3" ] (message :: links)

type state = {
  total: int;
  items_node: Node.t;
  show_all_node: Node.t;
  jump_to_node: Node.t;
  ingredients: (Glossary.t * int) list;
}

type category_folder = {
  nodes: Node.t list;
  total: int;
  updates: (Card.Action.t -> Ui_event.t) Glossary.Map.t;
  ingredients: (Glossary.t * int) list;
}

let default_model =
  let module KM = Ingredient.Effect.Kind.Map in
  List.fold Glossary.all ~init:KM.empty ~f:(fun acc x ->
      KM.update acc (Glossary.to_kind x) ~f:(fun existing ->
          let key = Glossary.to_string x in
          match existing with
          | None -> String.Map.singleton key x
          | Some acc -> String.Map.set acc ~key ~data:x))
  |> Bonsai.Value.return

let component ~inventory () =
  let%sub show_all = Bonsai.state [%here] (module Bool) ~default_model:true in
  let%pattern_bind show_all, update_show_all = show_all in
  let%sub selected = Bonsai.state_opt [%here] (module Glossary) in
  let%pattern_bind selected, update_selected = selected in
  let%sub backpack =
    Bonsai.assoc
      (module Ingredient.Effect.Kind.Map.Key)
      default_model
      ~f:(fun key data -> category ~inventory ~selected ~update_selected ~show_all key data)
  in
  return
  @@ let%map backpack = backpack
     and show_all = show_all
     and update_show_all = update_show_all
     and selected = selected
     and update_selected = update_selected in
     let { nodes; ingredients; total; updates } =
       Ingredient.Effect.Kind.Map.fold_right backpack
         ~init:{ nodes = []; total = 0; updates = Glossary.Map.empty; ingredients = [] }
         ~f:(fun ~key:_ ~data acc ->
           match data with
           | { total = 0; _ }, _ when not show_all -> acc
           | { updates; total; ingredients; _ }, node ->
             {
               nodes = node :: acc.nodes;
               total = total + acc.total;
               updates =
                 List.fold updates ~init:acc.updates ~f:(fun acc (key, data) ->
                     Glossary.Map.set acc ~key ~data);
               ingredients = ingredients @ acc.ingredients;
             })
     in
     let items_node =
       let handler evt =
         let cl =
           let open Js_of_ocaml in
           evt##.target |> Js.Opt.to_option |> Option.map ~f:(fun el -> el##.className |> Js.to_string)
         in
         let evts =
           Option.both selected (Card.get_quantity ())
           |> Option.value_map ~default:[] ~f:(fun (item, x) ->
                  [ Card.trigger_action updates item (Card.Action.Set x) ])
         in
         match cl with
         | Some "has-handler" -> Event.Many evts
         | _ -> Event.Many (update_selected None :: evts)
       in
       Node.div Attr.[ on_click handler ] nodes
     in
     let show_all_node =
       Utils.render_switch ~update:update_show_all ~disabled:false ~id:"show-all-checkbox" "Show All"
         show_all
     in
     { total; items_node; show_all_node; jump_to_node; ingredients }, updates
