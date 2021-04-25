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

module Group = struct
  module Self = struct
    type t =
      | Nothing
      | Chilly
      | Electro
      | Enduring
      | Energizing
      | Fireproof
      | Hasty
      | Hearty
      | Mighty
      | Sneaky
      | Spicy
      | Tough
      | Monster_parts
    [@@deriving sexp, compare, enumerate]
  end

  module Map = Map.Make (Self)
  include Self

  let of_glossary : Glossary.t -> t = function
  | Monster_horn _
   |Monster_fang _
   |Monster_guts _ ->
    Monster_parts
  | x -> (
    match Glossary.to_kind x with
    | Nothing
     |Neutral ->
      Nothing
    | Chilly -> Chilly
    | Electro -> Electro
    | Enduring -> Enduring
    | Energizing -> Energizing
    | Fireproof -> Fireproof
    | Hasty -> Hasty
    | Hearty -> Hearty
    | Mighty -> Mighty
    | Sneaky -> Sneaky
    | Spicy -> Spicy
    | Tough -> Tough
  )

  let to_string = function
  | Nothing -> "None"
  | Chilly -> "Chilly"
  | Electro -> "Electro"
  | Enduring -> "Enduring"
  | Energizing -> "Energizing"
  | Fireproof -> "Fireproof"
  | Hasty -> "Hasty"
  | Hearty -> "Hearty"
  | Mighty -> "Mighty"
  | Sneaky -> "Sneaky"
  | Spicy -> "Spicy"
  | Tough -> "Tough"
  | Monster_parts -> "Monster Parts"

  let to_img_node ?width:(w = 2.0) ?height:(h = 2.0) group =
    let kind : (Recipes.Ingredient.Effect.Kind.t, string) Either.t =
      match group with
      | Nothing -> First Nothing
      | Chilly -> First Chilly
      | Electro -> First Electro
      | Enduring -> First Enduring
      | Energizing -> First Energizing
      | Fireproof -> First Fireproof
      | Hasty -> First Hasty
      | Hearty -> First Hearty
      | Mighty -> First Mighty
      | Sneaky -> First Sneaky
      | Spicy -> First Spicy
      | Tough -> First Tough
      | Monster_parts -> Second Glossary.(to_img_src (Monster_guts Bokoblin_guts))
    in
    Either.value_map kind
      ~first:(fun kind -> Icon.svg (Icon.of_kind kind) ~width:w ~height:h ~container:Span [])
      ~second:(fun img ->
        Node.create "img" Attr.[ src img; style Css_gen.(width (`Rem w) @> height (`Rem h)) ] [])
end

let group ~inventory ~selected ~update_selected ~show_all group items =
  let%sub mapped =
    Bonsai.assoc
      (module String)
      items
      ~f:(fun _name data -> Card.component ~inventory ~selected ~update_selected data)
  in
  return
  @@ let%map group = group
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
       let title_ = Group.to_string group in
       let node =
         Node.div
           Attr.[ class_ "mb-4" ]
           [
             Node.h4 Attr.[ class_ "ms-3"; id title_ ] [ Node.text title_; Group.to_img_node group ];
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
    List.map Group.all ~f:(fun k ->
        Node.a Attr.[ href (sprintf "#%s" (Group.to_string k)); no_decoration ] [ Group.to_img_node k ])
  in
  let message = Node.text "Jump to... " in
  Node.div Attr.[ class_ "my-3" ] (message :: links)

type state = {
  total: int;
  items_node: Node.t;
  show_all_node: Node.t;
  jump_to_node: Node.t;
  clear_all_node: Node.t;
  ingredients: (Glossary.t * int) list;
}

type group_folder = {
  nodes: Node.t list;
  total: int;
  updates: (Card.Action.t -> Ui_event.t) Glossary.Map.t;
  ingredients: (Glossary.t * int) list;
}

let default_model =
  List.fold Glossary.all ~init:Group.Map.empty ~f:(fun acc x ->
      Group.Map.update acc (Group.of_glossary x) ~f:(fun existing ->
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
      (module Group.Map.Key)
      default_model
      ~f:(fun key data -> group ~inventory ~selected ~update_selected ~show_all key data)
  in
  let backpack_changed =
    let ingredients =
      backpack >>| fun backpack ->
      Group.Map.fold backpack ~init:[] ~f:(fun ~key:_ ~data:({ ingredients; _ }, _) acc ->
          List.fold ingredients ~init:acc ~f:(fun acc -> function
            | _, 0 -> acc
            | x -> x :: acc))
    in
    Bonsai.Value.cutoff ~equal:[%equal: (Glossary.t * int) list] ingredients >>| fun items ->
    Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
        let _res =
          Local_storage.set_item ~key:"inventory"
            ~data:(sprintf !"%{sexp: (Glossary.t * int) list}" items)
        in
        Lwt.return_unit)
  in
  return
  @@ let%map backpack = backpack
     and show_all = show_all
     and update_show_all = update_show_all
     and selected = selected
     and update_selected = update_selected
     and () = backpack_changed in
     let { nodes; ingredients; total; updates } =
       Group.Map.fold_right backpack
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
       let handler _evt = update_show_all (not show_all) in
       Utils.render_switch ~handler ~id:"show-all-checkbox" "Show All" show_all
     in
     let clear_all_node =
       let handler _evt =
         let events =
           Glossary.Map.fold updates ~init:[] ~f:(fun ~key:_ ~data acc -> data Remove :: acc)
         in
         Event.Many events
       in
       Node.div []
         [
           Node.button
             Attr.[ type_ "button"; classes [ "btn"; "btn-outline-danger"; "btn-sm" ]; on_click handler ]
             [ Node.text "Clear all " ];
         ]
     in
     { total; items_node; show_all_node; jump_to_node; clear_all_node; ingredients }, updates
