open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Recipes

let no_decoration = Attr.style Css_gen.(text_decoration ~line:[ `None ] ())

type item_folder = {
  keyed_nodes: (int * Node.t) list;
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
      | Sunny
      | Mighty
      | Sneaky
      | Spicy
      | Tough
      | Sticky
      | Glowing
      | Monster_parts
      | Special
    [@@deriving sexp, compare, enumerate]
  end

  module Map = Map.Make (Self)
  include Self

  let to_game : t -> Game.availability = function
  | Nothing
   |Chilly
   |Electro
   |Enduring
   |Energizing
   |Fireproof
   |Hasty
   |Hearty
   |Mighty
   |Sneaky
   |Spicy
   |Tough
   |Monster_parts
   |Special ->
    Both
  | Sunny
   |Sticky
   |Glowing ->
    TOTK

  let of_glossary : Glossary.t -> t = function
  | Monster_horn _
   |Monster_fang _
   |Monster_guts _ ->
    Monster_parts
  | Dragon_scales _
   |Dragon_claws _
   |Dragon_fangs _
   |Dragon_horns _
   |Star_fragment
   |Fairy ->
    Special
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
    | Sunny -> Sunny
    | Sticky -> Sticky
    | Glowing -> Glowing)

  let to_string = function
  | Nothing -> "No Effect"
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
  | Sunny -> "Sunny"
  | Sticky -> "Sticky"
  | Glowing -> "Glowing"
  | Monster_parts -> "Monster Parts"
  | Special -> "Special"

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
      | Sunny -> First Sunny
      | Sticky -> First Sticky
      | Glowing -> First Glowing
      | Monster_parts -> Second Glossary.(to_img_src (Monster_guts Bokoblin_guts))
      | Special -> Second Glossary.(to_img_src Fairy)
    in
    Either.value_map kind
      ~first:(fun kind -> Icon.svg (Icon.of_kind kind) ~width:w ~height:h ~container:Span [])
      ~second:(fun img ->
        Node.create "img" Attr.[ src img; style Css_gen.(width (`Rem w) @> height (`Rem h)) ] [])
end

let group ~inventory ~selected ~update_selected ~show_all ~game items =
  let%sub mapped =
    Bonsai.assoc
      (module String)
      items
      ~f:(fun _name data -> Card.component ~inventory ~selected ~update_selected data)
  in
  return
  @@ let%map mapped = mapped
     and show_all = show_all
     and game = game in
     String.Map.fold mapped ~init:{ keyed_nodes = []; total = 0; updates = []; ingredients = [] }
       ~f:(fun
            ~key:_
            ~data:((n, update, item), node)
            ({ keyed_nodes; total; updates; ingredients } as acc)
          ->
         let updates = (item, update) :: updates in
         match n with
         | 0 when not show_all -> { acc with updates }
         | _ when not (Game.is_in_game (Glossary.availability item) ~game) -> { acc with updates }
         | _ ->
           {
             keyed_nodes = (Glossary.(Map.find_exn ordered item), node) :: keyed_nodes;
             total = total + n;
             updates;
             ingredients = (item, n) :: ingredients;
           })

let render_group group nodes =
  let title_ = Group.to_string group in
  Node.div
    Attr.[ class_ "mb-4" ]
    [
      Node.h4 Attr.[ class_ "ms-3"; id title_ ] [ Node.text title_; Group.to_img_node group ];
      Node.div Attr.[ classes [ "row"; "row-cols-auto" ] ] nodes;
      Node.a
        Attr.[ href "#top"; no_decoration ]
        [ Icon.svg Arrow_up ~width:1.5 ~height:1.5 ~container:Span []; Node.text "Scroll to the top" ];
    ]

let jump_to_node ~game =
  let links =
    List.filter_map Group.all ~f:(function
      | k when Game.is_in_game ~game (Group.to_game k) ->
        [ Group.to_img_node k ]
        |> Node.a Attr.[ href (sprintf "#%s" (Group.to_string k)); no_decoration ]
        |> Option.return
      | _ -> None)
  in
  let message = Node.text "Jump to... " in
  Node.div Attr.[ class_ "my-3" ] (message :: links)

type state = {
  total: int;
  items_node: Node.t;
  show_all_node: Node.t;
  by_effect: bool;
  by_effect_node: Node.t;
  jump_to_node: Node.t;
  clear_all_node: Node.t;
  ingredients: (Glossary.t * int) list;
}

type group_folder = {
  nodes: Node.t list;
  keyed_nodes: (int * Node.t) list;
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
          | Some acc -> String.Map.add_exn acc ~key ~data:x))
  |> Bonsai.Value.return

let component ~game ~inventory () =
  let%sub show_all = Switch.component ~id:"show-all-switch" ~label:"Show All" true in
  let%pattern_bind show_all, render_show_all, update_show_all = show_all in
  let%sub by_effect = Switch.component ~id:"by-effect-switch" ~label:"Group by Effect" false in
  let%sub selected = Bonsai.state_opt [%here] (module Glossary) in
  let%pattern_bind selected, update_selected = selected in
  let%sub backpack =
    Bonsai.assoc
      (module Group.Map.Key)
      default_model
      ~f:(fun _key data -> group ~inventory ~selected ~update_selected ~show_all ~game data)
  in
  let backpack_changed =
    let ingredients =
      backpack >>| fun backpack ->
      Group.Map.fold backpack ~init:[] ~f:(fun ~key:_ ~data:{ ingredients; _ } acc ->
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
     and game = game
     and show_all = show_all
     and render_show_all = render_show_all
     and update_show_all = update_show_all
     and by_effect, render_by_effect, update_by_effect = by_effect
     and selected = selected
     and update_selected = update_selected
     and () = backpack_changed in
     let { nodes; keyed_nodes; ingredients; total; updates } =
       Group.Map.fold_right backpack
         ~init:{ nodes = []; keyed_nodes = []; total = 0; updates = Glossary.Map.empty; ingredients = [] }
         ~f:(fun ~key ~data acc ->
           let updates =
             List.fold data.updates ~init:acc.updates ~f:(fun acc (key, data) ->
                 Glossary.Map.set acc ~key ~data)
           in
           match data with
           | { total = 0; _ } when not show_all -> { acc with updates }
           | { total; ingredients; keyed_nodes; _ } ->
             let nodes, keyed_nodes =
               match by_effect with
               | true ->
                 let node = Int.Map.of_alist_exn keyed_nodes |> Int.Map.data |> render_group key in
                 node :: acc.nodes, []
               | false -> [], List.fold keyed_nodes ~init:acc.keyed_nodes ~f:(fun acc x -> x :: acc)
             in
             {
               nodes;
               keyed_nodes;
               total = total + acc.total;
               updates;
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
       match by_effect with
       | true -> Node.div Attr.[ on_click handler ] nodes
       | false ->
         Int.Map.of_alist_exn keyed_nodes
         |> Int.Map.data
         |> Node.div Attr.[ on_click handler; classes [ "row"; "row-cols-auto" ] ]
     in
     let show_all_node =
       let handler _evt = update_show_all (not show_all) in
       render_show_all ~handler
     in
     let by_effect_node =
       let handler _evt = update_by_effect (not by_effect) in
       render_by_effect ~handler
     in
     let clear_all_node =
       let handler _evt =
         let events =
           Glossary.Map.fold updates ~init:[] ~f:(fun ~key:_ ~data:update acc -> update Remove :: acc)
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
     let state =
       {
         total;
         items_node;
         show_all_node;
         by_effect;
         by_effect_node;
         jump_to_node = jump_to_node ~game;
         clear_all_node;
         ingredients;
       }
     in
     state, updates
