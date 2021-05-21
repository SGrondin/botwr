open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

module Model = struct
  type t =
    | New
    | Loading
    | Ready     of Recipes.Optimize.t
    | Completed
  [@@deriving sexp, equal]
end

let make_icon icon = Icon.svg icon ~width:1.5 ~height:1.5 ~container:Span []

let wrap_icon_list nodes = Node.div Attr.[ style Css_gen.(max_width (`Em 30) @> unselectable) ] nodes

let render_hearts max_hearts : Recipes.Cooking.Hearts.t -> (string * Node.t) option = function
| Nothing -> None
| Restores x ->
  let actual = min x max_hearts in
  let node = List.init actual ~f:(const (make_icon Heart)) |> wrap_icon_list in
  Some (sprintf "Hearts (%d)" actual, node)
| Full_plus_bonus x ->
  let actual = min x (30 - max_hearts) in
  let node =
    List.init max_hearts ~f:(const (make_icon Heart)) @ List.init actual ~f:(const (make_icon Hearty))
    |> wrap_icon_list
  in
  Some (sprintf "Hearts (%d + %d)" max_hearts actual, node)

let render_stamina max_stamina : Recipes.Cooking.Stamina.t -> (string * Node.t) option = function
| Nothing -> None
| Restores { potency; wasted = _ } ->
  let actual = min potency max_stamina in
  let wheels = Int.( /% ) actual 5 |> List.init ~f:(const (make_icon Energizing)) in
  let remainder =
    match Int.( % ) actual 5 with
    | 1 -> [ make_icon Energizing1 ]
    | 2 -> [ make_icon Energizing2 ]
    | 3 -> [ make_icon Energizing3 ]
    | 4 -> [ make_icon Energizing4 ]
    | _ -> []
  in
  let node = wheels @ remainder |> wrap_icon_list in
  Some ("Stamina", node)
| Full_plus_bonus { potency; wasted = _ } ->
  let green_wheels = Int.( /% ) max_stamina 5 |> List.init ~f:(const (make_icon Energizing)) in
  let green_remainder =
    match Int.( % ) max_stamina 5 with
    | 1 -> [ make_icon Energizing1 ]
    | 2 -> [ make_icon Energizing2 ]
    | 3 -> [ make_icon Energizing3 ]
    | 4 -> [ make_icon Energizing4 ]
    | _ -> []
  in
  let actual = min potency (25 - max_stamina) in
  let yellow_wheels = Int.( /% ) actual 5 |> List.init ~f:(const (make_icon Enduring)) in
  let yellow_remainder =
    match Int.( % ) actual 5 with
    | 1 -> [ make_icon Enduring1 ]
    | 2 -> [ make_icon Enduring2 ]
    | 3 -> [ make_icon Enduring3 ]
    | 4 -> [ make_icon Enduring4 ]
    | _ -> []
  in
  let node =
    List.concat [ green_wheels; green_remainder; yellow_wheels; yellow_remainder ] |> wrap_icon_list
  in
  Some ("Stamina", node)

let render ~updates ~update_data ~max_hearts ~max_stamina (basic : Recipes.Optimize.t) =
  let open Option.Monad_infix in
  let open Recipes.Optimize in
  let render_iteration { rarity; score = _; tiebreaker = _; recipe } =
    let make_duration sec =
      let actual = min sec 1800 in
      let minutes = Int.( /% ) actual 60 in
      let remainder = Int.( % ) actual 60 in
      match minutes, remainder with
      | 0, x -> Node.textf "%d seconds" x
      | x, 0 -> Node.textf "%d minutes" x
      | x, y -> Node.textf "%d minutes and %d seconds" x y
    in
    let icon_text icon text = Node.span [] [ icon >>| make_icon |> or_none; Node.text text ] in
    let cooked = Recipes.Cooking.cook recipe in

    let rarity_icon, rarity_text =
      let open Float in
      match rarity with
      | x when x < 0.05 -> Icon.Reception_4, "Abundant"
      | x when x < 0.15 -> Icon.Reception_3, "Plenty"
      | x when x < 0.45 -> Icon.Reception_2, "Enough"
      | x when x < 1.3 -> Icon.Reception_1, "Limited"
      | _ -> Icon.Reception_0, "Rare"
    in

    let meal_type, meal_icon, meal =
      match cooked with
      | Food m -> "Meal", Some Icon.Meal, Some m
      | Elixir m -> "Elixir", Some Icon.Elixir, Some m
      | Tonic m -> "Tonic", Some Icon.Elixir, Some m
      | Dubious -> "Dubious food", None, None
      | Failed msg ->
        print_endline msg;
        sprintf "Error: %s" msg, None, None
    in

    let hearts = meal >>| Recipes.Cooking.Meal.hearts >>= render_hearts max_hearts in
    let stamina = meal >>| Recipes.Cooking.Meal.stamina >>= render_stamina max_stamina in

    let effect, duration =
      let opt =
        meal >>| Recipes.Cooking.Meal.effect >>= function
        | Nothing -> None
        | Spicy x -> Some ("Spicy", Icon.Spicy, x)
        | Chilly x -> Some ("Chilly", Icon.Chilly, x)
        | Electro x -> Some ("Electro", Icon.Electro, x)
        | Fireproof x -> Some ("Fireproof", Icon.Fireproof, x)
        | Hasty x -> Some ("Hasty", Icon.Hasty, x)
        | Sneaky x -> Some ("Sneaky", Icon.Sneaky, x)
        | Mighty x -> Some ("Mighty", Icon.Mighty, x)
        | Tough x -> Some ("Tough", Icon.Tough, x)
      in
      let effect =
        opt >>| fun (s, icon, { potency; _ }) ->
        Node.span [] (Node.textf "%s %d " s potency :: List.init potency ~f:(const (make_icon icon)))
      in
      let duration = opt >>| fun (_, _, { duration; _ }) -> make_duration duration in
      effect, duration
    in

    let random_effects =
      meal >>= function
      | { random_effects = []; _ } -> None
      | { random_effects; effect; _ } ->
        let choices =
          List.filter_map random_effects ~f:(function
            | Red_hearts ->
              Some (Node.span [] [ Node.text "+"; make_icon Heart; make_icon Heart; make_icon Heart ])
            | Yellow_hearts -> None
            | Green_wheels -> Some (Node.span [] [ Node.text "+"; make_icon Energizing2 ])
            | Yellow_wheels -> Some (Node.span [] [ Node.text "+"; make_icon Enduring2 ])
            | Potency -> (
              match Recipes.Cooking.Effect.potency effect with
              | 3 -> None
              | _ -> Some (Node.text "+1 power")
            )
            | Duration -> (
              match Recipes.Cooking.Effect.duration effect with
              | x when x >= 1800 -> None
              | _ -> Some (Node.text "+5:00")
            ))
        in
        let pct = 100 / List.length random_effects in
        List.map choices ~f:(fun node -> Node.span [] [ node; Node.textf " ( %d %% )" pct ])
        |> List.intersperse ~sep:(Node.div [] [ Node.text " or " ])
        |> Node.span []
        |> Option.some_if (List.is_empty choices |> not)
    in

    let col_width x = Attr.[ style Css_gen.(width (`Em x)) ] in
    let make_row x y = Node.tr [] [ Node.td (col_width 6) [ Node.text x ]; Node.td [] [ y ] ] in

    let table =
      Node.table
        Attr.[ classes [ "table"; "mb-0" ] ]
        [
          Node.tbody []
            [
              Node.tr []
                [
                  Node.td (col_width 6)
                    [ Node.span [] [ Node.text meal_type; meal_icon >>| make_icon |> or_none ] ];
                  Node.td [] [ icon_text (Some rarity_icon) rarity_text ];
                ];
              hearts >>| Tuple2.uncurry make_row |> or_none;
              stamina >>| Tuple2.uncurry make_row |> or_none;
              effect >>| make_row "Effect" |> or_none;
              duration >>| make_row "Duration" |> or_none;
              random_effects >>| make_row "Random Effects" |> or_none;
            ];
        ]
    in

    let cook_button =
      let handler _evt =
        let evts =
          let _x = window##scrollTo 0 0 in
          update_data Model.Completed
          :: Recipes.Glossary.Map.fold_right recipe ~init:[] ~f:(fun ~key ~data acc ->
                 Card.trigger_action updates key (Card.Action.Decrement_by data) :: acc)
        in
        Event.Many evts
      in
      Node.div
        Attr.[ classes [ "v-center"; "align-items-center"; "my-2" ] ]
        [
          Node.button
            Attr.[ type_ "button"; on_click handler; classes [ "btn"; "btn-success"; "px-4" ] ]
            [ Node.text "I cooked it" ];
        ]
    in

    let recipe_rows =
      Recipes.Glossary.Map.fold_right recipe ~init:[ [ cook_button ] ] ~f:(fun ~key ~data acc ->
          List.init data ~f:(fun _ ->
              Node.li
                Attr.
                  [
                    classes
                      [
                        "list-group-item";
                        "p-1";
                        "d-flex";
                        "align-items-center";
                        "justify-content-between";
                      ];
                  ]
                [
                  Node.div [] [ Node.textf !"%{Recipes.Glossary}" key ];
                  Node.create "img"
                    Attr.[ style Css_gen.(height (`Em 2)); src (Recipes.Glossary.to_img_src key) ]
                    [];
                ])
          :: acc)
      |> List.concat
    in

    Node.div
      Attr.[ classes [ "border"; "border-1"; "row"; "row-cols-auto" ] ]
      [
        Node.div
          Attr.
            [
              class_ "col";
              style Css_gen.(min_width (`Percent (Percent.of_percentage 30.0)) @> width (`Em 25));
            ]
          [ table ];
        Node.div
          Attr.[ class_ "col-auto" ]
          [
            Node.ul
              Attr.[ classes [ "list-group"; "list-group-flush" ]; style Css_gen.(width (`Em 20)) ]
              recipe_rows;
          ];
      ]
  in
  let seconds = Float.round_significant ~significant_digits:3 basic.duration in
  match basic with
  | { count = 0; iterations = []; _ } ->
    Node.div [] [ Node.text "Nothing to cook! Try a different bonus or ingredients." ]
  | { iterations = []; _ } ->
    Node.div []
      [
        Node.h6 [] [ Node.textf "(%.3f seconds)" seconds ];
        Node.div
          Attr.[ classes [ "d-flex"; "align-items-center" ] ]
          [
            Node.create "img" Attr.[ src (force Recipes.Blob.dubious) ] [];
            Node.h6 [] [ Node.text "Nothing with the special effect you seek..." ];
          ];
      ]
  | { iterations; _ } ->
    let time_node =
      Node.h6 []
        [
          Node.textf "Best of %s recipes. (%.3f seconds)"
            (Int.to_string_hum ~delimiter:',' basic.count)
            seconds;
        ]
    in
    Node.div [] (time_node :: List.map iterations ~f:render_iteration)

let button_choices =
  String.Map.of_alist_exn
    Recipes.Ingredient.Effect.Kind.
      [
        "Hearty", Hearty;
        "Energizing", Energizing;
        "Enduring", Enduring;
        "Spicy", Spicy;
        "Chilly", Chilly;
        "Electro", Electro;
        "Fireproof", Fireproof;
        "Hasty", Hasty;
        "Sneaky", Sneaky;
        "Mighty", Mighty;
        "Tough", Tough;
      ]

let render_buttons ~update selected_kind =
  let handler _evt s = update (String.Map.find button_choices s) in
  Node.div
    Attr.[ class_ "my-2"; on_change handler ]
    (String.Map.fold_right button_choices ~init:[] ~f:(fun ~key:label ~data:kind acc ->
         let id_ = sprintf "kind-choice-%s" label in
         let attrs =
           Attr.
             [ class_ "form-check-input"; type_ "radio"; name "kind-radio-buttons"; id id_; value label ]
           |> add_if
                ([%equal: Recipes.Ingredient.Effect.Kind.t option] selected_kind (Some kind))
                Attr.checked
         in
         let node =
           Node.div
             Attr.[ classes [ "form-check"; "form-check-inline"; "d-inline-flex"; "align-items-center" ] ]
             [
               Node.input attrs [];
               Node.label
                 Attr.[ class_ "form-check-label"; for_ id_ ]
                 [ Icon.svg ~width:1.5 ~height:1.5 (Icon.of_kind kind) ~container:Span [] ];
             ]
         in
         node :: acc))

type state = {
  data: Model.t;
  update_data: Model.t -> Ui_event.t;
  calculate: Recipes.Ingredient.Effect.Kind.t -> (Recipes.Glossary.t * int) list -> Recipes.Optimize.t;
  kitchen_node: Node.t;
  kind: Recipes.Ingredient.Effect.Kind.t option;
  kind_buttons: Node.t;
  meals_switch: Node.t;
  elixirs_switch: Node.t;
  use_special_switch: Node.t;
  max_hearts_node: Node.t;
  max_stamina_node: Node.t;
}

let button_label = "Cook"

let component ~updates ?kind () =
  let%sub component = Bonsai.state [%here] (module Model) ~default_model:New in
  let%pattern_bind _, update_kitchen = component in
  let%sub max_hearts =
    Stepper.component "max_hearts" 28 ~max_value:30 ~update_kitchen New ~render:(fun x ->
        Recipes.Cooking.Hearts.Restores x
        |> render_hearts 30
        |> Option.value_map ~f:snd ~default:Node.none)
  in
  let%sub max_stamina =
    Stepper.component "max_stamina" 15 ~max_value:15 ~update_kitchen New ~render:(fun potency ->
        Recipes.Cooking.Stamina.Restores { potency; wasted = 0 }
        |> render_stamina 15
        |> Option.value_map ~f:snd ~default:Node.none)
  in
  let%sub meals = Switch.component ~id:"meals-switch" ~label:"Meals" true in
  let%sub elixirs = Switch.component ~id:"elixirs-switch" ~label:"Elixirs" true in
  let%sub use_special =
    Switch.component ~id:"special-switch" ~label:"Use dragon parts, fairies, stars" false
  in
  let%sub algo = Bonsai.state [%here] (module Recipes.Cooking.Algo) ~default_model:Balanced in
  let%sub kind = Bonsai.state_opt [%here] (module Recipes.Ingredient.Effect.Kind) ?default_model:kind in
  return
  @@ let%map data, update_data = component
     and max_hearts, max_hearts_node = max_hearts
     and max_stamina, max_stamina_node = max_stamina
     and updates = updates
     and meals, render_meals, update_meals = meals
     and elixirs, render_elixirs, update_elixirs = elixirs
     and use_special, render_use_special, update_use_special = use_special
     and algo, update_algo = algo
     and kind, update_kind = kind in
     let category : Recipes.Glossary.Category.t =
       match meals, elixirs with
       | true, true
        |false, false ->
         Any
       | true, false -> Meals
       | false, true -> Elixirs
     in
     let kitchen_node =
       match data with
       | Ready optimized -> render ~updates ~update_data ~max_hearts ~max_stamina optimized
       | Completed ->
         Node.div []
           [
             Node.div []
               [
                 Node.text "All done!";
                 Icon.svg Check_all ~width:1.5 ~height:1.5 ~container:Span Attr.[ class_ "text-success" ];
               ];
             Node.div []
               [
                 Node.textf "Click on the %s button to continue with your remaining ingredients."
                   button_label;
               ];
           ]
       | Loading ->
         Node.div
           Attr.[ classes [ "spinner-border"; "text-info"; "m-3" ]; create "role" "status" ]
           [ Node.span Attr.[ class_ "visually-hidden" ] [ Node.text "Loading..." ] ]
       | New -> Node.none
     in
     let meals_switch =
       let handler _evt =
         let events = [ update_meals (not meals); update_data New ] in
         match meals, elixirs with
         | false, true
          |true, true ->
           Event.Many events
         | _ -> Event.Many (update_elixirs (not elixirs) :: events)
       in
       render_meals ~handler
     in
     let elixirs_switch =
       let handler _evt =
         let events = [ update_elixirs (not elixirs); update_data New ] in
         match elixirs, meals with
         | false, true
          |true, true ->
           Event.Many events
         | _ -> Event.Many (update_meals (not meals) :: events)
       in
       render_elixirs ~handler
     in
     let use_special_switch =
       let handler _evt = Event.Many [ update_use_special (not use_special); update_data New ] in
       render_use_special ~handler
     in
     let algo_node =
       let make_choice ~css_left ~css_right ~is_checked ~algo text =
         let handler _evt s =
           update_algo (Sexp.of_string_conv_exn s [%of_sexp: Recipes.Cooking.Algo.t])
         in
         let text_value = sprintf !"%{sexp: Recipes.Cooking.Algo.t}" algo in
         let id_ = sprintf "algo-radio-%s" text_value in
         let input_attrs =
           Attr.
             [
               classes [ "form-check-input"; "mt-0" ];
               type_ "radio";
               name "algo-radio";
               id id_;
               value text_value;
               on_change handler;
               create "aria-label" "Radio button to select optimization algorithm";
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
       Node.div []
         [
           make_choice
             ~css_left:Css_gen.(create ~field:"border-bottom-left-radius" ~value:"0")
             ~css_right:Css_gen.(create ~field:"border-bottom-right-radius" ~value:"0")
             ~is_checked:true ~algo:Balanced "Max effect + Balanced duration";
           make_choice
             ~css_left:Css_gen.(create ~field:"border-top-left-radius" ~value:"0")
             ~css_right:Css_gen.(create ~field:"border-top-right-radius" ~value:"0")
             ~is_checked:false ~algo:Maximize "Max effect + Max duration";
         ]
     in
     let kind_buttons =
       let update x = Event.Many [ update_kind x; update_data New ] in
       let kind_with_duration =
         Option.filter kind ~f:(function
           | Nothing
            |Neutral
            |Hearty
            |Energizing
            |Enduring ->
             false
           | Spicy
            |Chilly
            |Electro
            |Fireproof
            |Hasty
            |Sneaky
            |Mighty
            |Tough ->
             true)
       in
       let nodes =
         [] |> add_opt kind_with_duration ~f:(const algo_node) |> List.cons @@ render_buttons ~update kind
       in
       Node.div [] nodes
     in
     let calculate kind ingredients =
       let settings = Recipes.Optimize.{ max_hearts; max_stamina; algo; kind; category; use_special } in
       Recipes.Optimize.run settings ingredients
     in
     {
       data;
       update_data;
       calculate;
       kitchen_node;
       kind;
       kind_buttons;
       meals_switch;
       elixirs_switch;
       use_special_switch;
       max_hearts_node;
       max_stamina_node;
     }
