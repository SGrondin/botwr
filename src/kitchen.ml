open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic
open Recipes

module Model = struct
  type t =
    | New
    | Loading
    | Ready     of Optimize.t
    | Completed
  [@@deriving sexp, equal]
end

let make_icon ?fill icon = Icon.svg icon ?fill ~width:1.5 ~height:1.5 ~container:Span []

let wrap_icon_list nodes = Node.div Attr.[ style Css_gen.(max_width (`Em 30) @> unselectable) ] nodes

let get_heart_nodes (Ingredient.Hearts.Quarters actual) =
  let full_hearts = Int.( /% ) actual 4 |> List.init ~f:(const (make_icon Heart)) in
  let remainder =
    match Int.( % ) actual 4 with
    | 1 -> [ make_icon Heart1 ]
    | 2 -> [ make_icon Heart2 ]
    | 3 -> [ make_icon Heart3 ]
    | _ -> []
  in
  let label =
    match actual // 4 with
    | 0.0 -> None
    | frac -> Some (Float.to_string_hum frac ~delimiter:',' ~decimals:2 ~strip_zero:true)
  in
  full_hearts @ remainder, label

let render_hearts max_hearts : Cooking.Hearts.t -> (string * Node.t) option = function
| Nothing -> None
| Restores (Quarters _ as x) ->
  let hearts, label = get_heart_nodes x in
  Some (sprintf "Hearts (%s)" (Option.value label ~default:"0"), wrap_icon_list hearts)
| Full_plus_bonus x ->
  let actual = min x (30 - max_hearts) in
  let node =
    List.init max_hearts ~f:(const (make_icon Heart)) @ List.init actual ~f:(const (make_icon Hearty))
    |> wrap_icon_list
  in
  Some (sprintf "Hearts (%d+%d)" max_hearts actual, node)
| Unglooms (x, (Quarters _ as q)) ->
  let unglooms = List.init x ~f:(const (make_icon Sunny)) in
  let hearts, hearts_label = get_heart_nodes q in
  let node = unglooms @ hearts |> wrap_icon_list in
  let label =
    match x, hearts_label with
    | _, Some s -> sprintf "Repairs %d+%s Hearts" x s
    | 1, None -> "Repairs 1 Heart"
    | _, None -> sprintf "Repairs %d Hearts" x
  in

  Some (label, node)

let render_stamina max_stamina : Cooking.Stamina.t -> (string * Node.t) option = function
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

let render ~updates ~update_data ~game ~max_hearts ~max_stamina (basic : Optimize.t) =
  let open Option.Monad_infix in
  let open Optimize in
  let render_iteration { rarity; score; tiebreaker = _; recipe } =
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
    let cooked = Cooking.cook recipe in

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

    let hearts = meal >>| Cooking.Meal.hearts >>= render_hearts max_hearts in
    let stamina = meal >>| Cooking.Meal.stamina >>= render_stamina max_stamina in

    let effect, duration =
      let opt =
        meal >>| Cooking.Meal.effect >>= function
        | Nothing -> None
        | Spicy x -> Some ("Spicy", Icon.Spicy, x)
        | Chilly x -> Some ("Chilly", Icon.Chilly, x)
        | Electro x -> Some ("Electro", Icon.Electro, x)
        | Fireproof x -> Some ("Fireproof", Icon.Fireproof, x)
        | Hasty x -> Some ("Hasty", Icon.Hasty, x)
        | Rapid x -> Some ("Rapid", Icon.Rapid, x)
        | Sticky x -> Some ("Sticky", Icon.Sticky, x)
        | Sneaky x -> Some ("Sneaky", Icon.Sneaky, x)
        | Mighty x -> Some ("Mighty", Icon.Mighty, x)
        | Tough x -> Some ("Tough", Icon.Tough, x)
        | Bright x -> Some ("Bright", Icon.Bright, x)
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
              match Cooking.Effect.max_potency effect with
              | true -> None
              | false -> Some (Node.text "+1 power"))
            | Duration -> (
              match Cooking.Effect.duration effect with
              | x when x >= 1800 -> None
              | _ -> Some (Node.text "+5:00")))
        in
        let pct = 100 / List.length random_effects in
        List.map choices ~f:(fun node -> Node.span [] [ node; Node.textf " ( %d %% )" pct ])
        |> List.intersperse ~sep:(Node.div [] [ Node.text " or " ])
        |> Node.span []
        |> Option.some_if (List.is_empty choices |> not)
    in

    let score_node = if Env.is_dev then Node.none else Node.text (sprintf "Score: %d" score) in

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
                    [
                      Node.span [] [ Node.text meal_type; meal_icon >>| make_icon |> or_none; score_node ];
                    ];
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
          :: Ingredient.Map.fold_right recipe ~init:[] ~f:(fun ~key ~data acc ->
                 Card.trigger_action updates key.item (Card.Action.Decrement_by data) :: acc)
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
      let order = Glossary.game_ordered game in
      Ingredient.Map.to_alist recipe
      |> List.sort ~compare:(fun (x, _) (y, _) ->
             [%compare: int] (Glossary.Map.find_exn order y.item) (Glossary.Map.find_exn order x.item))
      |> List.fold ~init:[ [ cook_button ] ] ~f:(fun acc ({ item; _ }, data) ->
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
                     Node.div [] [ Node.textf !"%{Glossary}" item ];
                     Node.create "img"
                       Attr.[ style Css_gen.(height (`Em 2)); src (Blob.get item game) ]
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
            Node.create "img" Attr.[ src (force Blob.dubious) ] [];
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
  Ingredient.Effect.Kind.
    [|
      "Hearts", Neutral;
      "Chilly", Chilly;
      "Electro", Electro;
      "Energizing", Energizing;
      "Enduring", Enduring;
      "Fireproof", Fireproof;
      "Hasty", Hasty;
      "Rapid", Rapid;
      "Sticky", Sticky;
      "Hearty", Hearty;
      "Sunny", Sunny;
      "Mighty", Mighty;
      "Sneaky", Sneaky;
      "Spicy", Spicy;
      "Tough", Tough;
      "Bright", Bright;
    |]

let render_buttons ~game ~update selected_kind =
  let handler _evt s =
    update (Array.find_map button_choices ~f:(fun (k, v) -> Option.some_if String.(k = s) v))
  in
  Node.div
    Attr.[ class_ "my-2"; on_change handler; style Css_gen.(max_width (`Em 37)) ]
    (Array.fold_right button_choices ~init:[] ~f:(fun (label, kind) -> function
       | acc when not (Game.is_in_game (Ingredient.Effect.Kind.availability kind) ~game) -> acc
       | acc ->
         let id_ = sprintf "kind-choice-%s" label in
         let attrs =
           Attr.
             [ class_ "form-check-input"; type_ "radio"; name "kind-radio-buttons"; id id_; value label ]
           |> add_if ([%equal: Ingredient.Effect.Kind.t option] selected_kind (Some kind)) Attr.checked
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

module SunnyAlgo = struct
  type t =
    | Full
    | Gloomy
  [@@deriving sexp, equal, enumerate]

  let to_string = function
  | Full -> "Heal to full"
  | Gloomy -> "Heal gloomy hearts only"
end

type state = {
  data: Model.t;
  update_data: Model.t -> Event.t;
  calculate: Ingredient.Effect.Kind.t -> (Glossary.t * int) list -> Optimize.t;
  kitchen_node: Node.t;
  kind: Ingredient.Effect.Kind.t option;
  kind_buttons: Node.t;
  meals_switch: Node.t;
  elixirs_switch: Node.t;
  use_special_switch: Node.t;
  max_hearts: int;
  max_hearts_node: Node.t;
  max_stamina: int;
  max_stamina_node: Node.t;
}

let button_label = "Cook"

let component ~game ~init_max_hearts ~init_max_stamina ~updates ?kind () =
  let%sub component = Bonsai.state [%here] (module Model) ~default_model:New in
  let%pattern_bind _, update_kitchen = component in
  let%sub max_hearts =
    Stepper.component "max_hearts" ?start_value:init_max_hearts 5 ~min_value:3
      ~max_value:(Bonsai.Value.return 30) ~update_kitchen New ~render:(fun x ->
        List.init x ~f:(const (make_icon Heart)) |> wrap_icon_list)
  in

  let%sub gloomy_hearts =
    Stepper.component "gloomy_hearts" 1 ~min_value:1
      ~max_value:(max_hearts >>| fun (hearts, _) -> hearts - 1)
      ~update_kitchen New
      ~render:(fun x -> List.init x ~f:(const (make_icon ~fill:Icon.fill_grey Sunny)) |> wrap_icon_list)
  in
  let%sub max_stamina =
    Stepper.component "max_stamina" ?start_value:init_max_stamina 5 ~min_value:5
      ~max_value:(Bonsai.Value.return 15) ~update_kitchen New ~render:(fun potency ->
        Cooking.Stamina.Restores { potency; wasted = 0 }
        |> render_stamina 15
        |> Option.value_map ~f:snd ~default:Node.none)
  in
  let%sub meals = Switch.component ~id:"meals-switch" ~label:"Meals" true in
  let%sub elixirs = Switch.component ~id:"elixirs-switch" ~label:"Elixirs" true in
  let%sub use_special =
    Switch.component ~id:"special-switch" ~label:"Use dragon parts, fairies, stars" false
  in
  let%sub algo =
    Choices.component "algo" [%here]
      (module Cooking.Algo)
      Balanced ~aria:"Radio button to select optimization algorithm"
  in
  let%sub sunny_algo =
    Choices.component "sunny-algo" [%here]
      (module SunnyAlgo)
      Full ~aria:"Radio button to select gloomy healing optimization algorithm"
  in
  let%sub kind = Bonsai.state_opt [%here] (module Ingredient.Effect.Kind) ?default_model:kind in
  return
  @@ let%map data, update_data = component
     and game = game
     and max_hearts, max_hearts_node = max_hearts
     and gloomy_hearts, gloomy_hearts_node = gloomy_hearts
     and max_stamina, max_stamina_node = max_stamina
     and updates = updates
     and meals, render_meals, update_meals = meals
     and elixirs, render_elixirs, update_elixirs = elixirs
     and use_special, render_use_special, update_use_special = use_special
     and algo, algo_node = algo
     and sunny_algo, sunny_algo_node = sunny_algo
     and kind, update_kind = kind in
     let category : Glossary.Category.t =
       match meals, elixirs with
       | true, true
        |false, false ->
         Any
       | true, false -> Meals
       | false, true -> Elixirs
     in
     let kitchen_node =
       match data with
       | Ready optimized -> render ~updates ~update_data ~game ~max_hearts ~max_stamina optimized
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
     let kind_buttons =
       let update x = Event.Many [ update_kind x; update_data New ] in
       let kind_with_duration =
         Option.filter kind ~f:(function
           | Nothing
            |Neutral
            |Hearty
            |Sunny
            |Energizing
            |Enduring ->
             false
           | Spicy
            |Chilly
            |Electro
            |Fireproof
            |Hasty
            |Rapid
            |Sticky
            |Sneaky
            |Mighty
            |Tough
            |Bright ->
             true)
       in
       let kind_with_gloomy_hearts =
         Option.filter kind ~f:(function
           | Sunny -> true
           | _ -> false)
       in
       let nodes =
         []
         |> add_opt kind_with_duration ~f:(const algo_node)
         |> add_opt kind_with_gloomy_hearts ~f:(fun _ ->
                Node.div [] [ Node.text "Gloomy hearts"; gloomy_hearts_node; sunny_algo_node ])
         |> List.cons @@ render_buttons ~game ~update kind
       in
       Node.div [] nodes
     in
     let calculate (kind : Ingredient.Effect.Kind.t) ingredients =
       let algo : Cooking.Algo.t =
         match kind, sunny_algo with
         | Sunny, Gloomy -> Balanced
         | Sunny, Full -> Maximize
         | _ -> algo
       in
       let settings =
         Optimize.{ game; max_hearts; max_stamina; gloomy_hearts; algo; kind; category; use_special }
       in
       Optimize.run settings ingredients
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
       max_hearts;
       max_hearts_node;
       max_stamina;
       max_stamina_node;
     }
