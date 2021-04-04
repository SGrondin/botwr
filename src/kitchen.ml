open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

module Model = struct
  type t =
    | New
    | Ready     of (Recipes.Glossary.t * int) list
    | Completed
  [@@deriving sexp, equal]
end

let render ~update_backpack ~update_data ~max_hearts ~max_stamina (basic : Recipes.Optimize.Basic.t) =
  let open Option.Monad_infix in
  let open Recipes.Optimize.Basic in
  let current_uri = Uri.of_string Js_of_ocaml.Url.Current.as_string in
  let score_icon, score_text =
    let icon : Icon.t =
      match basic.score with
      | x when x < 25 -> Reception_0
      | x when x < 50 -> Reception_1
      | x when x < 100 -> Reception_2
      | x when x < 200 -> Reception_3
      | _ -> Reception_4
    in
    icon, sprintf "%d points" basic.score
  in
  let is_single_recipe =
    match basic.iterations with
    | [ _ ] -> true
    | _ -> false
  in
  let render_iteration { rarity; best } =
    let make_icon icon = Icon.svg icon ~width:1.5 ~height:1.5 ~container:Span [] in
    let make_duration sec =
      let minutes = Int.( /% ) sec 60 in
      let remainder = Int.( % ) sec 60 in
      match minutes, remainder with
      | 0, x -> Node.textf "%d seconds" x
      | x, 0 -> Node.textf "%d minutes" x
      | x, y -> Node.textf "%d minutes and %d seconds" x y
    in
    let icon_text icon text = Node.span [] [ icon >>| make_icon |> or_none; Node.text text ] in
    let recipe_rows =
      Recipes.Glossary.Map.fold_right best ~init:[] ~f:(fun ~key ~data acc ->
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
                    Attr.
                      [
                        style Css_gen.(height (`Em 2));
                        src
                          (Recipes.Glossary.to_filename key
                          |> Utils.image_uri current_uri
                          |> Uri.to_string
                          );
                      ]
                    [];
                ])
          :: acc)
      |> List.concat
    in
    let cooked = Recipes.Cooking.cook ~max_hearts ~max_stamina best in

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
      | Dubious -> "Dubious food", None, None
      | Failed msg ->
        print_endline msg;
        sprintf "Error: %s" msg, None, None
    in

    let hearts =
      meal >>| Recipes.Cooking.Meal.hearts >>= function
      | Nothing -> None
      | Restores x -> List.init x ~f:(const (make_icon Heart)) |> Node.span [] |> Option.return
      | Full_plus_bonus x ->
        List.init max_hearts ~f:(const (make_icon Heart)) @ List.init x ~f:(const (make_icon Hearty))
        |> Node.span []
        |> Option.return
    in

    let stamina =
      meal >>| Recipes.Cooking.Meal.stamina >>= function
      | Nothing -> None
      | Restores x ->
        let wheels = Int.( /% ) x 5 in
        let remainder =
          match Int.( % ) x 5 with
          | 1 -> [ make_icon Energizing1 ]
          | 2 -> [ make_icon Energizing2 ]
          | 3 -> [ make_icon Energizing3 ]
          | 4 -> [ make_icon Energizing4 ]
          | _ -> []
        in
        List.init wheels ~f:(const (make_icon Energizing)) @ remainder |> Node.span [] |> Option.return
      | Full_plus_bonus x ->
        List.init max_stamina ~f:(const (make_icon Enduring1))
        @ List.init x ~f:(const (make_icon Enduring2))
        |> Node.span []
        |> Option.return
    in

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
      ( ( opt >>| fun (s, icon, { potency; _ }) ->
          Node.span [] (Node.textf "%s %d " s potency :: List.init potency ~f:(const (make_icon icon))) ),
        opt >>| fun (_, _, { duration; _ }) -> make_duration duration )
    in

    let value_nodes =
      ( if is_single_recipe
      then []
      else [ Node.span Attr.[ class_ "px-2" ] []; icon_text (Some rarity_icon) rarity_text ]
      )
      |> List.cons (icon_text (Some score_icon) score_text)
      |> Node.span []
    in

    let make_row x y =
      Node.tr [] [ Node.td Attr.[ style Css_gen.(width (`Em 5)) ] [ Node.text x ]; Node.td [] [ y ] ]
    in

    let table =
      Node.table
        Attr.[ classes [ "table"; "mb-0" ] ]
        [
          Node.tbody []
            [
              make_row "Value" value_nodes;
              make_row "Type" (icon_text meal_icon meal_type);
              hearts >>| make_row "Hearts" |> or_none;
              stamina >>| make_row "Stamina" |> or_none;
              effect >>| make_row "Effect" |> or_none;
              duration >>| make_row "Duration" |> or_none;
            ];
        ]
    in

    let cook_button =
      let handler _evt =
        let evts =
          let _x = window##scrollTo 0 0 in
          update_data Model.Completed
          :: Recipes.Glossary.Map.fold_right best ~init:[] ~f:(fun ~key ~data acc ->
                 update_backpack (Backpack.Action.Decrement_by (key, data)) :: acc)
        in
        Event.Many evts
      in
      Node.div
        Attr.[ classes [ "v-center"; "align-items-center"; "my-2" ] ]
        [
          Node.button
            Attr.[ type_ "button"; on_click handler; classes [ "btn"; "btn-success"; "px-4" ] ]
            [ Node.text "Cook" ];
          Node.h6 Attr.[ class_ "fst-italic" ] [ Node.text "Removes ingredients from your inventory" ];
        ]
    in

    Node.div
      Attr.[ classes [ "border"; "border-1"; "row"; "row-cols-auto" ] ]
      [
        Node.div
          Attr.[ class_ "col"; style Css_gen.(min_width (`Percent (Percent.of_percentage 30.0))) ]
          [ table; cook_button ];
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
  | { count = 0; iterations = []; _ } -> Node.div [] []
  | { iterations = []; _ } ->
    Node.div []
      [
        Node.h6 [] [ Node.textf "(%.3f seconds)" seconds ];
        Node.div
          Attr.[ classes [ "d-flex"; "align-items-center" ] ]
          [
            Node.create "img" Attr.[ src (Utils.image_uri current_uri "dubious.png" |> Uri.to_string) ] [];
            Node.h6 [] [ Node.text "Nothing with the special effect you seek..." ];
          ];
      ]
  | { iterations; _ } ->
    let time_node =
      Node.h6 [] [ Node.textf "Best of %d possible recipes. (%.3f seconds)" basic.count seconds ]
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

let render_buttons ~update_kind selected_kind =
  Node.div
    Attr.
      [
        class_ "d-inline-block"; on_change (fun _evt s -> update_kind (String.Map.find button_choices s));
      ]
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

let component ~backpack_is_empty ~update_backpack ~max_hearts ~max_stamina ?kind () =
  let%sub component = Bonsai.state [%here] (module Model) ~default_model:New in
  let%sub meals = Bonsai.state [%here] (module Bool) ~default_model:true in
  let%sub elixirs = Bonsai.state [%here] (module Bool) ~default_model:true in
  let%sub kind = Bonsai.state_opt [%here] (module Recipes.Ingredient.Effect.Kind) ?default_model:kind in
  return
  @@ let%map data, update_data = component
     and backpack_is_empty = backpack_is_empty
     and update_backpack = update_backpack
     and meals, update_meals = meals
     and elixirs, update_elixirs = elixirs
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
       let instructions =
         let not_loaded =
           match data with
           | New
            |Ready [] ->
             false
           | _ -> true
         in
         [
           "Pick a bonus: Hearty, Tough, etc.", Option.is_some kind;
           "Add ingredients to your inventory.", not backpack_is_empty;
           "Click Generate Recipes!", not_loaded;
         ]
       in
       match data, kind with
       | Ready (_ :: _ as data), Some kind ->
         let optimized = Recipes.Optimize.Basic.run ~max_hearts ~max_stamina ~kind ~category data in
         render ~update_backpack ~update_data ~max_hearts ~max_stamina optimized
       | Completed, _ ->
         Node.div []
           [
             Node.div []
               [
                 Node.text "All done!";
                 Icon.svg Check_all ~width:1.5 ~height:1.5 ~container:Span Attr.[ class_ "text-success" ];
               ];
             Node.div []
               [ Node.text "Click Generate Recipes to continue with your remaining ingredients." ];
           ]
       | _ ->
         let nodes =
           List.mapi instructions ~f:(fun i (s, ok) ->
               let svg =
                 let icon, cl = if ok then Icon.Check_all, "text-success" else Icon.X, "text-danger" in
                 Icon.svg icon ~width:1.5 ~height:1.5 ~container:Span Attr.[ class_ cl ]
               in
               Node.li Attr.[ class_ "list-group-item" ] [ Node.textf "%d. %s" (i + 1) s; svg ])
         in
         Node.ul Attr.[ classes [ "list-group"; "list-group-flush" ] ] nodes
     in
     let buttons = render_buttons ~update_kind kind in
     let meals_switch =
       Utils.render_switch ~update:update_meals ~disabled:(not elixirs) ~id:"meals-switch" "Meals" meals
     in
     let elixirs_switch =
       Utils.render_switch ~update:update_elixirs ~disabled:(not meals) ~id:"elixirs-switch" "Elixirs"
         elixirs
     in
     `Kitchen kitchen_node, `Kind buttons, `Meals meals_switch, `Elixirs elixirs_switch, update_data
