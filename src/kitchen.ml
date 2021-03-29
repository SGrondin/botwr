open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

module Model = struct
  type t = (Recipes.Glossary.t * int) list [@@deriving sexp, equal]
end

let render ~max_hearts ~max_stamina (basic : Recipes.Optimize.Basic.t) =
  let open Option.Monad_infix in
  let open Recipes.Optimize.Basic in
  let current_uri = Uri.of_string Js_of_ocaml.Url.Current.as_string in
  let render_iteration i { score; count; best } =
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
                        src (Recipes.Glossary.to_img_uri current_uri key |> Uri.to_string);
                      ]
                    [];
                ])
          :: acc)
      |> List.concat
    in
    let cooked = Recipes.Cooking.cook ~max_hearts ~max_stamina best in
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

    let make_row x y =
      Node.tr [] [ Node.td Attr.[ style Css_gen.(width (`Em 5)) ] [ Node.text x ]; Node.td [] [ y ] ]
    in
    let instructions =
      let s = sprintf "Best of %d recipes with a score of %d" count score in
      if i = 0 then s else sprintf "After you've made the above recipe, cook this one. %s" s
    in
    Node.div
      Attr.[ classes [ "border"; "border-1"; "row"; "row-cols-auto" ] ]
      [
        Node.h6 Attr.[ class_ "col-3" ] [ Node.text instructions ];
        Node.div
          Attr.[ class_ "col-4" ]
          [
            Node.table
              Attr.[ class_ "table" ]
              [
                Node.tbody []
                  [
                    make_row "Type" (icon_text meal_icon meal_type);
                    hearts >>| make_row "Hearts" |> or_none;
                    stamina >>| make_row "Stamina" |> or_none;
                    effect >>| make_row "Effect" |> or_none;
                    duration >>| make_row "Duration" |> or_none;
                  ];
              ];
          ];
        Node.div
          Attr.[ class_ "col-auto" ]
          [
            Node.ul
              Attr.[ classes [ "list-group"; "list-group-flush" ]; style Css_gen.(width (`Em 20)) ]
              recipe_rows;
          ];
      ]
  in
  match basic.iterations with
  | [] -> Node.div [] []
  | iterations ->
    let time_node =
      Node.h6 []
        [ Node.textf "Took: %.3f seconds" (Float.round_significant ~significant_digits:3 basic.duration) ]
    in
    Node.div [] (time_node :: List.mapi iterations ~f:render_iteration)

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
    Attr.[ on_change (fun _evt s -> update_kind (String.Map.find button_choices s)) ]
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

let component ~max_hearts ~max_stamina ~factor ?kind () =
  let%sub component = Bonsai.state [%here] (module Model) ~default_model:[] in
  let%sub kind = Bonsai.state_opt [%here] (module Recipes.Ingredient.Effect.Kind) ?default_model:kind in
  return
  @@ let%map data, update_data = component
     and kind, update_kind = kind in
     let kitchen_node =
       Option.map kind ~f:(fun kind ->
           let optimized = Recipes.Optimize.Basic.run ~max_hearts ~max_stamina ~factor ~kind data in
           render ~max_hearts ~max_stamina optimized)
       |> or_none
     in
     let buttons = render_buttons ~update_kind kind in
     `Kitchen kitchen_node, `Kind buttons, update_data
