open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap

let inventory1 =
  let open Recipes.Glossary in
  ( Recipes.Ingredient.Effect.Kind.Hearty,
    [
      Apple, 2;
      Goat_butter, 7;
      Tabantha_wheat, 2;
      Stamella_shroom, 24;
      Big_hearty_truffle, 2;
      Raw_gourmet_meat, 3;
      Goron_spice, 2;
    ]
    |> Map.of_alist_exn )

let inventory2 =
  let open Recipes.Glossary in
  ( Recipes.Ingredient.Effect.Kind.Energizing,
    [
      Apple, 2;
      Goat_butter, 7;
      Tabantha_wheat, 2;
      Stamella_shroom, 6;
      Staminoka_bass, 4;
      Courser_bee_honey, 4;
      Goron_spice, 2;
    ]
    |> Map.of_alist_exn )

let inventory3 =
  let open Recipes.Glossary in
  ( Recipes.Ingredient.Effect.Kind.Chilly,
    [
      Apple, 2;
      Goat_butter, 7;
      Tabantha_wheat, 2;
      Chillshroom, 6;
      Chillfin_trout, 4;
      Cool_safflina, 1;
      Hydromelon, 2;
    ]
    |> Map.of_alist_exn )

let inventory4 =
  let open Recipes.Glossary in
  ( Recipes.Ingredient.Effect.Kind.Mighty,
    [
      Apple, 2;
      Goat_butter, 7;
      Mighty_bananas, 3;
      Mighty_carp, 3;
      Razorshroom, 6;
      Mighty_porgy, 4;
      Razorclaw_crab, 2;
    ]
    |> Map.of_alist_exn )

let inventory5 =
  let open Recipes.Glossary in
  ( Recipes.Ingredient.Effect.Kind.Sneaky,
    [ Apple, 2; Goat_butter, 7; Sunset_firefly, 5; Monster_guts, 3 ] |> Map.of_alist_exn )

let application =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let _kind, inventory = inventory5 in
  let%sub backpack = Backpack.component ~inventory () in
  let%pattern_bind backpack, update_backpack, backpack_nodes = backpack in
  let backpack_is_empty = backpack >>| Recipes.Glossary.Map.is_empty in
  let%sub kitchen = Kitchen.component ~backpack_is_empty ~update_backpack ~max_hearts ~max_stamina () in
  return
  @@ let%map backpack = backpack
     and `Organize buttons_node, `Show_all show_all_node, `Jump_to jump_to, `Items items_node =
       backpack_nodes
     and ( `Kitchen kitchen,
           `Kind kind_buttons,
           `Meals meals_switch,
           `Elixirs elixirs_switch,
           update_kitchen ) =
       kitchen
     in
     let kitchen_node =
       let recipes_button =
         Node.div []
           [
             Node.button
               Attr.
                 [
                   type_ "button";
                   classes [ "btn"; "btn-primary"; "m-2" ];
                   on_click (fun _evt -> update_kitchen (Ready (Recipes.Glossary.Map.to_alist backpack)));
                 ]
               [ Node.text "Generate recipes" ];
           ]
       in
       Node.div []
         [
           Node.h3 [] [ Node.text "BOTW Cooking Optimizer" ];
           kind_buttons;
           meals_switch;
           elixirs_switch;
           recipes_button;
           kitchen;
         ]
     in

     Node.div
       Attr.[ class_ "m-2" ]
       [
         kitchen_node;
         Node.h3 Attr.[ class_ "mt-4" ] [ Node.text "Ingredients" ];
         jump_to;
         Node.div
           Attr.[ classes [ "mb-3"; "d-flex"; "align-items-center" ] ]
           (List.map ~f:(fun x -> Node.div Attr.[ class_ "mx-2" ] [ x ]) [ buttons_node; show_all_node ]);
         items_node;
       ]

let generate_and_save_graph () =
  let regex = Re.Perl.re ~opts:[ `Caseless ] "with-model-resetter_[0-9]*" |> Re.compile in
  let data =
    Bonsai.Private.to_dot application
    |> Re.replace regex ~all:true ~f:(fun g -> sprintf {|"%s"|} (Re.Group.get g 0))
  in
  Local_storage.set_item ~key:"graph" ~data

let _app =
  (* let _result = generate_and_save_graph () in *)
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"main" application
