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

let application =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let _kind, inventory = inventory4 in
  let%sub backpack = Backpack.component ~inventory () in
  let%sub kitchen = Kitchen.component ~max_hearts ~max_stamina ~factor:2 () in
  return
  @@ let%map ( (backpack, _update_backpack),
               `Organize buttons_node,
               `Show_all show_all_node,
               `Items items_node ) =
       backpack
     and `Kitchen kitchen, `Kind kind_buttons, update_kitchen = kitchen in
     let kitchen_node =
       match Recipes.Glossary.Map.is_empty backpack with
       | true -> Node.none
       | false ->
         let cook_button =
           Node.div []
             [
               Node.button
                 Attr.
                   [
                     type_ "button";
                     classes [ "btn"; "btn-primary"; "mx-2" ];
                     on_click (fun _evt -> update_kitchen (Recipes.Glossary.Map.to_alist backpack));
                   ]
                 [ Node.text "Cook!" ];
             ]
         in
         Node.div [] [ Node.h3 [] [ Node.text "Cooking" ]; kitchen; kind_buttons; cook_button ]
     in

     Node.div []
       [
         kitchen_node;
         Node.h3 Attr.[ class_ "mt-4" ] [ Node.text "Ingredients" ];
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
