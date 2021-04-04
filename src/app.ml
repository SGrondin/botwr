open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap

let application =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let%sub backpack = Backpack.component ~inventory:Recipes.Glossary.Map.empty () in
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
