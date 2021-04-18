open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let inventory = Recipes.Glossary.Map.empty

let inventory =
  let open Recipes.Glossary in
  [ Endura_carrot, 2; Endura_shroom, 2 ] |> Map.of_alist_exn

let render_help (data : Kitchen.Model.t) kind total ~kind_buttons ~max_hearts_node ~max_stamina_node =
  let is_loaded =
    match data with
    | New
     |Completed ->
      true
    | Ready _
     |Loading ->
      false
  in
  let instructions =
    [
      "Set your maximum hearts containers.", None, max_hearts_node;
      "Set your maximum stamina containers.", None, max_stamina_node;
      "Pick a bonus.", Some (Option.is_some kind), kind_buttons;
      "Add ingredients to your inventory.", Some (total <> 0), Node.none;
      sprintf "Click %s!" Kitchen.button_label, Some (not is_loaded), Node.none;
    ]
  in
  List.mapi instructions ~f:(fun i (s, ok, node) ->
      let svg =
        Option.value_map ok ~default:Node.none ~f:(fun ok ->
            let icon, cl = if ok then Icon.Check_all, "text-success" else Icon.X, "text-danger" in
            Icon.svg icon ~width:1.5 ~height:1.5 ~container:Span Attr.[ class_ cl ])
      in
      Node.li
        Attr.[ class_ "list-group-item"; style unselectable ]
        [ Node.textf "%d. %s" (i + 1) s; svg; node ])
  |> Node.ul Attr.[ classes [ "list-group"; "list-group-flush" ] ]

let application =
  let%sub backpack = Backpack.component ~inventory:(Bonsai.Value.return inventory) () in
  let%pattern_bind backpack, updates = backpack in
  let%sub kitchen = Kitchen.component ~updates () in
  return
  @@ let%map Backpack.{ total; items_node; show_all_node; jump_to_node; ingredients } = backpack
     and ( data,
           update_kitchen,
           calculate,
           `Kitchen kitchen,
           `Kind (kind, kind_buttons),
           `Meals meals_switch,
           `Elixirs elixirs_switch,
           `Max_hearts max_hearts_node,
           `Max_stamina max_stamina_node ) =
       kitchen
     in
     let kitchen_node =
       let id_ = "hidden-btn" in
       let hidden =
         Node.button
           Attr.
             [
               class_ "d-none";
               id id_;
               type_ "button";
               on_click (fun _evt ->
                   match kind with
                   | Some kind ->
                     let optimized = calculate kind ingredients in
                     update_kitchen (Ready optimized)
                   | None -> update_kitchen New);
             ]
           []
       in
       let button =
         let cl =
           [ "btn"; "btn-primary"; "m-2" ] |> add_if ([%equal: Kitchen.Model.t] data Loading) "d-none"
         in
         Node.div []
           [
             hidden;
             Node.button
               Attr.
                 [
                   type_ "button";
                   classes cl;
                   on_click (fun _evt ->
                       match data with
                       | Ready _
                        |Completed
                        |New
                         when Option.is_some kind ->
                         Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
                             let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.1 in
                             Js_of_ocaml.Dom_html.getElementById_opt id_
                             |> Option.iter ~f:(fun el -> el##click);
                             Lwt.return_unit);
                         update_kitchen Loading
                       | _ -> Event.Ignore);
                 ]
               [ Node.text Kitchen.button_label ];
           ]
       in
       let help_node = render_help data kind total ~kind_buttons ~max_hearts_node ~max_stamina_node in
       Node.div []
         [
           Node.h3 Attr.[ id "top" ] [ Node.text "BOTW Cooking Optimizer" ];
           help_node;
           Node.div
             Attr.[ classes [ "d-flex"; "align-items-center" ] ]
             [ button; meals_switch; elixirs_switch ];
           kitchen;
         ]
     in
     Node.div
       Attr.[ class_ "m-2" ]
       [
         kitchen_node;
         Node.h3 Attr.[ class_ "mt-4"; style unselectable ] [ Node.text "Ingredients" ];
         Node.div Attr.[ class_ "my-3" ] [ show_all_node ];
         jump_to_node;
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
