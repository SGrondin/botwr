open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let initial =
  let from_uri =
    let open Js_of_ocaml in
    Url.Current.as_string
    |> Uri.of_string
    |> Uri.fragment
    |> Option.find_map ~f:(String.chop_prefix ~prefix:Share.prefix)
    |> Option.bind ~f:(fun s ->
           match Recipes.Compression.decompress s with
           | Ok x ->
             Url.Current.set_fragment "";
             Some x
           | Error err ->
             print_endline (sprintf !"%{Error.to_string_hum}" err);
             None)
  in
  let from_storage =
    Local_storage.parse_item "inventory" [%of_sexp: (Recipes.Glossary.t * int) list]
    |> Option.bind ~f:(fun ll ->
           Recipes.Glossary.Map.of_alist_or_error ll
           |> Or_error.map ~f:(fun items -> Recipes.Compression.{ empty with items })
           |> Or_error.ok)
  in
  Option.first_some from_uri from_storage |> Option.value ~default:Recipes.Compression.empty

let application =
  print_endline "Made by SGrondin for his one true love ❤️";
  print_endline "🐫 Source code: https://github.com/SGrondin/botwr";
  let%sub game =
    Choices.component "game" [%here]
      (module Recipes.Game)
      TOTK ~aria:"Radio button to select which game you're playing"
      ~node_mapper:(fun ~data node ->
        match data with
        | TOTK ->
          Node.div []
            [
              node;
              Node.div [] [ Node.text "Work in progress!" ];
              Node.div []
                [ Node.text "The game is new, not all new monster parts have been tested and added yet" ];
            ]
        | BOTW -> Node.div [] [ node ])
  in
  let%pattern_bind game, game_node = game in
  let%sub backpack = Backpack.component ~game ~inventory:(Bonsai.Value.return initial.items) () in
  let%pattern_bind backpack, updates = backpack in
  let%sub kitchen =
    Kitchen.component ~game ~init_max_hearts:initial.max_hearts ~init_max_stamina:initial.max_stamina
      ~updates ()
  in
  let%pattern_bind { max_hearts; max_stamina; update_data = update_kitchen; _ } = kitchen in
  let%sub set_all = Set_all.component ~updates ~update_kitchen in
  let%sub share =
    Share.component ~updates ~max_hearts ~max_stamina (backpack >>| fun { ingredients; _ } -> ingredients)
  in
  return
  @@ let%map game_node = game_node
     and Backpack.
           { total; items_node; show_all_node; by_effect; by_effect_node; jump_to_node; ingredients } =
       backpack
     and kitchen = kitchen
     and set_all_node = set_all
     and share = share in
     let kitchen_node = Header.render ~game_node ~set_all_node ~total ingredients kitchen in

     let handler ev =
       let open Js_of_ocaml in
       (match Js.Optdef.case ev##.key (fun () -> None) (fun jss -> Some (Js.to_string jss)) with
       | Some "c" ->
         Js_of_ocaml.Dom_html.getElementById_opt Header.cook_button_id
         |> Option.iter ~f:(fun el -> el##click)
       | _ -> ());
       Event.Ignore
     in

     let cl = Attr.[ class_ "m-2" ] |> add_if Env.is_dev Attr.(on_keydown handler) in
     Node.div cl
       [
         kitchen_node;
         Node.div []
           [
             Node.h3
               Attr.[ classes [ "mt-4"; "d-inline-block" ]; style unselectable ]
               [ Node.text "Ingredients" ];
           ];
         share;
         Node.div Attr.[ class_ "my-3" ] [ show_all_node; by_effect_node ];
         (if by_effect then jump_to_node else Node.none);
         items_node;
         Node.div []
           [
             Node.a
               Attr.
                 [
                   href "https://github.com/SGrondin/botwr";
                   create "target" "blank";
                   style Css_gen.(text_decoration ~line:[ `None ] ());
                 ]
               [ Node.create "small" [] [ Node.text "🐫 Source code 🐫" ] ];
           ];
       ]

let generate_and_save_graph computation =
  let regex = Re.Perl.re ~opts:[ `Caseless ] "with-model-resetter_[0-9]*" |> Re.compile in
  let data =
    Bonsai.Private.to_dot computation
    |> Re.replace regex ~all:true ~f:(fun g -> sprintf {|"%s"|} (Re.Group.get g 0))
  in
  Local_storage.set_item ~key:"graph" ~data

let _app =
  (* let _result = generate_and_save_graph () in *)
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"main" application
