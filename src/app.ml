open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let inventory =
  Local_storage.parse_item "inventory" [%of_sexp: (Recipes.Glossary.t * int) list]
  |> Option.bind ~f:(fun ll -> Recipes.Glossary.Map.of_alist_or_error ll |> Or_error.ok)
  |> Option.value ~default:Recipes.Glossary.Map.empty

let application =
  print_endline "Made by SGrondin for his one true love ❤️";
  print_endline "🐫 Source code: https://github.com/SGrondin/botwr";
  let%sub backpack = Backpack.component ~inventory:(Bonsai.Value.return inventory) () in
  let%pattern_bind backpack, updates = backpack in
  let%sub kitchen = Kitchen.component ~updates () in
  return
  @@ let%map Backpack.{ total; items_node; show_all_node; jump_to_node; clear_all_node; ingredients } =
       backpack
     and kitchen = kitchen in
     let kitchen_node = Header.render ~clear_all_node ~total ingredients kitchen in
     Node.div
       Attr.[ class_ "m-2" ]
       [
         kitchen_node;
         Node.h3 Attr.[ class_ "mt-4"; style unselectable ] [ Node.text "Ingredients" ];
         Node.div Attr.[ class_ "my-3" ] [ show_all_node ];
         jump_to_node;
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
