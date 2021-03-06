open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let inventory =
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
    |> Option.bind ~f:(fun ll -> Recipes.Glossary.Map.of_alist_or_error ll |> Or_error.ok)
  in
  Option.first_some from_uri from_storage |> Option.value ~default:Recipes.Glossary.Map.empty

let application =
  print_endline "Made by SGrondin for his one true love ❤️";
  print_endline "🐫 Source code: https://github.com/SGrondin/botwr";
  let%sub backpack = Backpack.component ~inventory:(Bonsai.Value.return inventory) () in
  let%pattern_bind backpack, updates = backpack in
  let%sub kitchen = Kitchen.component ~updates () in
  let%sub share = Share.component ~updates (backpack >>| fun { ingredients; _ } -> ingredients) in
  return
  @@ let%map Backpack.
               {
                 total;
                 items_node;
                 show_all_node;
                 by_effect;
                 by_effect_node;
                 jump_to_node;
                 clear_all_node;
                 ingredients;
               } =
       backpack
     and kitchen = kitchen
     and share = share in
     let kitchen_node = Header.render ~clear_all_node ~total ingredients kitchen in

     Node.div
       Attr.[ class_ "m-2" ]
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
