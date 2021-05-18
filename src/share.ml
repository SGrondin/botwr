open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let prefix = "inventory-"

let component (ingredients : (Recipes.Glossary.t * int) list Bonsai.Value.t) =
  let%sub clicker = Clicker.component 2.0 in
  return
  @@ let%map ingredients = ingredients
     and clicked, trigger_click = clicker in
     let handler _evt =
       let compressed = Recipes.Compression.compress ingredients in
       let open Js_of_ocaml in
       let url =
         Url.Current.as_string
         |> Uri.of_string
         |> Fn.flip Uri.with_fragment (Some (sprintf "%s%s" prefix compressed))
         |> Uri.to_string
       in
       let promise = window##.navigator##.clipboard##writeText url in
       let _then = promise##_then (fun _ -> print_endline (sprintf "Copied '%s' to clipboard!" url)) in
       let _catch = promise##catch (fun err -> window##.console##error err) in
       trigger_click
     in
     if clicked
     then Node.h6 Attr.[ classes [ "d-inline-block"; "ms-2" ] ] [ Node.text "Copied!" ]
     else
       Icon.svg Folder_symlink ~container:Span ~raw_extra_classes:[ "mb-1" ]
         Attr.[ class_ "ms-2"; on_click handler; style pointer ]
