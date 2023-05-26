open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let prefix = "inv-"

type inventory_save = (Recipes.Items.t * int) list [@@deriving sexp_of]

type inventory_load = int Recipes.Glossary.Map.t [@@deriving of_sexp]

let copy_link ~max_hearts ~max_stamina ingredients =
  let compressed = Recipes.Compression.compress ingredients ~max_hearts ~max_stamina in
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
  ()

module Visibility = struct
  type t =
    | Nothing
    | Save
    | Restore
  [@@deriving sexp, compare, equal, enumerate]
end

module Status = struct
  type t =
    | Success of string
    | Danger  of string
  [@@deriving sexp, equal]
end

let save_component ~update_status_msg ~update_visibility
   (ingredients : (Recipes.Glossary.t * int) list Bonsai.Value.t) =
  let%sub name = Bonsai.state_opt [%here] (module String) in
  let%sub overwriting = Bonsai.state [%here] (module Bool) ~default_model:false in
  return
  @@ let%map name, update_name = name
     and overwriting, update_overwriting = overwriting
     and update_status_msg = update_status_msg
     and update_visibility = update_visibility
     and ingredients = ingredients in
     let textbox_node =
       let handler _evt s =
         let ow =
           sprintf "%s%s" prefix s |> Array.mem (Local_storage.keys_array ()) ~equal:[%equal: string]
         in
         Event.Many [ update_name (Some s); update_status_msg None; update_overwriting ow ]
       in
       let cl =
         [ "form-control"; "mb-2" ]
         |> add_if (Option.value_map name ~default:false ~f:String.is_empty) "is-invalid"
       in
       Node.input
         Attr.
           [
             on_input handler;
             type_ "text";
             placeholder "Give it a name...";
             classes cl;
             style Css_gen.(max_width (`Em 18));
           ]
         []
     in
     let save_node =
       let handler _evt =
         match name with
         | Some ""
          |None ->
           Event.Ignore
         | Some name -> (
           let key = sprintf "%s%s" prefix name in
           let data =
             List.filter ingredients ~f:(fun (_, n) -> n > 0) |> sprintf !"%{sexp#mach: inventory_save}"
           in
           match Local_storage.set_item ~key ~data with
           | Ok () ->
             Event.Many
               [
                 update_overwriting false;
                 update_status_msg (Some (Status.Success (sprintf "Saved %s!" name)));
                 update_visibility Visibility.Nothing;
               ]
           | Error msg ->
             print_endline (sprintf "Error writing to local storage: %s" msg);
             update_status_msg (Some (Status.Danger "Could not save due to a browser error")))
       in
       let text, cl =
         match overwriting with
         | false -> "Save", [ "btn"; "btn-outline-primary"; "me-2" ]
         | true -> "Overwrite", [ "btn"; "btn-outline-danger"; "me-2" ]
       in
       let attrs =
         Attr.[ on_click handler; classes cl ]
         |> add_if (Option.value_map name ~default:true ~f:String.is_empty) Attr.disabled
       in
       Node.button attrs [ Node.text text ]
     in
     let cancel_node =
       let handler _evt =
         Event.Many [ update_name None; update_overwriting false; update_visibility Nothing ]
       in
       Node.button Attr.[ on_click handler; classes [ "btn"; "btn-secondary" ] ] [ Node.text "Cancel" ]
     in
     Node.div []
       [
         Node.div Attr.[ class_ "mb-2" ] [ Node.text "Save Inventory" ];
         textbox_node;
         Node.div [] [ save_node; cancel_node ];
       ]

let restore_component ~update_visibility ~update_status_msg updates =
  let%sub selected = Bonsai.state_opt [%here] (module String) in
  return
  @@ let%map updates = updates
     and update_visibility = update_visibility
     and update_status_msg = update_status_msg
     and selected, update_selected = selected in
     let li =
       Local_storage.keys ()
       |> List.filter_map ~f:(fun save ->
              match String.chop_prefix ~prefix save with
              | Some chopped ->
                let active = [%equal: string option] selected (Some save) in
                let handler _evt = update_selected (if active then None else Some save) in
                let cl = [ "list-group-item" ] |> add_if active "active" in
                let attrs =
                  Attr.[ classes cl; style pointer; on_click handler ]
                  |> add_if active (Attr.create "aria-current" "true")
                in
                Some (Node.li attrs [ Node.text chopped ])
              | _ -> None)
     in
     let ul =
       match li with
       | [] -> Node.div Attr.[ class_ "mb-2" ] [ Node.text "No saved inventories" ]
       | ll ->
         Node.div []
           [
             Node.div Attr.[ class_ "mb-2" ] [ Node.text "Load Inventory" ];
             Node.ul Attr.[ classes [ "list-group"; "mb-2" ] ] ll;
           ]
     in
     let restore_node =
       let handler _evt =
         Option.bind selected ~f:Local_storage.get_item |> function
         | None -> Event.Ignore
         | Some raw -> (
           match Result.try_with (fun () -> Sexp.of_string_conv_exn raw [%of_sexp: inventory_load]) with
           | Ok map ->
             Recipes.Glossary.Map.fold2 map updates ~init:[] ~f:(fun ~key:_ ~data acc ->
                 match data with
                 | `Both (qty, update) -> update (Card.Action.Set qty) :: acc
                 | `Left _ -> acc
                 | `Right update -> update Card.Action.Remove :: acc)
             |> List.cons @@ update_visibility Visibility.Nothing
             |> List.cons @@ update_selected None
             |> List.cons @@ update_status_msg (Some (Status.Success "Done!"))
             |> Event.Many
           | Error err -> update_status_msg (Some (Danger (sprintf !"An error occured (%{Exn})" err))))
       in
       let attrs, text =
         match selected with
         | Some _ -> Attr.[ classes [ "btn"; "btn-primary"; "me-2" ] ], "Load"
         | None -> Attr.[ classes [ "btn"; "btn-outline-primary"; "me-2" ]; disabled ], "Pick one"
       in
       let attrs = Attr.on_click handler :: Attr.style Css_gen.(width (`Em 7)) :: attrs in
       Node.button attrs [ Node.text text ]
     in
     let delete_node =
       let handler _evt =
         Option.iter selected ~f:(fun x ->
             let _res = Local_storage.remove_item x in
             ());
         update_selected None
       in
       let base = [ Attr.style Css_gen.(height (`Px 35)); Attr.on_click handler ] in
       match selected with
       | Some _ ->
         let attrs =
           merge_attrs [ base; Attr.[ classes [ "btn"; "btn-danger"; "text-white"; "me-2" ] ] ]
         in
         Node.button attrs [ Icon.svg X_square [] ]
       | None ->
         let attrs =
           merge_attrs [ base; Attr.[ classes [ "btn"; "btn-outline-danger"; "me-2" ]; disabled ] ]
         in
         Node.button attrs [ Icon.svg X_square_fill [] ]
     in
     let cancel_node =
       let handler _evt = Event.Many [ update_selected None; update_visibility Visibility.Nothing ] in
       Node.button Attr.[ on_click handler; classes [ "btn"; "btn-secondary" ] ] [ Node.text "Cancel" ]
     in
     let not_empty = List.is_empty li |> not in
     Node.div
       Attr.[ classes [ "d-inline-flex"; "flex-column" ] ]
       [
         ul;
         [ cancel_node ] |> add_if not_empty delete_node |> add_if not_empty restore_node |> Node.div [];
       ]

let links_component ~update_visibility ~update_status_msg ~max_hearts ~max_stamina ingredients =
  return
  @@ let%map update_visibility = update_visibility
     and update_status_msg = update_status_msg
     and ingredients = ingredients
     and max_hearts = max_hearts
     and max_stamina = max_stamina in
     let save_button =
       let handler _evt = Event.Many [ update_visibility Visibility.Save; update_status_msg None ] in
       Node.span
         Attr.[ on_click handler; style pointer; class_ "me-2" ]
         [ Icon.svg ~container:Span Bookmark_plus []; Node.text "Save" ]
     in
     let restore_button =
       let handler _evt = Event.Many [ update_visibility Visibility.Restore; update_status_msg None ] in
       Node.span
         Attr.[ on_click handler; style pointer; class_ "me-2" ]
         [ Icon.svg ~container:Span Folder2_open []; Node.text "Load" ]
     in
     let export =
       let handler _evt =
         copy_link ~max_hearts ~max_stamina ingredients;
         Event.Many [ update_status_msg (Some (Status.Success "Link copied!")) ]
       in
       Node.span
         Attr.[ on_click handler; style pointer ]
         [ Icon.svg ~container:Span Folder_symlink []; Node.text "Share" ]
     in
     Node.div [] [ save_button; restore_button; export ]

let component ~updates ~max_hearts ~max_stamina ingredients =
  let%sub status_msg = Bonsai.state_opt [%here] (module Status) in
  let%pattern_bind status_msg, update_status_msg = status_msg in
  let%sub visibility = Bonsai.state [%here] (module Visibility) ~default_model:Nothing in
  let%pattern_bind visibility, update_visibility = visibility in
  let%sub links =
    links_component ~update_visibility ~update_status_msg ~max_hearts ~max_stamina ingredients
  in
  let%sub contents =
    Bonsai.enum
      (module Visibility)
      ~match_:visibility
      ~with_:(function
        | Nothing -> Bonsai.const @@ Node.div [] []
        | Save -> save_component ~update_status_msg ~update_visibility ingredients
        | Restore -> restore_component ~update_visibility ~update_status_msg updates)
  in
  return
  @@ let%map links = links
     and status_msg = status_msg
     and visibility = visibility
     and contents = contents in
     let status_node =
       match status_msg with
       | None -> Node.span [] []
       | Some (Success msg) -> Node.span Attr.[ class_ "text-success" ] [ Node.text msg ]
       | Some (Danger msg) -> Node.span Attr.[ class_ "text-danger" ] [ Node.text msg ]
     in
     match visibility with
     | Nothing -> Node.div [] [ status_node; links ]
     | _ ->
       Node.div []
         [
           Node.div
             Attr.[ classes [ "border"; "border-secondary"; "d-inline-block"; "p-3"; "rounded" ] ]
             [ status_node; contents ];
           links;
         ]
