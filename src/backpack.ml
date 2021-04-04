open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic
open Recipes
module Map = Glossary.Map
module Model = Combinations.Recipe

module Action = struct
  type t =
    | Increment    of Glossary.t
    | Decrement    of Glossary.t
    | Decrement_by of (Glossary.t * int)
    | Set          of (Glossary.t * int)
    | Remove       of Glossary.t
  [@@deriving sexp]
end

let unselectable = Attr.style Css_gen.(user_select `None)

let pointer = Attr.style Css_gen.(create ~field:"cursor" ~value:"pointer")

let quantity_node_id = "quantity-input"

let get_quantity_element () =
  let open Js_of_ocaml in
  Dom_html.getElementById_coerce quantity_node_id Dom_html.CoerceTo.input

let get_quantity () =
  let open Js_of_ocaml in
  get_quantity_element ()
  |> Option.bind ~f:(fun el -> Option.try_with (fun () -> el##.value |> Js.to_string |> Int.of_string))

let input_node ~update_backpack ~update_selected key =
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      Option.iter (get_quantity_element ()) ~f:(fun el -> el##focus);
      Lwt.return_unit);

  let handler ev =
    let open Js_of_ocaml in
    match Js.Optdef.case ev##.key (fun () -> None) (fun jss -> Some (Js.to_string jss)) with
    | Some "Enter" ->
      let num = get_quantity () |> Option.value ~default:0 in
      Event.Many [ update_backpack (Action.Set (key, num)); update_selected None ]
    | Some "Escape" -> update_selected None
    | _ -> Event.Ignore
  in
  Node.input
    Attr.
      [
        type_ "text";
        on_keydown handler;
        class_ "form-control";
        id quantity_node_id;
        style Css_gen.(width (`Em 4));
      ]
    []

module Organize = struct
  type t =
    | Effect
    | Alphabetical
  [@@deriving sexp, equal]

  let choices = String.Map.of_alist_exn [ "By Effect", Effect; "A-Z", Alphabetical ]
end

let group_sort backpack : Organize.t -> ((string * Icon.t) option * (Glossary.t * int) String.Map.t) list
    = function
| Effect ->
  let module KM = Ingredient.Effect.Kind.Map in
  Map.fold backpack ~init:KM.empty ~f:(fun ~key:x ~data:num acc ->
      KM.update acc (Glossary.to_kind x) ~f:(fun existing ->
          let key = Glossary.to_string x in
          let data = x, num in
          match existing with
          | None -> String.Map.singleton key data
          | Some acc -> String.Map.set acc ~key ~data))
  |> KM.fold_right ~init:[] ~f:(fun ~key:kind ~data:map acc ->
         let name =
           match kind with
           | Nothing -> "None"
           | Neutral -> "None"
           | Hearty -> "Hearty"
           | Energizing -> "Energizing"
           | Enduring -> "Enduring"
           | Spicy -> "Spicy"
           | Chilly -> "Chilly"
           | Electro -> "Electro"
           | Fireproof -> "Fireproof"
           | Hasty -> "Hasty"
           | Sneaky -> "Sneaky"
           | Mighty -> "Mighty"
           | Tough -> "Tough"
         in
         (Some (name, Icon.of_kind kind), map) :: acc)
| Alphabetical ->
  let sorted =
    Map.fold backpack ~init:String.Map.empty ~f:(fun ~key:x ~data:num acc ->
        String.Map.set acc ~key:(Glossary.to_string x) ~data:(x, num))
  in
  [ None, sorted ]

let render_organize_buttons ~update_organize organize buttons =
  let make_one ~key:label ~data:action acc =
    let id_ = sprintf "organize-%s" label in
    let is_selected = [%equal: Organize.t] action organize in
    let attrs =
      Attr.
        [
          type_ "radio";
          class_ "btn-check";
          name "organize-buttons";
          value label;
          id id_;
          create "autocomplete" "off";
        ]
      |> add_if is_selected Attr.checked
    in
    Node.input attrs []
    :: Node.label
         Attr.
           [
             for_ id_;
             classes [ "btn"; "btn-outline-secondary"; (if is_selected then "active" else "inactive") ];
           ]
         [ Node.text label ]
    :: acc
  in
  String.Map.fold buttons ~init:[] ~f:make_one
  |> Node.div
       Attr.
         [
           class_ "btn-group";
           create "role" "group";
           create "aria-label" "Basic radio toggle button group";
           on_change (fun _evt s -> update_organize (String.Map.find_exn buttons s));
         ]

let render_items ~update_backpack ~update_selected backpack selected =
  let current_uri = Uri.of_string Js_of_ocaml.Url.Current.as_string in
  let svg x action =
    Icon.svg ~bold:true ~width:1.6 ~height:1.6 x
      Attr.[ on_click (fun _ev -> update_backpack action); pointer ]
  in
  let render_category items =
    String.Map.fold_right items ~init:[] ~f:(fun ~key:_ ~data:(item, num) acc ->
        let is_selected = [%equal: Glossary.t option] (Some item) selected in
        let handler =
          Attr.on_click (function
            | _ev when is_selected -> (
              match get_quantity () with
              | Some quantity ->
                Event.Many [ update_backpack (Action.Set (item, quantity)); update_selected None ]
              | None -> update_selected None
            )
            | _ev -> update_selected (Some item))
        in
        let node =
          let first_col =
            Node.div []
              [
                Node.create "img"
                  (Attrs.merge_classes_and_styles
                     Attr.
                       [
                         pointer;
                         unselectable;
                         src (Glossary.to_filename item |> Utils.image_uri current_uri |> Uri.to_string);
                         handler;
                       ])
                  [];
              ]
          in
          let second_col =
            let quantity =
              match is_selected with
              | true -> input_node ~update_backpack ~update_selected item
              | false -> Node.div [ pointer; handler ] [ Node.textf !"x%d" num ]
            in
            Node.div
              Attr.
                [ classes [ "d-flex"; "flex-column"; "justify-content-between"; "align-items-center" ] ]
              [ svg Up (Action.Increment item); quantity; svg Down (Action.Decrement item) ]
          in
          let first_row =
            Node.div Attr.[ classes [ "d-flex"; "justify-content-around" ] ] [ first_col; second_col ]
          in
          let second_row =
            Node.div
              Attr.[ classes [ "d-flex"; "align-items-center" ] ]
              [
                svg X (Action.Remove item); Node.span [ unselectable ] [ Node.textf !"%{Glossary}" item ];
              ]
          in
          Node.div
            Attr.
              [
                style Css_gen.(width (`Em 11) @> height (`Em 11));
                classes [ "border"; "border-1"; "d-flex"; "flex-column"; "justify-content-around" ];
              ]
            [ first_row; second_row ]
        in
        node :: acc)
    |> Node.div Attr.[ classes [ "row"; "row-cols-auto" ] ]
  in
  List.map backpack ~f:(fun (title, items) ->
      Node.div
        Attr.[ class_ "mb-4" ]
        [
          Option.value_map title ~default:Node.none ~f:(fun (s, icon) ->
              Node.h4
                Attr.[ class_ "ms-3"; id (sprintf "anchor-%s" s) ]
                [ Node.text s; Icon.svg icon ~width:2.0 ~height:2.0 ~container:Span [] ]);
          render_category items;
        ])
  |> Node.div []

let all_items =
  let open Glossary in
  List.fold all ~init:Map.empty ~f:(fun acc key -> Map.set acc ~key ~data:0)

let decrement by x =
  match max (x - by) 0 with
  | 0 -> None
  | y -> Some y

let component ~inventory () =
  let apply_action ~inject:_ ~schedule_event:_ model : Action.t -> Model.t = function
    | Increment x -> Map.update model x ~f:(Option.value_map ~default:1 ~f:succ)
    | Decrement x -> Map.change model x ~f:(Option.bind ~f:(decrement 1))
    | Decrement_by (x, i) -> Map.change model x ~f:(Option.bind ~f:(decrement i))
    | Remove x -> Map.remove model x
    | Set (key, 0) -> Map.remove model key
    | Set (key, data) -> Map.set model ~key ~data
  in
  let%sub show_all = Bonsai.state [%here] (module Bool) ~default_model:true in
  let%sub selected = Bonsai.state_opt [%here] (module Glossary) in
  let%sub organize = Bonsai.state [%here] (module Organize) ~default_model:Effect in
  let%sub backpack =
    Bonsai.state_machine0 [%here] (module Model) (module Action) ~default_model:inventory ~apply_action
  in
  return
  @@ let%map backpack, update_backpack = backpack
     and organize, update_organize = organize
     and selected, update_selected = selected
     and show_all, update_show_all = show_all in

     let buttons_node = render_organize_buttons ~update_organize organize Organize.choices in
     let show_all_node =
       Utils.render_switch ~update:update_show_all ~disabled:false ~id:"show-all-checkbox" "Show All"
         show_all
     in

     let jump_to, items_node =
       let items =
         match show_all with
         | true ->
           Map.merge backpack all_items ~f:(fun ~key:_ -> function
             | `Both (x, _)
              |`Left x
              |`Right x ->
               Some x)
         | false -> backpack
       in
       let organized = group_sort items organize in
       let items_node = render_items ~update_backpack ~update_selected organized selected in
       let jump_to =
         match organize with
         | Effect when show_all ->
           let no_decoration = Attr.style Css_gen.(text_decoration ~line:[ `None ] ()) in
           let links =
             List.filter_map organized ~f:(function
               | None, _ -> None
               | Some (s, icon), _ ->
                 Node.a
                   Attr.[ href (sprintf "#anchor-%s" s); no_decoration ]
                   [ Icon.svg icon ~width:2.0 ~height:2.0 ~container:Span Attr.[ class_ "ps-2" ] ]
                 |> Option.return)
           in
           let message = Node.text "Jump to... " in
           Node.div Attr.[ class_ "my-2" ] (message :: links)
         | _ -> Node.none
       in
       jump_to, items_node
     in
     ( backpack,
       update_backpack,
       (`Organize buttons_node, `Show_all show_all_node, `Jump_to jump_to, `Items items_node) )
