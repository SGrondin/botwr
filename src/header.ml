open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

let render_instructions (data : Kitchen.Model.t) kind total ~game_node ~kind_buttons ~max_hearts_node
   ~max_stamina_node ~update_kitchen ~clear_all_events =
  let is_loaded =
    match data with
    | New
     |Completed ->
      true
    | Ready _
     |Loading ->
      false
  in
  let clear_all_node =
    let handler _evt = Event.Many (update_kitchen Kitchen.Model.New :: clear_all_events) in
    Node.div []
      [
        Node.button
          Attr.[ type_ "button"; classes [ "btn"; "btn-outline-danger"; "btn-sm" ]; on_click handler ]
          [ Node.text "Reset inventory " ];
      ]
  in
  let instructions =
    [
      "Pick a game.", None, game_node;
      "Set your maximum hearts containers.", None, max_hearts_node;
      "Set your maximum stamina containers.", None, max_stamina_node;
      "Pick a bonus.", Some (Option.is_some kind), kind_buttons;
      "Add ingredients to your inventory.", Some (total <> 0), clear_all_node;
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

let render ~game_node ~clear_all_events ~total ingredients
   Kitchen.
     {
       data;
       update_data = update_kitchen;
       calculate;
       kitchen_node;
       kind;
       kind_buttons;
       meals_switch;
       elixirs_switch;
       use_special_switch;
       max_hearts = _;
       max_hearts_node;
       max_stamina = _;
       max_stamina_node;
     } =
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
                update_kitchen (Kitchen.Model.Ready optimized)
              | None -> update_kitchen Kitchen.Model.New);
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
  let instructions_node =
    render_instructions data kind total ~game_node ~kind_buttons ~max_hearts_node ~max_stamina_node
      ~update_kitchen ~clear_all_events
  in
  Node.div []
    [
      Node.h3 Attr.[ id "top" ] [ Node.text "BOTW Cooking Optimizer" ];
      instructions_node;
      Node.div Attr.[ class_ "ms-2" ] [ meals_switch; elixirs_switch ];
      Node.div Attr.[ class_ "ms-2" ] [ use_special_switch ];
      button;
      kitchen_node;
    ]
