open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom

let set_all_input_id = "set-all-input"

let get_quantity_element () =
  let open Js_of_ocaml in
  Dom_html.getElementById_coerce set_all_input_id Dom_html.CoerceTo.input

module Status = struct
  type t =
    | New
    | Success of string
    | Danger  of string
  [@@deriving sexp, equal]
end

let component ~updates ~update_kitchen =
  let%sub status = Bonsai.state [%here] (module Status) ~default_model:New in
  return
  @@ let%map status, update_status = status
     and updates = updates
     and update_kitchen = update_kitchen in
     let input =
       Node.input
         Attr.
           [
             type_ "tel";
             classes [ "form-control"; "mx-2" ];
             placeholder "Qty";
             id set_all_input_id;
             create "max_length" "3";
             style Css_gen.(width (`Em 4) @> height (`Em 2));
           ]
         []
     in
     let button =
       let handler _ev =
         let open Js_of_ocaml in
         Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
             let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.1 in
             get_quantity_element () |> Option.iter ~f:(fun el -> el##.value := Js.string "");
             Lwt.return_unit);
         get_quantity_element ()
         |> Option.bind ~f:(fun el ->
                Option.try_with (fun () -> el##.value |> Js.to_string |> Int.of_string |> min 999))
         |> function
         | None -> update_status (Danger "Enter a number")
         | Some qty ->
           let events =
             Recipes.Glossary.Map.fold updates ~init:[] ~f:(fun ~key:_ ~data:trigger acc ->
                 trigger (Card.Action.Set qty) :: acc)
           in
           update_status (Success (sprintf "Done! (All: x%d)" qty))
           :: update_kitchen Kitchen.Model.New
           :: events
           |> Ui_event.Many
       in
       Node.button
         Attr.
           [
             type_ "button";
             on_click handler;
             classes [ "btn"; "btn-outline-secondary"; "btn-sm"; "me-3" ];
           ]
         [ Node.text "Apply" ]
     in
     let status_node =
       match status with
       | New -> Node.span [] []
       | Success msg -> Node.span Attr.[ class_ "text-success" ] [ Node.text msg ]
       | Danger msg -> Node.span Attr.[ class_ "text-danger" ] [ Node.text msg ]
     in

     Node.div
       Attr.[ classes [ "d-flex"; "align-items-center" ] ]
       [ Node.text "Set all items to..."; input; button; status_node ]
