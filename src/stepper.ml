open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Bootstrap
open Bootstrap.Basic

module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp]
end

let component default_model ~render ~max_value ~update_kitchen status =
  let apply_action ~inject:_ ~schedule_event:_ model : Action.t -> int = function
    | Increment -> min (model + 1) max_value
    | Decrement -> max (model - 1) 1
  in
  let%sub state =
    Bonsai.state_machine0 [%here] (module Int) (module Action) ~default_model ~apply_action
  in
  return
  @@ let%map state, update = state
     and update_kitchen = update_kitchen in
     let node =
       Node.div
         Attr.[ classes [ "d-flex"; "align-items-center"; "my-2" ] ]
         [
           Node.div
             Attr.[ classes [ "d-inline-flex"; "justify-content-between"; "align-items-center" ] ]
             [
               (* Down *)
               Attr.
                 [
                   style Css_gen.(pointer @> create ~field:"transform" ~value:"rotate(90deg)");
                   on_click (fun _ev -> Event.Many [ update_kitchen status; update Action.Decrement ]);
                 ]
               |> Icon.svg ~bold:true ~width:1.6 ~height:1.6 Down;
               (* Number *)
               Node.div
                 Attr.
                   [
                     classes [ "d-flex"; "justify-content-center" ];
                     style Css_gen.(pointer @> unselectable @> width (`Em 2));
                   ]
                 [ Node.textf "%d" state ];
               (* Up *)
               Attr.
                 [
                   style Css_gen.(pointer @> create ~field:"transform" ~value:"rotate(90deg)");
                   on_click (fun _ev -> Event.Many [ update_kitchen status; update Action.Increment ]);
                 ]
               |> Icon.svg ~bold:true ~width:1.6 ~height:1.6 Up;
             ];
           render state;
         ]
     in
     state, node
