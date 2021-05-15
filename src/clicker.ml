open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom

module Action = struct
  type t =
    | Click
    | Elapsed
  [@@deriving sexp]
end

let component ?(default_model = false) delay =
  let apply_action ~inject ~schedule_event _ : Action.t -> bool = function
    | Click ->
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep delay in
          Action.Elapsed |> inject |> schedule_event;
          Lwt.return_unit);
      true
    | Elapsed -> false
  in
  let%sub component =
    Bonsai.state_machine0 [%here] (module Bool) (module Action) ~default_model ~apply_action
  in
  return
  @@ let%map state, update = component in
     state, update Action.Click
