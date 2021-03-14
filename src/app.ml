open! Core

let () =
  Lwt.async_exception_hook :=
    fun ex ->
      let open Lwt in
      let (_ : unit Lwt.t) =
        Lwt_io.printlf "💀 UNCAUGHT EXCEPTION: %s" (Exn.to_string ex) >>= fun () -> exit 2
      in
      ()

let () =
  Lwt_engine.set ~transfer:true ~destroy:true
    ((* Linux *)
    try new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll () with
    | _ ->
      (* MacOS *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.kqueue ())

let () =
  let open Recipes in
  let items =
    Glossary.
      [
        Apple, 3;
        Cane_sugar, 3;
        Raw_prime_meat, 1;
        Raw_bird_thigh, 1;
        Raw_meat, 1;
        Raw_bird_drumstick, 1;
        Bird_egg, 1;
        Fresh_milk, 1;
        Acorn, 1;
        Chickaloo_tree_nut, 1;
        Hylian_rice, 1;
        Tabantha_wheat, 1;
        Goat_butter, 1;
        Goron_spice, 1;
        Rock_salt, 1;
        Hearty_truffle, 1;
        Hearty_bass, 1;
        Hearty_radish, 1;
        Hearty_blueshel_snail, 1;
        Hearty_durian, 1;
        Big_hearty_truffle, 1;
        Hearty_salmon, 1;
        Palm_fruit, 1;
      ]
  in
  Cooking.Compute.run ~max_hearts:20 ~max_stamina:15 ~kind:Hearty items
  |> Cooking.Compute.to_string
  |> print_endline
