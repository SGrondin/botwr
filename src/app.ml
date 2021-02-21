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
  let open Combinations.Advanced in
  let _test r ll =
    generate r ll
    |> Every.keys
    |> List.map ~f:(fun map ->
           Recipes.to_alist map
           |> List.map ~f:(fun (recipe, times) ->
                  sprintf "-- %dx -- %s" times (Combinations.string_of_recipe recipe))
           |> String.concat ~sep:"\n")
    |> String.concat ~sep:"\n-------------------------------\n"
    |> print_endline
  in
  let count r ll = generate r ll |> Every.length in
  let open Glossary in
  let list2 =
    [
      Apple;
      Cane_sugar;
      Apple;
      Cane_sugar;
      Apple;
      Cane_sugar;
      Raw_prime_meat;
      Raw_bird_thigh;
      Raw_meat;
      Raw_bird_drumstick;
      Bird_egg;
      (* Fresh_milk;
         Acorn;
         Chickaloo_tree_nut;
         Hylian_rice;
         Tabantha_wheat;
         Cane_sugar;
         Goat_butter;
         Goron_spice;
         Rock_salt;
            Hearty_truffle;
            Hearty_bass;
            Hearty_radish;
            Hearty_blueshel_snail;
            Hearty_durian;
            Big_hearty_truffle;
            Hearty_salmon;
            Apple;
            Goat_butter;
            Palm_fruit; *)
    ]
  in
  count 5 list2 |> Int.to_string |> print_endline
