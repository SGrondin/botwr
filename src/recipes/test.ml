open! Core_kernel
open Cooking
open Glossary

let%expect_test "List of ingredients" =
  let test ll = ll |> cook |> sprintf !"%{sexp: Cooking.t}" |> print_endline in
  test [];
  [%expect {| (Failed "No ingredients") |}];
  test [ Endura_shroom ];
  [%expect
    {|
    (Food ((hearts (Restores 2)) (stamina (Full_plus_bonus 1)) (effect Nothing))) |}];
  test [ Endura_shroom; Endura_shroom ];
  [%expect
    {|
    (Food ((hearts (Restores 4)) (stamina (Full_plus_bonus 1)) (effect Nothing))) |}];
  test [ Endura_shroom; Endura_shroom; Big_hearty_radish ];
  [%expect {| (Food ((hearts (Restores 12)) (stamina Nothing) (effect Nothing))) |}];
  test [ Big_hearty_radish ];
  [%expect {| (Food ((hearts (Full_plus_bonus 5)) (stamina Nothing) (effect Nothing))) |}];
  test [ Hylian_rice; Bird_egg; Raw_meat; Staminoka_bass ];
  [%expect
    {|
    (Food ((hearts (Restores 8)) (stamina (Restores 5)) (effect Nothing))) |}]
