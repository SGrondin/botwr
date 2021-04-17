open! Core_kernel

let%expect_test "Cooking by category, basic" =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let data1 =
    Glossary.[ Apple, 1; Palm_fruit, 7; Apple, 1; Ironshell_crab, 2; Ironshroom, 3; Armored_carp, 1 ]
  in
  let data2 =
    Glossary.
      [
        Apple, 2;
        Goat_butter, 7;
        Tabantha_wheat, 2;
        Stamella_shroom, 24;
        Big_hearty_truffle, 2;
        Raw_gourmet_meat, 3;
        Goron_spice, 2;
      ]
  in
  let data3 = Glossary.[ Big_hearty_truffle, 5 ] in
  let data4 = Glossary.[ Staminoka_bass, 5; Stamella_shroom, 5 ] in
  let test ~kind ~category ll =
    Optimize.run ll ~max_hearts ~max_stamina ~kind ~category
    |> Optimize.to_string ~max_hearts ~max_stamina
    |> print_endline
  in
  test ~kind:Tough ~category:Meals data1;
  [%expect
    {|
    (0s)
    123 pts (1585, 2.500000) -- Armored_carp, Ironshell_crab, Ironshroom x3 -- (Food
     ((hearts (Restores 7)) (stamina Nothing)
      (effect (Tough ((potency 4) (duration 250)))) (num_ingredients 5))) |}];
  test ~kind:Energizing ~category:Meals data2;
  [%expect
    {|
    (0s)
    23 pts (12615, 0.208333) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5))) |}];
  test ~kind:Hearty ~category:Meals data3;
  [%expect
    {|
    (0s)
    55 pts (31, 0.600000) -- Big_hearty_truffle x3 -- (Food
     ((hearts (Full_plus_bonus 12)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3))) |}];
  test ~kind:Energizing ~category:Meals data4;
  [%expect
    {|
    (0s)
    57 pts (637, 0.600000) -- Staminoka_bass x3 -- (Food
     ((hearts (Restores 6)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 3))) |}]
