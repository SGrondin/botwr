open! Core_kernel

let%expect_test "Cooking by category, basic" =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let factor = 2 in
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
  let test ~kind ll =
    Optimize.Basic.run ll ~max_hearts ~max_stamina ~factor ~kind
    |> Optimize.Basic.to_string
    |> print_endline
  in
  test ~kind:Tough data1;
  [%expect
    {|
    (0s)
    59 pts (1585) -- Armored_carp, Ironshell_crab, Ironshroom x3 -- (Food
     ((hearts (Restores 7)) (stamina Nothing)
      (effect (Tough ((potency 4) (duration 250)))) (num_ingredients 5)))
    5 pts (119) -- Ironshell_crab, Palm_fruit x4 -- (Food
     ((hearts (Restores 10)) (stamina Nothing)
      (effect (Tough ((potency 1) (duration 170)))) (num_ingredients 5))) |}];
  test ~kind:Energizing data2;
  [%expect
    {|
    (0s)
    9 pts (12615) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5)))
    9 pts (12615) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5)))
    9 pts (12615) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5)))
    9 pts (12615) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5)))
    6 pts (9401) -- Stamella_shroom x4 -- (Food
     ((hearts (Restores 4)) (stamina (Restores 5)) (effect Nothing)
      (num_ingredients 4))) |}]

let%expect_test "Cooking by category, advanced" =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let factor = 2 in
  let data1 =
    Glossary.[ Apple, 1; Palm_fruit, 7; Apple, 1; Ironshell_crab, 2; Ironshroom, 3; Armored_carp, 1 ]
  in
  let data2 =
    Glossary.
      [
        Apple, 2;
        Goat_butter, 7;
        Tabantha_wheat, 2;
        Stamella_shroom, 25;
        Big_hearty_truffle, 2;
        Raw_gourmet_meat, 3;
      ]
  in
  data1
  |> Optimize.group
  |> Optimize.filter ~kind:Tough
  |> sprintf !"%{sexp: Glossary.t list}"
  |> print_endline;
  [%expect
    {|
    (Armored_carp Ironshroom Ironshroom Ironshroom Ironshell_crab Ironshell_crab
     Apple Apple Palm_fruit Palm_fruit Palm_fruit Palm_fruit) |}];
  let test ~kind items =
    Optimize.Advanced.run ~max_hearts ~max_stamina ~factor ~kind items
    |> Optimize.Advanced.to_string
    |> print_endline
  in
  test ~kind:Tough data1;
  [%expect
    {|
    Best of 10473 with 64 points (0s) :
    -- 1x -- Apple x2, Ironshell_crab, Palm_fruit x2
    -- 1x -- Armored_carp, Ironshell_crab, Ironshroom x3 |}];
  test ~kind:Energizing data2;
  [%expect
    {|
    Best of 81224 with 6 points (0s) :
    -- 1x -- Stamella_shroom x4
    -- 1x -- Apple, Stamella_shroom |}]
