open! Core_kernel

let%expect_test "Filter" =
  let data1 =
    Items.Table.of_alist_exn
      [
        Electric_darner, 9;
        Electric_safflina, 7;
        Thunderwing_butterfly, 8;
        Bird_egg, 6;
        Fresh_milk, 2;
        Goron_spice, 1;
        Raw_meat, 3;
        Hearty_bass, 2;
        Hearty_durian, 7;
        Hearty_lizard, 7;
        Hearty_salmon, 9;
        Big_hearty_radish, 8;
        Big_hearty_truffle, 8;
        Monster_guts Hinox_guts, 1;
        Monster_horn Keese_wing, 3;
        Monster_horn Lizalfos_horn, 3;
        Monster_guts Moblin_guts, 1;
        Monster_fang Bokoblin_fang, 1;
        Monster_fang Ancient_gear, 5;
      ]
  in
  let test kind category grouped =
    Optimize.filter ~kind ~category grouped |> sprintf !"%{sexp: Glossary.t list}" |> print_endline
  in
  test Electro Elixirs data1;
  [%expect
    {|
    ((Monster_guts Moblin_guts) (Monster_guts Hinox_guts)
     (Monster_fang Bokoblin_fang) (Monster_fang Ancient_gear) Electric_darner
     Electric_darner Electric_darner Electric_darner Electric_darner
     Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
     Thunderwing_butterfly Thunderwing_butterfly) |}];
  test Electro Meals data1;
  [%expect
    {|
    (Goron_spice Fresh_milk Fresh_milk Bird_egg Electric_safflina
     Electric_safflina Electric_safflina Electric_safflina Electric_safflina) |}];
  test Electro Any data1;
  [%expect
    {|
    ((Monster_guts Moblin_guts) (Monster_guts Hinox_guts)
     (Monster_fang Bokoblin_fang) (Monster_fang Ancient_gear) Goron_spice
     Fresh_milk Fresh_milk Bird_egg Electric_darner Electric_darner
     Electric_darner Electric_darner Electric_darner Thunderwing_butterfly
     Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
     Thunderwing_butterfly Electric_safflina Electric_safflina Electric_safflina
     Electric_safflina Electric_safflina) |}];
  test Hearty Meals data1;
  [%expect
    {|
    (Raw_meat Raw_meat Raw_meat Bird_egg Big_hearty_truffle Big_hearty_truffle
     Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle Hearty_bass
     Hearty_bass Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon
     Hearty_salmon Big_hearty_radish Big_hearty_radish Big_hearty_radish
     Big_hearty_radish Big_hearty_radish Hearty_durian Hearty_durian
     Hearty_durian Hearty_durian Hearty_durian) |}];
  test Hearty Elixirs data1;
  [%expect
    {|
    ((Monster_horn Lizalfos_horn) (Monster_horn Lizalfos_horn)
     (Monster_horn Lizalfos_horn) (Monster_horn Keese_wing) Hearty_lizard
     Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard) |}];
  test Hearty Any data1;
  [%expect
    {|
    ((Monster_horn Lizalfos_horn) (Monster_horn Lizalfos_horn)
     (Monster_horn Lizalfos_horn) (Monster_horn Keese_wing) Raw_meat Raw_meat
     Raw_meat Bird_egg Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle
     Big_hearty_truffle Big_hearty_truffle Hearty_bass Hearty_bass Hearty_salmon
     Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon Big_hearty_radish
     Big_hearty_radish Big_hearty_radish Big_hearty_radish Big_hearty_radish
     Hearty_durian Hearty_durian Hearty_durian Hearty_durian Hearty_durian
     Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard) |}]

let%expect_test "Cooking by category, basic" =
  let max_hearts = 20 in
  let max_stamina = 15 in
  let data1 =
    Glossary.[ Apple, 1; Palm_fruit, 7; Apple, 1; Ironshell_crab, 2; Ironshroom, 3; Armored_carp, 1 ]
  in
  let data1b =
    Glossary.
      [
        Mighty_bananas, 9;
        Mighty_carp, 9;
        Mighty_porgy, 9;
        Mighty_thistle, 9;
        Bladed_rhino_beetle, 9;
        Razorclaw_crab, 9;
        Razorshroom, 9;
      ]
  in
  let data2 =
    Glossary.
      [
        Apple, 5;
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
    88 pts (637, 2.000000) -- Ironshell_crab x2, Ironshroom x3 -- (Food
     ((hearts (Restores 7)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 250)))) (num_ingredients 5)))
    88 pts (637, 2.666667) -- Armored_carp, Ironshell_crab x2, Ironshroom x2 -- (Food
     ((hearts (Restores 8)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 250)))) (num_ingredients 5)))
    79 pts (637, 1.642857) -- Ironshell_crab, Ironshroom x3, Palm_fruit -- (Food
     ((hearts (Restores 7)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 230)))) (num_ingredients 5))) |}];
  test ~kind:Energizing ~category:Meals data2;
  [%expect
    {|
    (0s)
    9 pts (381, 0.208333) -- Stamella_shroom x5 -- (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5)))
    6 pts (381, 0.166667) -- Stamella_shroom x4 -- (Food
     ((hearts (Restores 4)) (stamina (Restores 5)) (effect Nothing)
      (num_ingredients 4)))
    5 pts (381, 0.125000) -- Stamella_shroom x3 -- (Food
     ((hearts (Restores 3)) (stamina (Restores 4)) (effect Nothing)
      (num_ingredients 3))) |}];
  test ~kind:Mighty ~category:Meals data1b;
  [%expect {|
    (0s)
    88 pts (174436, 0.555556) -- Mighty_porgy x5 -- (Food
     ((hearts (Restores 10)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 250)))) (num_ingredients 5)))
    88 pts (174436, 0.555556) -- Razorclaw_crab x5 -- (Food
     ((hearts (Restores 10)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 250)))) (num_ingredients 5)))
    88 pts (174436, 0.555556) -- Mighty_carp x5 -- (Food
     ((hearts (Restores 10)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 250)))) (num_ingredients 5))) |}];
  test ~kind:Hearty ~category:Meals data3;
  [%expect
    {|
    (0s)
    55 pts (31, 0.600000) -- Big_hearty_truffle x3 -- (Food
     ((hearts (Full_plus_bonus 12)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3)))
    54 pts (31, 0.400000) -- Big_hearty_truffle x2 -- (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing)
      (num_ingredients 2)))
    50 pts (31, 0.800000) -- Big_hearty_truffle x4 -- (Food
     ((hearts (Full_plus_bonus 16)) (stamina Nothing) (effect Nothing)
      (num_ingredients 4))) |}];
  test ~kind:Energizing ~category:Meals data4;
  [%expect
    {|
    (0s)
    27 pts (637, 0.600000) -- Staminoka_bass x3 -- (Food
     ((hearts (Restores 6)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 3)))
    26 pts (637, 0.800000) -- Staminoka_bass x4 -- (Food
     ((hearts (Restores 8)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 4)))
    26 pts (637, 0.800000) -- Stamella_shroom, Staminoka_bass x3 -- (Food
     ((hearts (Restores 7)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 4))) |}]

let%expect_test "Optimize" =
  let grouped = Items.Table.of_alist_exn [ Stamella_shroom, 4; Armored_carp, 2; Ironshroom, 1 ] in
  let test ll =
    ll |> Optimize.top_sort grouped |> sprintf !"%{sexp: Optimize.iteration list}" |> print_endline
  in
  test
    [
      50, [ Glossary.Map.of_alist_exn [ Stamella_shroom, 1 ] ];
      ( 10,
        [
          Glossary.Map.of_alist_exn [ Armored_carp, 1 ];
          Glossary.Map.of_alist_exn [ Ironshroom, 1 ];
          Glossary.Map.of_alist_exn [ Stamella_shroom, 3; Ironshroom, 1 ];
        ] );
      3, [ Glossary.Map.of_alist_exn [ Ironshroom, 1 ] ];
    ];
  [%expect
    {|
    (((rarity 0.25) (score 50) (recipe ((Stamella_shroom 1))))
     ((rarity 0.5) (score 10) (recipe ((Armored_carp 1))))
     ((rarity 1) (score 10) (recipe ((Ironshroom 1))))) |}]
