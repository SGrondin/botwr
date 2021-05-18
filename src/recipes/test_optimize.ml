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
    (Bird_egg Goron_spice Fresh_milk Raw_meat Electric_safflina Electric_safflina
     Electric_safflina Electric_safflina Electric_safflina) |}];
  test Electro Any data1;
  [%expect
    {|
    ((Monster_guts Moblin_guts) (Monster_guts Hinox_guts)
     (Monster_fang Bokoblin_fang) (Monster_fang Ancient_gear) Bird_egg
     Goron_spice Fresh_milk Raw_meat Electric_darner Electric_darner
     Electric_darner Electric_darner Electric_darner Thunderwing_butterfly
     Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
     Thunderwing_butterfly Electric_safflina Electric_safflina Electric_safflina
     Electric_safflina Electric_safflina) |}];
  test Hearty Meals data1;
  [%expect
    {|
    (Raw_meat Raw_meat Raw_meat Fresh_milk Big_hearty_truffle Big_hearty_truffle
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
     Raw_meat Fresh_milk Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle
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
    169 pts (637, 6.880952)
    Armored_carp, Ironshell_crab, Ironshroom, Palm_fruit x2
    (Food
     ((hearts (Restores 9)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 210)))) (num_ingredients 5)
      (num_effect_ingredients 3)))
    166 pts (637, 5.023810)
    Armored_carp, Ironshell_crab, Ironshroom, Palm_fruit
    (Food
     ((hearts (Restores 7)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 180)))) (num_ingredients 4)
      (num_effect_ingredients 3)))
    163 pts (637, 3.166667)
    Armored_carp, Ironshell_crab, Ironshroom
    (Food
     ((hearts (Restores 5)) (stamina Nothing)
      (effect (Tough ((potency 3) (duration 150)))) (num_ingredients 3)
      (num_effect_ingredients 3))) |}];
  test ~kind:Energizing ~category:Meals data2;
  [%expect
    {|
    (0s)
    113 pts (381, 4.791667)
    Stamella_shroom x5
    (Food
     ((hearts (Restores 5)) (stamina (Restores 7)) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 5)))
    108 pts (381, 3.833333)
    Stamella_shroom x4
    (Food
     ((hearts (Restores 4)) (stamina (Restores 5)) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4)))
    107 pts (381, 4.633333)
    Apple, Stamella_shroom x4
    (Food
     ((hearts (Restores 5)) (stamina (Restores 5)) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 4))) |}];
  test ~kind:Mighty ~category:Meals
    [
      Mighty_bananas, 9;
      Mighty_carp, 9;
      Mighty_porgy, 9;
      Mighty_thistle, 9;
      Bladed_rhino_beetle, 9;
      Razorclaw_crab, 9;
      Razorshroom, 9;
    ];
  [%expect
    {|
    (0s)
    163 pts (174436, 5.666667)
    Mighty_porgy x3
    (Food
     ((hearts (Restores 6)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 150)))) (num_ingredients 3)
      (num_effect_ingredients 3)))
    163 pts (174436, 5.666667)
    Mighty_porgy x2, Razorclaw_crab
    (Food
     ((hearts (Restores 6)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 150)))) (num_ingredients 3)
      (num_effect_ingredients 3)))
    163 pts (174436, 5.666667)
    Mighty_carp, Mighty_porgy x2
    (Food
     ((hearts (Restores 6)) (stamina Nothing)
      (effect (Mighty ((potency 3) (duration 150)))) (num_ingredients 3)
      (num_effect_ingredients 3))) |}];
  test ~kind:Electro ~category:Meals
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Bird_egg, 1;
      Goron_spice, 1;
      Goat_butter, 2;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
    (0s)
    229 pts (2379, 3.100000)
    Bird_egg, Goat_butter, Goron_spice, Voltfin_trout x2
    (Food
     ((hearts (Restores 6)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 560)))) (num_ingredients 5)
      (num_effect_ingredients 2)))
    222 pts (2379, 9.100000)
    Bird_egg, Goron_spice, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores 12)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 510)))) (num_ingredients 5)
      (num_effect_ingredients 2)))
    221 pts (2379, 9.600000)
    Bird_egg, Goat_butter, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores 12)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 500)))) (num_ingredients 5)
      (num_effect_ingredients 2))) |}];
  test ~kind:Electro ~category:Meals
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Bird_egg, 7;
      Goron_spice, 1;
      Goat_butter, 2;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
      (0s)
      229 pts (2379, 3.957143)
      Bird_egg, Goat_butter, Goron_spice, Voltfin_trout x2
      (Food
       ((hearts (Restores 6)) (stamina Nothing)
        (effect (Electro ((potency 3) (duration 560)))) (num_ingredients 5)
        (num_effect_ingredients 2)))
      222 pts (2379, 9.957143)
      Bird_egg, Goron_spice, Raw_whole_bird, Voltfin_trout x2
      (Food
       ((hearts (Restores 12)) (stamina Nothing)
        (effect (Electro ((potency 3) (duration 510)))) (num_ingredients 5)
        (num_effect_ingredients 2)))
      221 pts (2379, 10.457143)
      Bird_egg, Goat_butter, Raw_whole_bird, Voltfin_trout x2
      (Food
       ((hearts (Restores 12)) (stamina Nothing)
        (effect (Electro ((potency 3) (duration 500)))) (num_ingredients 5)
        (num_effect_ingredients 2))) |}];
  test ~kind:Electro ~category:Meals
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Bird_egg, 7;
      Goron_spice, 1;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
    (0s)
    222 pts (2379, 9.957143)
    Bird_egg, Goron_spice, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores 12)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 510)))) (num_ingredients 5)
      (num_effect_ingredients 2)))
    221 pts (2379, 6.257143)
    Bird_egg, Goron_spice, Voltfin_trout x3
    (Food
     ((hearts (Restores 8)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 630)))) (num_ingredients 5)
      (num_effect_ingredients 3)))
    221 pts (2379, 2.657143)
    Bird_egg, Goron_spice, Voltfin_trout, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores 6)) (stamina Nothing)
      (effect (Electro ((potency 3) (duration 630)))) (num_ingredients 5)
      (num_effect_ingredients 3))) |}];
  test ~kind:Electro ~category:Meals
    [ Electric_safflina, 1; Zapshroom, 1; Bird_egg, 7; Goron_spice, 1; Apple, 2; Raw_whole_bird, 2 ];
  [%expect
    {|
    (0s)
    190 pts (62, 5.357143)
    Bird_egg, Electric_safflina, Goron_spice, Raw_whole_bird, Zapshroom
    (Food
     ((hearts (Restores 9)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 510)))) (num_ingredients 5)
      (num_effect_ingredients 2)))
    188 pts (62, -0.142857)
    Bird_egg, Electric_safflina, Goron_spice, Zapshroom
    (Food
     ((hearts (Restores 3)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 480)))) (num_ingredients 4)
      (num_effect_ingredients 2)))
    183 pts (62, 11.857143)
    Bird_egg, Electric_safflina, Raw_whole_bird x2, Zapshroom
    (Food
     ((hearts (Restores 15)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 450)))) (num_ingredients 5)
      (num_effect_ingredients 2))) |}];
  test ~kind:Electro ~category:Meals [ Voltfruit, 1; Zapshroom, 1; Apple, 2; Raw_whole_bird, 1 ];
  [%expect {|
    (0s)
    175 pts (31, 6.000000)
    Apple x2, Raw_whole_bird, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores 10)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 390)))) (num_ingredients 5)
      (num_effect_ingredients 2)))
    173 pts (31, 5.500000)
    Apple, Raw_whole_bird, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores 9)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 360)))) (num_ingredients 4)
      (num_effect_ingredients 2)))
    173 pts (31, 1.000000)
    Apple x2, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores 4)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 360)))) (num_ingredients 4)
      (num_effect_ingredients 2))) |}];
  test ~kind:Hearty ~category:Meals data3;
  [%expect
    {|
    (0s)
    155 pts (31, -0.600000)
    Big_hearty_truffle x3
    (Food
     ((hearts (Full_plus_bonus 12)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3)))
    154 pts (31, -0.400000)
    Big_hearty_truffle x2
    (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 2)))
    150 pts (31, -0.800000)
    Big_hearty_truffle x4
    (Food
     ((hearts (Full_plus_bonus 16)) (stamina Nothing) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4))) |}];
  test ~kind:Energizing ~category:Meals data4;
  [%expect
    {|
    (0s)
    151 pts (637, 5.400000)
    Staminoka_bass x3
    (Food
     ((hearts (Restores 6)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3)))
    148 pts (637, 7.200000)
    Staminoka_bass x4
    (Food
     ((hearts (Restores 8)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4)))
    148 pts (637, 6.200000)
    Stamella_shroom, Staminoka_bass x3
    (Food
     ((hearts (Restores 7)) (stamina (Restores 15)) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4))) |}]

let%expect_test "Optimize" =
  let grouped = Items.Table.of_alist_exn [ Stamella_shroom, 4; Armored_carp, 2; Ironshroom, 1 ] in
  let test ll =
    ll
    |> Optimize.top_sort ~max_hearts:20 ~max_stamina:15 grouped
    |> sprintf !"%{sexp: Optimize.iteration list}"
    |> print_endline
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
    (((tiebreaker 0.75) (rarity 0.25) (score 50) (recipe ((Stamella_shroom 1))))
     ((tiebreaker 2.25) (rarity 1.75) (score 10)
      (recipe ((Stamella_shroom 3) (Ironshroom 1))))
     ((tiebreaker 1.5) (rarity 0.5) (score 10) (recipe ((Armored_carp 1))))) |}]
