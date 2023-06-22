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
        Endura_carrot, 7;
        Endura_shroom, 7;
        Tireless_frog, 7;
        Monster_guts Hinox_guts, 1;
        Monster_horn Keese_wing, 3;
        Monster_horn Lizalfos_horn, 3;
        Monster_guts Moblin_guts, 1;
        Monster_fang Bokoblin_fang, 1;
        Monster_fang Ancient_gear, 5;
      ]
  in
  let test ?(game = (BOTW : Game.t)) kind category grouped =
    Optimize.filter ~game ~kind ~category ~use_special:true grouped
    |> List.map ~f:(fun ll -> List.map ll ~f:(fun x -> x.item) |> List.sort ~compare:Glossary.compare)
    |> sprintf !"%{sexp: Glossary.t list list}"
    |> print_endline
  in
  test Electro Elixirs data1;
  [%expect
    {|
    (((Monster_horn Keese_wing))
     (Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
      Thunderwing_butterfly Electric_darner Electric_darner Electric_darner
      Electric_darner (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Electro Meals data1;
  [%expect
    {|
    ((Raw_meat Fresh_milk Goron_spice Bird_egg Electric_safflina
      Electric_safflina Electric_safflina Electric_safflina Electric_safflina)
     ()) |}];
  test Electro Any data1;
  [%expect
    {|
    ((Raw_meat Fresh_milk Goron_spice Bird_egg Electric_safflina
      Electric_safflina Electric_safflina Electric_safflina Electric_safflina
      (Monster_horn Keese_wing))
     (Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
      Thunderwing_butterfly Electric_darner Electric_darner Electric_darner
      Electric_darner (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Hearty Meals data1;
  [%expect
    {|
    ((Hearty_bass Hearty_bass Hearty_durian Hearty_durian Hearty_durian
      Hearty_durian Hearty_durian Big_hearty_truffle Big_hearty_truffle
      Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle Hearty_salmon
      Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon Big_hearty_radish
      Big_hearty_radish Big_hearty_radish Big_hearty_radish Big_hearty_radish)
     ()) |}];
  test Hearty Elixirs data1;
  [%expect
    {|
    (((Monster_horn Keese_wing))
     (Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard
      (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Hearty Any data1;
  [%expect
    {|
    ((Hearty_bass Hearty_bass Hearty_durian Hearty_durian Hearty_durian
      Hearty_durian Hearty_durian Big_hearty_truffle Big_hearty_truffle
      Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle Hearty_salmon
      Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon Big_hearty_radish
      Big_hearty_radish Big_hearty_radish Big_hearty_radish Big_hearty_radish
      (Monster_horn Keese_wing))
     (Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard
      (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Enduring Meals data1;
  [%expect
    {|
    ((Endura_shroom Endura_shroom Endura_shroom Endura_shroom Endura_shroom
      Endura_carrot Endura_carrot Endura_carrot Endura_carrot Endura_carrot)
     ()) |}];
  test Enduring Elixirs data1;
  [%expect
    {|
    (((Monster_horn Keese_wing))
     (Tireless_frog Tireless_frog Tireless_frog Tireless_frog
      (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Enduring Any data1;
  [%expect
    {|
    ((Endura_shroom Endura_shroom Endura_shroom Endura_shroom Endura_shroom
      Endura_carrot Endura_carrot Endura_carrot Endura_carrot Endura_carrot
      (Monster_horn Keese_wing))
     (Tireless_frog Tireless_frog Tireless_frog Tireless_frog
      (Monster_horn Keese_wing) (Monster_horn Keese_wing)
      (Monster_horn Keese_wing) (Monster_horn Lizalfos_horn)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_fang Ancient_gear) (Monster_fang Ancient_gear)
      (Monster_guts Moblin_guts) (Monster_guts Hinox_guts))) |}];
  test Enduring Any
    (Items.Table.of_alist_exn [ Endura_carrot, 1; Dragon_horns Farosh, 0; Endura_shroom, 0 ]);
  [%expect {| ((Endura_carrot) ()) |}];
  test Enduring Any (Items.Table.of_alist_exn [ Endura_carrot, 1; Star_fragment, 0; Endura_shroom, 0 ]);
  [%expect {| ((Endura_carrot) ()) |}];

  test Neutral Any
    (Items.Table.of_alist_exn
       [
         Sundelion, 1;
         Sun_pumpkin, 3;
         Raw_bird_drumstick, 1;
         Bird_egg, 2;
         Raw_prime_meat, 1;
         Sanke_carp, 1;
         Apple, 1;
       ]);
  [%expect {| ((Apple Sanke_carp Raw_bird_drumstick Raw_prime_meat Bird_egg Bird_egg) ()) |}]

let%expect_test "Cooking by category, basic" =
  let test ?(game = (BOTW : Game.t)) ~kind ~category ~algo ?(max_hearts = 20) ?(max_stamina = 15)
     ?(gloomy_hearts = 0) ?(use_special = true) ll =
    let settings =
      Optimize.{ game; max_hearts; max_stamina; gloomy_hearts; algo; kind; category; use_special }
    in
    Optimize.run settings ll |> Optimize.to_string |> print_endline
  in
  test ~kind:Tough ~category:Meals ~algo:Balanced
    [ Apple, 1; Palm_fruit, 7; Apple, 1; Ironshell_crab, 2; Ironshroom, 3; Armored_carp, 1 ];
  [%expect
    {|
    (0s)
    718 pts (637, 5.333333)
    Armored Carp, Ironshell Crab x2, Ironshroom x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    718 pts (637, 5.000000)
    Ironshell Crab x2, Ironshroom x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    718 pts (637, 4.500000)
    Armored Carp, Ironshell Crab, Ironshroom x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  test ~kind:Energizing ~category:Meals ~algo:Balanced
    [
      Apple, 5;
      Goat_butter, 7;
      Tabantha_wheat, 2;
      Stamella_shroom, 24;
      Big_hearty_truffle, 2;
      Raw_gourmet_meat, 3;
      Goron_spice, 2;
    ];
  [%expect
    {|
    (0s)
    141 pts (31, 4.791667)
    Stamella Shroom x5
    (Food
     ((hearts (Restores (Quarters 20)))
      (stamina (Restores ((potency 7) (wasted 0)))) (effect Nothing) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    125 pts (31, 3.833333)
    Stamella Shroom x4
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Restores ((potency 5) (wasted 3)))) (effect Nothing) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    122 pts (31, 2.875000)
    Stamella Shroom x3
    (Food
     ((hearts (Restores (Quarters 12)))
      (stamina (Restores ((potency 4) (wasted 1)))) (effect Nothing) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Mighty ~category:Meals ~algo:Balanced
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
    787 pts (174436, 5.666667)
    Mighty Porgy, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    787 pts (174436, 5.666667)
    Mighty Carp x2, Mighty Porgy
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    787 pts (174436, 5.666667)
    Mighty Carp, Mighty Porgy, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Mighty ~category:Meals ~algo:Maximize
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
    889 pts (174436, 3.444444)
    Mighty Thistle x3, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    889 pts (174436, 3.444444)
    Mighty Carp x2, Mighty Thistle x3
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    889 pts (174436, 3.444444)
    Mighty Carp, Mighty Thistle x3, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 250)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Fresh_milk, 1;
      Goron_spice, 1;
      Goat_butter, 2;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
    (0s)
    861 pts (2379, 8.100000)
    Fresh Milk, Goron Spice, Raw Whole Bird, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    861 pts (2379, 7.600000)
    Goat Butter, Goron Spice, Raw Whole Bird, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    860 pts (2379, 8.600000)
    Fresh Milk, Goat Butter, Raw Whole Bird, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 490)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Maximize
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
    949 pts (2379, 0.800000)
    Electric Safflina x2, Voltfin Trout, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 2) (duration 750)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    948 pts (2379, 2.600000)
    Electric Safflina x2, Voltfin Trout x2, Voltfruit
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 3) (duration 750)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    947 pts (2379, 3.100000)
    Electric Safflina, Voltfin Trout x2, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 4) (duration 750)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Fresh_milk, 7;
      Goron_spice, 1;
      Goat_butter, 2;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
      (0s)
      861 pts (2379, 8.957143)
      Fresh Milk, Goron Spice, Raw Whole Bird, Voltfin Trout x2
      (Food
       ((hearts (Restores (Quarters 44))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 500)))) (fused 5)
        (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
      861 pts (2379, 7.600000)
      Goat Butter, Goron Spice, Raw Whole Bird, Voltfin Trout x2
      (Food
       ((hearts (Restores (Quarters 40))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 500)))) (fused 5)
        (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
      860 pts (2379, 9.457143)
      Fresh Milk, Goat Butter, Raw Whole Bird, Voltfin Trout x2
      (Food
       ((hearts (Restores (Quarters 44))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 490)))) (fused 5)
        (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [
      Electric_safflina, 2;
      Voltfin_trout, 5;
      Voltfruit, 1;
      Zapshroom, 1;
      Fresh_milk, 7;
      Goron_spice, 1;
      Apple, 2;
      Raw_whole_bird, 2;
    ];
  [%expect
    {|
    (0s)
    861 pts (2379, 8.957143)
    Fresh Milk, Goron Spice, Raw Whole Bird, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    858 pts (2379, 3.457143)
    Fresh Milk, Goron Spice, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 470)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 2) (random_effects ())))
    855 pts (2379, 13.600000)
    Goron Spice, Raw Whole Bird x2, Voltfin Trout x2
    (Food
     ((hearts (Restores (Quarters 64))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 450)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [ Electric_safflina, 1; Zapshroom, 1; Bird_egg, 7; Goron_spice, 1; Apple, 2; Raw_whole_bird, 2 ];
  [%expect
    {|
    (0s)
    367 pts (62, 10.857143)
    Bird Egg, Electric Safflina, Goron Spice, Raw Whole Bird x2
    (Food
     ((hearts (Restores (Quarters 56))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 390)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    365 pts (62, 5.357143)
    Bird Egg, Electric Safflina, Goron Spice, Raw Whole Bird
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 360)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    362 pts (62, -0.142857)
    Bird Egg, Electric Safflina, Goron Spice
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 330)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [ Voltfruit, 1; Zapshroom, 1; Apple, 2; Raw_whole_bird, 1 ];
  [%expect
    {|
    (0s)
    350 pts (31, 6.000000)
    Apple x2, Raw Whole Bird, Voltfruit
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 240)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    347 pts (31, 5.500000)
    Apple, Raw Whole Bird, Voltfruit
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 210)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ())))
    347 pts (31, 1.000000)
    Apple x2, Voltfruit
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 210)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Hearty ~category:Meals ~algo:Balanced [ Big_hearty_truffle, 5 ];
  [%expect
    {|
    (0s)
    213 pts (31, -0.600000)
    Big Hearty Truffle x3
    (Food
     ((hearts (Full_plus_bonus 12)) (stamina Nothing) (effect Nothing) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    210 pts (31, -0.400000)
    Big Hearty Truffle x2
    (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ())))
    204 pts (31, -0.800000)
    Big Hearty Truffle x4
    (Food
     ((hearts (Full_plus_bonus 16)) (stamina Nothing) (effect Nothing) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ()))) |}];
  test ~kind:Energizing ~category:Meals ~algo:Balanced [ Staminoka_bass, 5; Stamella_shroom, 5; Apple, 5 ];
  [%expect
    {|
    (0s)
    206 pts (637, 5.400000)
    Staminoka Bass x3
    (Food
     ((hearts (Restores (Quarters 24)))
      (stamina (Restores ((potency 16) (wasted 4)))) (effect Nothing) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    204 pts (637, 6.200000)
    Stamella Shroom, Staminoka Bass x3
    (Food
     ((hearts (Restores (Quarters 28)))
      (stamina (Restores ((potency 18) (wasted 1)))) (effect Nothing) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    203 pts (637, 6.000000)
    Stamella Shroom x3, Staminoka Bass x2
    (Food
     ((hearts (Restores (Quarters 28)))
      (stamina (Restores ((potency 15) (wasted 2)))) (effect Nothing) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  let data5 =
    Glossary.
      [
        Goat_butter, 1;
        Goron_spice, 1;
        Silent_shroom, 2;
        Sunset_firefly, 3;
        Monster_guts Hinox_guts, 5;
        Fairy, 2;
      ]
  in
  test ~kind:Sneaky ~category:Any ~algo:Balanced data5;
  [%expect
    {|
    (0s)
    383 pts (500, -0.733333)
    Hinox Guts x2, Sunset Firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 500)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ())))
    360 pts (500, -0.533333)
    Hinox Guts, Sunset Firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 310)))) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 1) (random_effects ())))
    344 pts (500, 8.000000)
    Fairy, Goat Butter, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 320)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize data5;
  [%expect
    {|
    (0s)
    460 pts (500, -1.133333)
    Hinox Guts x4, Sunset Firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 880)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    449 pts (500, -1.266667)
    Hinox Guts x3, Sunset Firefly x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 810)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    438 pts (500, -1.400000)
    Hinox Guts x2, Sunset Firefly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 2) (duration 740)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
  let data6 =
    Glossary.
      [
        Silent_shroom, 2;
        Cane_sugar, 1;
        Fresh_milk, 1;
        Goat_butter, 1;
        Goron_spice, 1;
        Sanke_carp, 1;
        Fairy, 2;
        Dragon_scales Dinraal, 1;
        Dragon_claws Dinraal, 1;
        Dragon_fangs Dinraal, 1;
        Dragon_horns Dinraal, 1;
      ]
  in
  test ~kind:Sneaky ~category:Any ~algo:Balanced data6;
  [%expect
    {|
    (0s)
    359 pts (1647, -2.500000)
    Cane Sugar, Fresh Milk, Goat Butter, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 450)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    353 pts (1647, 8.000000)
    Fairy, Fresh Milk, Goat Butter, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 48))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 400)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    353 pts (1647, 8.000000)
    Cane Sugar, Fairy, Fresh Milk, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 48))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 400)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize data6;
  [%expect
    {|
    (0s)
    563 pts (1647, 2.500000)
    Shard of Dinraal's Horn, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 19))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1920)))) (fused 27)
      (num_ingredients 2) (num_effect_ingredients 2)
      (random_effects (Potency Duration Red_hearts))))
    562 pts (1647, 12.000000)
    Fairy, Shard of Dinraal's Horn, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 59))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1950)))) (fused 28)
      (num_ingredients 3) (num_effect_ingredients 2)
      (random_effects (Potency Duration Red_hearts))))
    562 pts (1647, 2.500000)
    Fresh Milk, Shard of Dinraal's Horn, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 23))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 2000)))) (fused 28)
      (num_ingredients 3) (num_effect_ingredients 2)
      (random_effects (Potency Duration Red_hearts)))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Balanced ~use_special:false data6;
  [%expect
    {|
    (0s)
    359 pts (62, -2.500000)
    Cane Sugar, Fresh Milk, Goat Butter, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 450)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    350 pts (62, -1.500000)
    Fresh Milk, Goat Butter, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 370)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    350 pts (62, -1.500000)
    Cane Sugar, Fresh Milk, Goron Spice, Silent Shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 370)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize ~use_special:false data6;
  [%expect
    {|
    (0s)
    407 pts (62, -1.000000)
    Fresh Milk, Goat Butter, Goron Spice, Silent Shroom x2
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    407 pts (62, -1.000000)
    Cane Sugar, Fresh Milk, Goron Spice, Silent Shroom x2
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    407 pts (62, -2.000000)
    Cane Sugar, Goat Butter, Goron Spice, Silent Shroom x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Enduring ~category:Any ~algo:Balanced [ Endura_shroom, 5 ];
  [%expect
    {|
    (0s)
    225 pts (31, 1.800000)
    Endura Shroom
    (Food
     ((hearts (Restores (Quarters 8)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (fused 1) (num_ingredients 1) (num_effect_ingredients 1)
      (random_effects ())))
    224 pts (31, 7.200000)
    Endura Shroom x4
    (Food
     ((hearts (Restores (Quarters 32)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects ())))
    222 pts (31, 3.600000)
    Endura Shroom x2
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (fused 2) (num_ingredients 2) (num_effect_ingredients 2)
      (random_effects ()))) |}];
  test ~kind:Hearty ~category:Any ~algo:Balanced [ Hearty_bass, 5 ];
  [%expect
    {|
    (0s)
    215 pts (31, -1.000000)
    Hearty Bass x5
    (Food
     ((hearts (Full_plus_bonus 10)) (stamina Nothing) (effect Nothing) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    208 pts (31, -0.800000)
    Hearty Bass x4
    (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    201 pts (31, -0.600000)
    Hearty Bass x3
    (Food
     ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Enduring ~category:Any ~algo:Balanced [ Endura_carrot, 3; Endura_shroom, 5; Apple, 2 ];
  [%expect
    {|
    (0s)
    261 pts (218, 14.600000)
    Endura Carrot x3, Endura Shroom x2
    (Food
     ((hearts (Restores (Quarters 64)))
      (stamina (Full_plus_bonus ((potency 7) (wasted 0)))) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 5)
      (random_effects ())))
    259 pts (218, 11.000000)
    Endura Carrot x3
    (Food
     ((hearts (Restores (Quarters 48)))
      (stamina (Full_plus_bonus ((potency 6) (wasted 0)))) (effect Nothing)
      (fused 3) (num_ingredients 3) (num_effect_ingredients 3)
      (random_effects ())))
    254 pts (218, 12.800000)
    Endura Carrot x3, Endura Shroom
    (Food
     ((hearts (Restores (Quarters 56)))
      (stamina (Full_plus_bonus ((potency 6) (wasted 2)))) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects ()))) |}];
  let data7 =
    Glossary.
      [
        Razorclaw_crab, 3;
        Mighty_porgy, 3;
        Mighty_carp, 3;
        Goron_spice, 1;
        Bird_egg, 1;
        Dragon_fangs Naydra, 1;
        Dragon_horns Naydra, 1;
      ]
  in
  test ~kind:Mighty ~category:Any ~algo:Balanced ~use_special:false data7;
  [%expect
    {|
    (0s)
    808 pts (1023, 5.000000)
    Bird Egg, Goron Spice, Mighty Porgy, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (1023, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp x2, Mighty Porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (1023, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp, Mighty Porgy, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Mighty ~category:Any ~algo:Balanced data7;
  [%expect
    {|
    (0s)
    808 pts (2382, 5.000000)
    Bird Egg, Goron Spice, Mighty Porgy, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (2382, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp x2, Mighty Porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (2382, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp, Mighty Porgy, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Mighty ~category:Any ~algo:Maximize data7;
  [%expect
    {|
    (0s)
    1072 pts (2382, 7.000000)
    Mighty Porgy, Razorclaw Crab x2, Shard of Naydra's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    1072 pts (2382, 7.000000)
    Mighty Carp x2, Mighty Porgy, Shard of Naydra's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    1072 pts (2382, 7.000000)
    Mighty Carp, Mighty Porgy, Razorclaw Crab, Shard of Naydra's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts)))) |}];
  let data8 =
    Glossary.
      [
        Razorclaw_crab, 3;
        Mighty_porgy, 3;
        Mighty_carp, 3;
        Goron_spice, 1;
        Bird_egg, 1;
        Dragon_scales Naydra, 1;
        Star_fragment, 1;
      ]
  in
  test ~kind:Mighty ~category:Any ~algo:Balanced data8;
  [%expect
    {|
    (0s)
    808 pts (1586, 5.000000)
    Bird Egg, Goron Spice, Mighty Porgy, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (1586, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp x2, Mighty Porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (1586, 5.000000)
    Bird Egg, Goron Spice, Mighty Carp, Mighty Porgy, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];

  test ~kind:Enduring ~category:Any ~algo:Balanced Glossary.[ Apple, 3; Endura_carrot, 1 ];
  [%expect
    {|
    (0s)
    233 pts (1, 3.000000)
    Endura Carrot
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (fused 1) (num_ingredients 1) (num_effect_ingredients 1)
      (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:8 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Balanced
    [ Glowing_cave_fish, 4; Brightcap, 2 ];
  [%expect
    {|
    (0s)
    796 pts (62, 5.750000)
    Brightcap, Glowing Cave Fish x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 0) (duration 480)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    780 pts (62, 7.000000)
    Glowing Cave Fish x4
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 480)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    712 pts (62, 6.250000)
    Brightcap x2, Glowing Cave Fish x3
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 600)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 2; Brightcap, 2 ];
  [%expect
    {|
    (0s)
    558 pts (15, 3.500000)
    Brightcap, Glowing Cave Fish x2
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 0) (duration 360)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    524 pts (15, 4.000000)
    Brightcap x2, Glowing Cave Fish x2
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 480)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    338 pts (15, 0.500000)
    Brightcap
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 0) (duration 120)))) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 3 ];
  [%expect
    {|
    (0s)
    542 pts (7, 5.000000)
    Glowing Cave Fish x3
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 360)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    322 pts (7, 1.666667)
    Glowing Cave Fish
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 120)))) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    272 pts (7, 3.333333)
    Glowing Cave Fish x2
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 3) (duration 240)))) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 4 ];
  [%expect
    {|
    (0s)
    780 pts (15, 7.000000)
    Glowing Cave Fish x4
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 480)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    542 pts (15, 5.250000)
    Glowing Cave Fish x3
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 360)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    322 pts (15, 1.750000)
    Glowing Cave Fish
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 120)))) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [ Brightcap, 4 ];
  [%expect
    {|
    (0s)
    405 pts (15, 3.000000)
    Brightcap x4
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 3) (duration 480)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    393 pts (15, 2.250000)
    Brightcap x3
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 2) (duration 360)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    381 pts (15, 1.500000)
    Brightcap x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 240)))) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Brightcap, 5 ];
  [%expect
    {|
    (0s)
    472 pts (31, 4.000000)
    Brightcap x5
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 0) (duration 600)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    338 pts (31, 0.800000)
    Brightcap
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 0) (duration 120)))) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    304 pts (31, 1.600000)
    Brightcap x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 240)))) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];

  test ~kind:Sunny ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:2 ~category:Any ~algo:Balanced
    [ Sundelion, 1 ];
  [%expect
    {|
    (0s)
    123 pts (1, -1.000000)
    Sundelion
    (Food
     ((hearts (Unglooms 3 (Quarters 0))) (stamina Nothing) (effect Nothing)
      (fused 1) (num_ingredients 1) (num_effect_ingredients 1)
      (random_effects ()))) |}];

  test ~kind:Sunny ~game:TOTK ~max_hearts:5 ~max_stamina:5 ~gloomy_hearts:3 ~category:Any ~algo:Balanced
    [ Apple, 1; Hylian_shroom, 1; Hyrule_herb, 1; Sundelion, 2 ];
  test ~kind:Sunny ~game:TOTK ~max_hearts:5 ~max_stamina:5 ~gloomy_hearts:3 ~category:Any ~algo:Maximize
    [ Apple, 1; Hylian_shroom, 1; Hyrule_herb, 1; Sundelion, 2 ];
  [%expect
    {|
      (0s)
      169 pts (31, -2.500000)
      Hylian Shroom, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
        (fused 3) (num_ingredients 3) (num_effect_ingredients 1)
        (random_effects ())))
      169 pts (31, -2.500000)
      Apple, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
        (fused 3) (num_ingredients 3) (num_effect_ingredients 1)
        (random_effects ())))
      164 pts (31, -3.500000)
      Apple, Hylian Shroom, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 16))) (stamina Nothing) (effect Nothing)
        (fused 4) (num_ingredients 4) (num_effect_ingredients 1)
        (random_effects ())))
      (0s)
      176 pts (31, -3.500000)
      Apple, Hylian Shroom, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 16))) (stamina Nothing) (effect Nothing)
        (fused 4) (num_ingredients 4) (num_effect_ingredients 1)
        (random_effects ())))
      169 pts (31, -2.500000)
      Hylian Shroom, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
        (fused 3) (num_ingredients 3) (num_effect_ingredients 1)
        (random_effects ())))
      169 pts (31, -2.500000)
      Apple, Hyrule Herb, Sundelion
      (Food
       ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
        (fused 3) (num_ingredients 3) (num_effect_ingredients 1)
        (random_effects ()))) |}];

  test ~kind:Sunny ~game:TOTK ~max_hearts:7 ~max_stamina:10 ~gloomy_hearts:5 ~category:Any ~algo:Balanced
    [
      Sundelion, 1;
      Sun_pumpkin, 2;
      Fairy, 1;
      Monster_horn Octorok_tentacle, 1;
      Monster_guts Moblin_guts, 1;
      Monster_fang Gibdo_guts, 1;
    ];
  test ~kind:Sunny ~game:TOTK ~max_hearts:7 ~max_stamina:10 ~gloomy_hearts:5 ~category:Any ~algo:Maximize
    [
      Sundelion, 1;
      Sun_pumpkin, 2;
      Fairy, 1;
      Monster_horn Octorok_tentacle, 1;
      Monster_guts Moblin_guts, 1;
      Monster_fang Gibdo_guts, 1;
    ];
  [%expect
    {|
    (0s)
    193 pts (46, -2.000000)
    Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 8))) (stamina Nothing) (effect Nothing)
      (fused 3) (num_ingredients 3) (num_effect_ingredients 3)
      (random_effects ())))
    188 pts (46, -3.000000)
    Fairy, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 48))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 3)
      (random_effects ())))
    187 pts (46, -4.000000)
    Fairy, Gibdo Guts, Sun Pumpkin x2, Sundelion
    (Tonic
     ((hearts (Unglooms 5 (Quarters 48))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ())))
    (0s)
    193 pts (46, -2.000000)
    Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 8))) (stamina Nothing) (effect Nothing)
      (fused 3) (num_ingredients 3) (num_effect_ingredients 3)
      (random_effects ())))
    172 pts (46, -3.000000)
    Fairy, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 48))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 3)
      (random_effects ())))
    171 pts (46, -4.000000)
    Fairy, Gibdo Guts, Sun Pumpkin x2, Sundelion
    (Tonic
     ((hearts (Unglooms 5 (Quarters 48))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ()))) |}];
  test ~kind:Sunny ~game:TOTK ~max_hearts:7 ~max_stamina:10 ~gloomy_hearts:5 ~category:Any ~algo:Balanced
    [ Sundelion, 2; Sun_pumpkin, 2; Fairy, 1; Skyshroom, 4; Raw_prime_meat, 1; Goat_butter, 1 ];
  test ~kind:Sunny ~game:TOTK ~max_hearts:7 ~max_stamina:10 ~gloomy_hearts:5 ~category:Any ~algo:Maximize
    [ Sundelion, 2; Sun_pumpkin, 2; Fairy, 1; Skyshroom, 4; Raw_prime_meat, 1; Goat_butter, 1 ];
  [%expect
    {|
    (0s)
    216 pts (638, -2.500000)
    Raw Prime Meat, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 20))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 3)
      (random_effects ())))
    213 pts (638, -2.750000)
    Raw Prime Meat, Skyshroom, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 22))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ())))
    199 pts (638, -2.000000)
    Skyshroom x2, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 12))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ())))
    (0s)
    219 pts (638, -2.750000)
    Raw Prime Meat, Skyshroom, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 22))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ())))
    216 pts (638, -2.500000)
    Raw Prime Meat, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 20))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 3)
      (random_effects ())))
    199 pts (638, -2.000000)
    Skyshroom x2, Sun Pumpkin x2, Sundelion
    (Food
     ((hearts (Unglooms 5 (Quarters 12))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects ()))) |}];
  let data10 =
    Glossary.
      [
        Sundelion, 3;
        Sun_pumpkin, 3;
        Palm_fruit, 5;
        Apple, 5;
        Golden_apple, 5;
        Rock_salt, 5;
        Hylian_tomato, 5;
        Raw_meat, 5;
      ]
  in
  test ~kind:Sunny ~game:TOTK ~max_hearts:5 ~max_stamina:10 ~gloomy_hearts:4 ~category:Any ~algo:Balanced
    data10;
  test ~kind:Sunny ~game:TOTK ~max_hearts:5 ~max_stamina:10 ~gloomy_hearts:4 ~category:Any ~algo:Maximize
    data10;
  [%expect
    {|
    (0s)
    192 pts (3472, -1.066667)
    Apple, Palm Fruit, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 16))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 2)
      (random_effects ())))
    191 pts (3472, -1.266667)
    Apple x3, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 16))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ())))
    188 pts (3472, -1.066667)
    Palm Fruit x2, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 20))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 2)
      (random_effects ())))
    (0s)
    200 pts (3472, -1.066667)
    Palm Fruit x2, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 20))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 2)
      (random_effects ())))
    199 pts (3472, -1.266667)
    Apple x2, Palm Fruit, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 20))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ())))
    192 pts (3472, -1.066667)
    Apple, Palm Fruit, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 16))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 2)
      (random_effects ()))) |}];

  let data11 =
    Glossary.
      [
        Sundelion, 1;
        Sun_pumpkin, 3;
        Raw_bird_drumstick, 1;
        Bird_egg, 2;
        Raw_prime_meat, 1;
        Sanke_carp, 1;
        Apple, 1;
      ]
  in
  test ~kind:Sunny ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:4 ~category:Any ~algo:Maximize
    data11;
  [%expect
    {|
    (0s)
    223 pts (218, -4.333333)
    Raw Bird Drumstick, Raw Prime Meat, Sanke Carp, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 32))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ())))
    215 pts (218, -4.333333)
    Apple, Raw Bird Drumstick, Raw Prime Meat, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 28))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ())))
    215 pts (218, -4.333333)
    Apple, Raw Prime Meat, Sanke Carp, Sun Pumpkin, Sundelion
    (Food
     ((hearts (Unglooms 4 (Quarters 28))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ()))) |}];

  test ~kind:Neutral ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize data11;
  [%expect
    {|
    (0s)
    40 pts (381, 6.000000)
    Apple, Bird Egg x2, Raw Bird Drumstick, Raw Prime Meat
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ())))
    40 pts (381, 6.000000)
    Apple, Bird Egg x2, Raw Prime Meat, Sanke Carp
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ())))
    40 pts (381, 5.500000)
    Apple, Bird Egg, Raw Bird Drumstick, Raw Prime Meat, Sanke Carp
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ()))) |}];

  test ~kind:Neutral ~game:TOTK ~max_hearts:30 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [ Raw_gourmet_meat, 5; Palm_fruit, 1; Raw_bird_drumstick, 1 ];
  [%expect
    {|
    (0s)
    120 pts (119, 29.000000)
    Raw Gourmet Meat x5
    (Food
     ((hearts (Restores (Quarters 120))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ())))
    104 pts (119, 24.200000)
    Raw Bird Drumstick, Raw Gourmet Meat x4
    (Food
     ((hearts (Restores (Quarters 104))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ())))
    104 pts (119, 24.200000)
    Palm Fruit, Raw Gourmet Meat x4
    (Food
     ((hearts (Restores (Quarters 104))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 0)
      (random_effects ()))) |}];

  test ~kind:Neutral ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [ Apple, 2; Raw_bird_drumstick, 1; Chillfin_trout, 2; Zapshroom, 1 ];
  [%expect
    {|
      (0s)
      29 pts (62, 4.500000)
      Apple, Chillfin Trout x2, Raw Bird Drumstick, Zapshroom
      (Food
       ((hearts (Restores (Quarters 32))) (stamina Nothing) (effect Nothing)
        (fused 5) (num_ingredients 5) (num_effect_ingredients 3)
        (random_effects ())))
      26 pts (62, 3.500000)
      Apple x2, Chillfin Trout, Raw Bird Drumstick, Zapshroom
      (Food
       ((hearts (Restores (Quarters 28))) (stamina Nothing) (effect Nothing)
        (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
        (random_effects ())))
      25 pts (62, 4.000000)
      Chillfin Trout x2, Raw Bird Drumstick, Zapshroom
      (Food
       ((hearts (Restores (Quarters 28))) (stamina Nothing) (effect Nothing)
        (fused 4) (num_ingredients 4) (num_effect_ingredients 3)
        (random_effects ()))) |}];

  test ~kind:Neutral ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [ Hylian_tomato, 3; Raw_bird_drumstick, 1; Chillshroom, 2; Zapshroom, 1 ];
  [%expect
    {|
    (0s)
    32 pts (119, 6.000000)
    Hylian Tomato x3, Raw Bird Drumstick
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing) (effect Nothing)
      (fused 4) (num_ingredients 4) (num_effect_ingredients 0)
      (random_effects ())))
    30 pts (119, 5.500000)
    Chillshroom, Hylian Tomato x3, Zapshroom
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ())))
    30 pts (119, 4.833333)
    Chillshroom, Hylian Tomato x2, Raw Bird Drumstick, Zapshroom
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing) (effect Nothing)
      (fused 5) (num_ingredients 5) (num_effect_ingredients 2)
      (random_effects ()))) |}];

  test ~kind:Mighty ~game:TOTK ~max_hearts:16 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [
      Bird_egg, 3;
      Goron_spice, 3;
      Mighty_bananas, 5;
      Mighty_carp, 5;
      Mighty_porgy, 5;
      Razorclaw_crab, 5;
      Star_fragment, 2;
      Dragon_horns Farosh, 2;
      Dragon_fangs Farosh, 1;
    ];
  [%expect
    {|
    (0s)
    1072 pts (83684, 7.900000)
    Mighty Porgy, Razorclaw Crab x2, Shard of Farosh's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    1072 pts (83684, 7.900000)
    Mighty Carp x2, Mighty Porgy, Shard of Farosh's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    1072 pts (83684, 7.900000)
    Mighty Carp, Mighty Porgy, Razorclaw Crab, Shard of Farosh's Horn
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950)))) (fused 29)
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts)))) |}];

  test ~kind:Mighty ~game:TOTK ~max_hearts:16 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [
      Bird_egg, 3;
      Goron_spice, 3;
      Mighty_carp, 5;
      Mighty_porgy, 5;
      Razorclaw_crab, 5;
      Star_fragment, 2;
      Dragon_fangs Farosh, 1;
    ];
  [%expect
    {|
    (0s)
    808 pts (21700, 6.733333)
    Bird Egg, Goron Spice, Mighty Porgy, Razorclaw Crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (21700, 6.733333)
    Bird Egg, Goron Spice, Mighty Carp x2, Mighty Porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    808 pts (21700, 6.733333)
    Bird Egg, Goron Spice, Mighty Carp, Mighty Porgy, Razorclaw Crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];

  test ~kind:Mighty ~game:TOTK ~max_hearts:16 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [
      Bird_egg, 3;
      Goron_spice, 3;
      Mighty_carp, 5;
      Mighty_porgy, 5;
      Razorclaw_crab, 5;
      Star_fragment, 2;
      Dragon_fangs Farosh, 1;
    ];
  [%expect
    {|
    (0s)
    960 pts (21700, 8.066667)
    Bird Egg, Mighty Porgy, Razorclaw Crab x2, Shard of Farosh's Fang
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870)))) (fused 24)
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    960 pts (21700, 8.066667)
    Bird Egg, Mighty Carp x2, Mighty Porgy, Shard of Farosh's Fang
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870)))) (fused 24)
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    960 pts (21700, 8.066667)
    Bird Egg, Mighty Carp, Mighty Porgy, Razorclaw Crab, Shard of Farosh's Fang
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870)))) (fused 24)
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts)))) |}];

  test ~kind:Chilly ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [
      Chillshroom, 3;
      Cold_darner, 3;
      Monster_fang Moblin_fang, 3;
      Monster_horn Moblin_horn, 3;
      Monster_guts Moblin_guts, 3;
    ];
  [%expect
    {|
    (0s)
    569 pts (1600, 2.000000)
    Chillshroom x3
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Chilly ((potency 2) (wasted 0) (duration 450)))) (fused 3)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    562 pts (1600, -1.333333)
    Cold Darner x3, Moblin Horn
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Chilly ((potency 2) (wasted 0) (duration 520)))) (fused 9)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ())))
    549 pts (1600, -1.333333)
    Cold Darner x3, Moblin Fang
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Chilly ((potency 2) (wasted 0) (duration 560)))) (fused 7)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ()))) |}];

  let data12 =
    Glossary.
      [
        Hot_footed_frog, 2;
        Hightail_lizard, 2;
        Monster_fang Like_like_stone, 3;
        Monster_fang Ice_like_stone, 3;
        Monster_fang Fire_like_stone, 3;
      ]
  in
  test ~kind:Hasty ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Balanced
    data12;
  test ~kind:Hasty ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Maximize
    data12;
  [%expect
    {|
    (0s)
    555 pts (219, -2.166667)
    Hightail Lizard, Hot Footed Frog x2, Like Like Stone x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 400)))) (fused 11)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    547 pts (219, -2.166667)
    Hightail Lizard, Hot Footed Frog x2, Ice Like Stone, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 400)))) (fused 19)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    545 pts (219, -1.833333)
    Hightail Lizard, Hot Footed Frog x2, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 290)))) (fused 7)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ())))
    (0s)
    648 pts (219, -2.166667)
    Hightail Lizard, Hot Footed Frog x2, Like Like Stone x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 400)))) (fused 11)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    642 pts (219, -2.333333)
    Hightail Lizard x2, Hot Footed Frog x2, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 1) (duration 350)))) (fused 8)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    640 pts (219, -2.166667)
    Hightail Lizard, Hot Footed Frog x2, Ice Like Stone, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 400)))) (fused 19)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];

  let data13 =
    Glossary.
      [
        Hot_footed_frog, 2;
        Hightail_lizard, 2;
        Monster_horn Aerocuda_wing, 3;
        Monster_fang Like_like_stone, 3;
        Monster_guts Hinox_guts, 3;
      ]
  in
  test ~kind:Hasty ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Balanced
    data13;
  test ~kind:Hasty ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Maximize
    data13;
  [%expect
    {|
    (0s)
    568 pts (2380, -2.166667)
    Hightail Lizard, Hinox Guts, Hot Footed Frog x2, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 480)))) (fused 8)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    563 pts (2380, -2.166667)
    Aerocuda Wing, Hightail Lizard, Hinox Guts, Hot Footed Frog x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 440)))) (fused 8)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    558 pts (2380, -1.833333)
    Hightail Lizard, Hinox Guts, Hot Footed Frog x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 370)))) (fused 4)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ())))
    (0s)
    674 pts (2380, -2.166667)
    Hightail Lizard, Hinox Guts x2, Hot Footed Frog x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 560)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    661 pts (2380, -2.166667)
    Hightail Lizard, Hinox Guts, Hot Footed Frog x2, Like Like Stone
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 480)))) (fused 8)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    656 pts (2380, -2.166667)
    Aerocuda Wing, Hightail Lizard, Hinox Guts, Hot Footed Frog x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Hasty ((potency 2) (wasted 0) (duration 440)))) (fused 8)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];

  let data14 =
    Glossary.
      [
        Fireproof_lizard, 5;
        Smotherwing_butterfly, 5;
        Monster_guts Moblin_guts, 5;
        Monster_horn Chuchu_jelly, 5;
      ]
  in
  test ~kind:Fireproof ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced data14;
  test ~kind:Fireproof ~game:TOTK ~max_hearts:10 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize data14;
  [%expect
    {|
    (0s)
    477 pts (6885, -1.000000)
    Chuchu Jelly, Fireproof Lizard, Smotherwing Butterfly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 0) (duration 670)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    461 pts (6885, -1.000000)
    Chuchu Jelly, Smotherwing Butterfly x4
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 1) (duration 670)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    432 pts (6885, -1.000000)
    Fireproof Lizard, Moblin Guts, Smotherwing Butterfly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 0) (duration 790)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    (0s)
    701 pts (6885, -1.000000)
    Fireproof Lizard, Moblin Guts, Smotherwing Butterfly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 0) (duration 790)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    700 pts (6885, -1.000000)
    Moblin Guts, Smotherwing Butterfly x4
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 1) (duration 790)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ())))
    686 pts (6885, -1.000000)
    Chuchu Jelly, Fireproof Lizard, Smotherwing Butterfly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Fireproof ((potency 2) (wasted 0) (duration 670)))) (fused 5)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}];

  let data15 =
    Glossary.
      [
        Hearty_radish, 5;
        Big_hearty_radish, 5;
        Hearty_truffle, 5;
        Big_hearty_truffle, 5;
        Hearty_salmon, 5;
        Hearty_bass, 5;
        Hearty_blueshell_snail, 5;
      ]
  in
  (* For BOTW 34 is not possible in the UI *)
  test ~kind:Hearty ~game:BOTW ~max_hearts:34 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize data15;
  test ~kind:Hearty ~game:TOTK ~max_hearts:34 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize data15;
  [%expect
    {|
    (0s)
    209 pts (384167, -0.200000)
    Hearty Truffle
    (Food
     ((hearts (Full_plus_bonus 1)) (stamina Nothing) (effect Nothing) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    207 pts (384167, -0.200000)
    Hearty Bass
    (Food
     ((hearts (Full_plus_bonus 2)) (stamina Nothing) (effect Nothing) (fused 1)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    206 pts (384167, -0.400000)
    Hearty Truffle x2
    (Food
     ((hearts (Full_plus_bonus 2)) (stamina Nothing) (effect Nothing) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ())))
    (0s)
    258 pts (384167, -0.400000)
    Hearty Blueshell Snail x2
    (Food
     ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ())))
    258 pts (384167, -0.400000)
    Hearty Radish x2
    (Food
     ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ())))
    258 pts (384167, -0.400000)
    Hearty Blueshell Snail, Hearty Radish
    (Food
     ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing) (fused 2)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}]

let%expect_test "Scoring" =
  let test_hearts ?(max_hearts = 20) ?(gloomy_hearts = 3) ~algo x =
    print_endline (sprintf !"%{sexp: Cooking.Hearts.t}" x);
    Cooking.Hearts.score ~max_hearts ~gloomy_hearts ~game:Game.TOTK ~algo x
    |> Int.to_string
    |> print_endline
  in
  test_hearts (Unglooms (3, Quarters 4)) ~algo:Balanced;
  test_hearts (Unglooms (3, Quarters 4)) ~algo:Maximize;
  [%expect {|
    (Unglooms 3 (Quarters 4))
    156
    (Unglooms 3 (Quarters 4))
    156 |}];
  test_hearts (Unglooms (3, Quarters 8)) ~algo:Balanced;
  test_hearts (Unglooms (3, Quarters 8)) ~algo:Maximize;
  [%expect {|
    (Unglooms 3 (Quarters 8))
    164
    (Unglooms 3 (Quarters 8))
    164 |}];
  test_hearts (Unglooms (4, Quarters 4)) ~algo:Balanced;
  test_hearts (Unglooms (4, Quarters 4)) ~algo:Maximize;
  [%expect {|
    (Unglooms 4 (Quarters 4))
    148
    (Unglooms 4 (Quarters 4))
    148 |}];
  test_hearts ~max_hearts:4 (Unglooms (3, Quarters 20)) ~algo:Balanced;
  test_hearts ~max_hearts:4 (Unglooms (3, Quarters 20)) ~algo:Maximize;
  [%expect {|
    (Unglooms 3 (Quarters 20))
    164
    (Unglooms 3 (Quarters 20))
    168 |}];
  test_hearts (Full_plus_bonus 3) ~algo:Balanced;
  test_hearts (Full_plus_bonus 3) ~algo:Maximize;
  [%expect {|
    (Full_plus_bonus 3)
    192
    (Full_plus_bonus 3)
    192 |}]

let%expect_test "Optimize" =
  let grouped = Items.Table.of_alist_exn [ Stamella_shroom, 4; Armored_carp, 2; Ironshroom, 1 ] in
  let test ll =
    List.map ll
      ~f:
        (Tuple2.map_snd
           ~f:
             (List.map ~f:(fun ll ->
                  List.map ll ~f:(Tuple2.map_fst ~f:Glossary.to_ingredient) |> Ingredient.Map.of_alist_exn)))
    |> Optimize.top_sort grouped
    |> sprintf !"%{sexp: Optimize.iteration list}"
    |> print_endline
  in
  test
    [
      50, [ [ Stamella_shroom, 1 ] ];
      10, [ [ Armored_carp, 1 ]; [ Ironshroom, 1 ]; [ Stamella_shroom, 3; Ironshroom, 1 ] ];
      3, [ [ Ironshroom, 1 ] ];
    ];
  [%expect
    {|
    (((tiebreaker 0.75) (rarity 0.25) (score 50)
      (recipe
       ((((item Stamella_shroom) (hearts (Always (Quarters 4)))
          (effect (Energizing (Fifths 7))) (category Food) (critical false)
          (fused 1))
         1))))
     ((tiebreaker 2.25) (rarity 1.75) (score 10)
      (recipe
       ((((item Stamella_shroom) (hearts (Always (Quarters 4)))
          (effect (Energizing (Fifths 7))) (category Food) (critical false)
          (fused 1))
         3)
        (((item Ironshroom) (hearts (Always (Quarters 4)))
          (effect (Tough ((duration (Always 50)) (points 2)))) (category Food)
          (critical false) (fused 1))
         1))))
     ((tiebreaker 1.5) (rarity 0.5) (score 10)
      (recipe
       ((((item Armored_carp) (hearts (Always (Quarters 8)))
          (effect (Tough ((duration (Always 50)) (points 2)))) (category Food)
          (critical false) (fused 1))
         1))))) |}]
