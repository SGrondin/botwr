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
    |> List.sort ~compare:Glossary.compare
    |> sprintf !"%{sexp: Glossary.t list}"
    |> print_endline
  in
  test Electro Elixirs data1;
  [%expect
    {|
    (Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
     Thunderwing_butterfly Electric_darner Electric_darner Electric_darner
     Electric_darner (Monster_fang Bokoblin_fang) (Monster_fang Ancient_gear)
     (Monster_guts Moblin_guts) (Monster_guts Hinox_guts)) |}];
  test Electro Meals data1;
  [%expect
    {|
    (Raw_meat Fresh_milk Goron_spice Bird_egg Electric_safflina Electric_safflina
     Electric_safflina Electric_safflina Electric_safflina) |}];
  test Electro Any data1;
  [%expect
    {|
    (Raw_meat Fresh_milk Goron_spice Bird_egg Electric_safflina Electric_safflina
     Electric_safflina Electric_safflina Electric_safflina Thunderwing_butterfly
     Thunderwing_butterfly Thunderwing_butterfly Thunderwing_butterfly
     Electric_darner Electric_darner Electric_darner Electric_darner
     (Monster_fang Bokoblin_fang) (Monster_fang Ancient_gear)
     (Monster_guts Moblin_guts) (Monster_guts Hinox_guts)) |}];
  test Hearty Meals data1;
  [%expect
    {|
    (Hearty_bass Hearty_bass Hearty_durian Hearty_durian Hearty_durian
     Hearty_durian Hearty_durian Big_hearty_truffle Big_hearty_truffle
     Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle Hearty_salmon
     Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon Big_hearty_radish
     Big_hearty_radish Big_hearty_radish Big_hearty_radish Big_hearty_radish) |}];
  test Hearty Elixirs data1;
  [%expect
    {|
    (Hearty_lizard Hearty_lizard Hearty_lizard Hearty_lizard
     (Monster_horn Lizalfos_horn) (Monster_horn Keese_wing)
     (Monster_horn Keese_wing) (Monster_horn Keese_wing)) |}];
  test Hearty Any data1;
  [%expect
    {|
    (Hearty_bass Hearty_bass Hearty_durian Hearty_durian Hearty_durian
     Hearty_durian Hearty_durian Big_hearty_truffle Big_hearty_truffle
     Big_hearty_truffle Big_hearty_truffle Big_hearty_truffle Hearty_salmon
     Hearty_salmon Hearty_salmon Hearty_salmon Hearty_salmon Hearty_lizard
     Hearty_lizard Hearty_lizard Hearty_lizard Big_hearty_radish
     Big_hearty_radish Big_hearty_radish Big_hearty_radish Big_hearty_radish
     (Monster_horn Lizalfos_horn) (Monster_horn Keese_wing)
     (Monster_horn Keese_wing) (Monster_horn Keese_wing)) |}];
  test Enduring Meals data1;
  [%expect
    {|
    (Raw_meat Raw_meat Raw_meat Fresh_milk Endura_shroom Endura_shroom
     Endura_shroom Endura_shroom Endura_shroom Endura_carrot Endura_carrot
     Endura_carrot Endura_carrot Endura_carrot) |}];
  test Enduring Elixirs data1;
  [%expect
    {|
    (Tireless_frog Tireless_frog Tireless_frog Tireless_frog
     (Monster_horn Lizalfos_horn) (Monster_horn Keese_wing)
     (Monster_horn Keese_wing) (Monster_horn Keese_wing)) |}];
  test Enduring Any data1;
  [%expect
    {|
    (Raw_meat Raw_meat Raw_meat Fresh_milk Endura_shroom Endura_shroom
     Endura_shroom Endura_shroom Endura_shroom Tireless_frog Tireless_frog
     Tireless_frog Tireless_frog Endura_carrot Endura_carrot Endura_carrot
     Endura_carrot Endura_carrot (Monster_horn Lizalfos_horn)
     (Monster_horn Keese_wing) (Monster_horn Keese_wing)
     (Monster_horn Keese_wing)) |}];
  test Enduring Any
    (Items.Table.of_alist_exn [ Endura_carrot, 1; Dragon_horns Farosh, 0; Endura_shroom, 0 ]);
  [%expect {| (Endura_carrot) |}];
  test Enduring Any (Items.Table.of_alist_exn [ Endura_carrot, 1; Star_fragment, 0; Endura_shroom, 0 ]);
  [%expect {| (Endura_carrot) |}]

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
    237 pts (637, 5.333333)
    Armored_carp, Ironshell_crab x2, Ironshroom x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    237 pts (637, 5.000000)
    Ironshell_crab x2, Ironshroom x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    237 pts (637, 4.500000)
    Armored_carp, Ironshell_crab, Ironshroom x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Tough ((potency 3) (wasted 1) (duration 250))))
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
    141 pts (381, 4.791667)
    Stamella_shroom x5
    (Food
     ((hearts (Restores (Quarters 20)))
      (stamina (Restores ((potency 7) (wasted 0)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    125 pts (381, 3.833333)
    Stamella_shroom x4
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Restores ((potency 5) (wasted 3)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    124 pts (381, 4.633333)
    Apple, Stamella_shroom x4
    (Food
     ((hearts (Restores (Quarters 20)))
      (stamina (Restores ((potency 5) (wasted 3)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}];
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
    259 pts (174436, 5.666667)
    Mighty_porgy, Razorclaw_crab x2
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    259 pts (174436, 5.666667)
    Mighty_carp x2, Mighty_porgy
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    259 pts (174436, 5.666667)
    Mighty_carp, Mighty_porgy, Razorclaw_crab
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 150))))
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
    318 pts (174436, 9.444444)
    Mighty_porgy x5
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 8) (duration 250))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    318 pts (174436, 9.444444)
    Razorclaw_crab x5
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 3) (duration 250))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    318 pts (174436, 9.444444)
    Mighty_porgy, Razorclaw_crab x4
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 4) (duration 250))))
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
    323 pts (2379, 2.100000)
    Fresh_milk, Goat_butter, Goron_spice, Voltfin_trout x2
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 550))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    317 pts (2379, 8.100000)
    Fresh_milk, Goron_spice, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    317 pts (2379, 7.600000)
    Goat_butter, Goron_spice, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500))))
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
    380 pts (2379, 9.000000)
    Voltfin_trout x5
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 9) (duration 750))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    380 pts (2379, 7.200000)
    Voltfin_trout x4, Zapshroom
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 8) (duration 750))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    380 pts (2379, 7.200000)
    Voltfin_trout x4, Voltfruit
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 7) (duration 750))))
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
      323 pts (2379, 2.957143)
      Fresh_milk, Goat_butter, Goron_spice, Voltfin_trout x2
      (Food
       ((hearts (Restores (Quarters 20))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 550))))
        (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
      317 pts (2379, 8.957143)
      Fresh_milk, Goron_spice, Raw_whole_bird, Voltfin_trout x2
      (Food
       ((hearts (Restores (Quarters 44))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 500))))
        (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
      317 pts (2379, 7.600000)
      Goat_butter, Goron_spice, Raw_whole_bird, Voltfin_trout x2
      (Food
       ((hearts (Restores (Quarters 40))) (stamina Nothing)
        (effect (Electro ((potency 3) (wasted 0) (duration 500))))
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
    317 pts (2379, 8.957143)
    Fresh_milk, Goron_spice, Raw_whole_bird, Voltfin_trout x2
    (Food
     ((hearts (Restores (Quarters 44))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 500))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    316 pts (2379, 1.657143)
    Fresh_milk, Goron_spice, Voltfin_trout, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 620))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    316 pts (2379, 1.157143)
    Electric_safflina, Fresh_milk, Goron_spice, Voltfin_trout, Zapshroom
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 620))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [ Electric_safflina, 1; Zapshroom, 1; Bird_egg, 7; Goron_spice, 1; Apple, 2; Raw_whole_bird, 2 ];
  [%expect
    {|
    (0s)
    191 pts (62, 10.857143)
    Bird_egg, Electric_safflina, Goron_spice, Raw_whole_bird x2
    (Food
     ((hearts (Restores (Quarters 56))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 390))))
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    190 pts (62, 11.857143)
    Bird_egg, Goron_spice, Raw_whole_bird x2, Zapshroom
    (Food
     ((hearts (Restores (Quarters 60))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 1) (duration 390))))
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    189 pts (62, 5.357143)
    Bird_egg, Electric_safflina, Goron_spice, Raw_whole_bird
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 360))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Electro ~category:Meals ~algo:Balanced
    [ Voltfruit, 1; Zapshroom, 1; Apple, 2; Raw_whole_bird, 1 ];
  [%expect
    {|
    (0s)
    174 pts (31, 6.000000)
    Apple x2, Raw_whole_bird, Voltfruit
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 0) (duration 240))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    173 pts (31, 6.000000)
    Apple x2, Raw_whole_bird, Zapshroom
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 1) (duration 240))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    173 pts (31, 6.000000)
    Apple x2, Raw_whole_bird, Voltfruit, Zapshroom
    (Food
     ((hearts (Restores (Quarters 40))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 2) (duration 390))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Hearty ~category:Meals ~algo:Balanced [ Big_hearty_truffle, 5 ];
  [%expect
    {|
    (0s)
    155 pts (31, -0.600000)
    Big_hearty_truffle x3
    (Food
     ((hearts (Full_plus_bonus 12)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    154 pts (31, -0.400000)
    Big_hearty_truffle x2
    (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ())))
    150 pts (31, -0.800000)
    Big_hearty_truffle x4
    (Food
     ((hearts (Full_plus_bonus 16)) (stamina Nothing) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ()))) |}];
  test ~kind:Energizing ~category:Meals ~algo:Balanced [ Staminoka_bass, 5; Stamella_shroom, 5; Apple, 5 ];
  [%expect
    {|
    (0s)
    206 pts (3472, 5.400000)
    Staminoka_bass x3
    (Food
     ((hearts (Restores (Quarters 24)))
      (stamina (Restores ((potency 16) (wasted 4)))) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    205 pts (3472, 6.200000)
    Apple, Staminoka_bass x3
    (Food
     ((hearts (Restores (Quarters 28)))
      (stamina (Restores ((potency 16) (wasted 4)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ())))
    204 pts (3472, 7.000000)
    Apple x2, Staminoka_bass x3
    (Food
     ((hearts (Restores (Quarters 32)))
      (stamina (Restores ((potency 16) (wasted 4)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
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
    253 pts (2379, -1.133333)
    Monster_guts x4, Sunset_firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 880))))
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    230 pts (2379, -0.933333)
    Monster_guts x3, Sunset_firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 690))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    227 pts (2379, -1.266667)
    Monster_guts x3, Sunset_firefly x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 810))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize data5;
  [%expect
    {|
    (0s)
    269 pts (2379, -1.133333)
    Monster_guts x4, Sunset_firefly
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 880))))
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    260 pts (2379, -1.266667)
    Monster_guts x3, Sunset_firefly x2
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 810))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    251 pts (2379, -1.400000)
    Monster_guts x2, Sunset_firefly x3
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 2) (duration 740))))
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
    263 pts (1585, 1.500000)
    Dragon_claws, Dragon_fangs, Fresh_milk, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 26))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1130))))
      (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects (Potency Duration Red_hearts))))
    263 pts (1585, 0.500000)
    Dragon_claws, Dragon_fangs, Goat_butter, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 22))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1130))))
      (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects (Potency Duration Red_hearts))))
    263 pts (1585, 0.500000)
    Cane_sugar, Dragon_claws, Dragon_fangs, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 22))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1130))))
      (num_ingredients 5) (num_effect_ingredients 3)
      (random_effects (Potency Duration Red_hearts)))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize data6;
  [%expect
    {|
    (0s)
    399 pts (1585, 2.500000)
    Dragon_horns, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 19))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1920))))
      (num_ingredients 2) (num_effect_ingredients 2)
      (random_effects (Potency Duration Red_hearts))))
    398 pts (1585, 12.000000)
    Dragon_horns, Fairy, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 59))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 1950))))
      (num_ingredients 3) (num_effect_ingredients 2)
      (random_effects (Potency Duration Red_hearts))))
    398 pts (1585, 4.500000)
    Dragon_fangs, Dragon_horns, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 29))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 2550))))
      (num_ingredients 3) (num_effect_ingredients 3)
      (random_effects (Potency Duration Red_hearts)))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Balanced ~use_special:false data6;
  [%expect
    {|
    (0s)
    198 pts (62, -2.500000)
    Cane_sugar, Fresh_milk, Goat_butter, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 450))))
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ())))
    189 pts (62, -1.500000)
    Fresh_milk, Goat_butter, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 370))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    189 pts (62, -1.500000)
    Cane_sugar, Fresh_milk, Goron_spice, Silent_shroom
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 1) (duration 370))))
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Sneaky ~category:Any ~algo:Maximize ~use_special:false data6;
  [%expect
    {|
    (0s)
    220 pts (62, -1.000000)
    Fresh_milk, Goat_butter, Goron_spice, Silent_shroom x2
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    220 pts (62, -1.000000)
    Cane_sugar, Fresh_milk, Goron_spice, Silent_shroom x2
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ())))
    220 pts (62, -2.000000)
    Cane_sugar, Goat_butter, Goron_spice, Silent_shroom x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 3) (duration 490))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Enduring ~category:Any ~algo:Balanced [ Endura_shroom, 5 ];
  [%expect
    {|
    (0s)
    225 pts (31, 1.800000)
    Endura_shroom
    (Food
     ((hearts (Restores (Quarters 8)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    224 pts (31, 7.200000)
    Endura_shroom x4
    (Food
     ((hearts (Restores (Quarters 32)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    222 pts (31, 3.600000)
    Endura_shroom x2
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];
  test ~kind:Hearty ~category:Any ~algo:Balanced [ Hearty_bass, 5 ];
  [%expect
    {|
    (0s)
    155 pts (31, -1.000000)
    Hearty_bass x5
    (Food
     ((hearts (Full_plus_bonus 10)) (stamina Nothing) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    152 pts (31, -0.800000)
    Hearty_bass x4
    (Food
     ((hearts (Full_plus_bonus 8)) (stamina Nothing) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    149 pts (31, -0.600000)
    Hearty_bass x3
    (Food
     ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Enduring ~category:Any ~algo:Balanced [ Endura_carrot, 3; Endura_shroom, 5; Apple, 2 ];
  [%expect
    {|
    (0s)
    261 pts (637, 14.600000)
    Endura_carrot x3, Endura_shroom x2
    (Food
     ((hearts (Restores (Quarters 64)))
      (stamina (Full_plus_bonus ((potency 7) (wasted 0)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    259 pts (637, 11.000000)
    Endura_carrot x3
    (Food
     ((hearts (Restores (Quarters 48)))
      (stamina (Full_plus_bonus ((potency 6) (wasted 0)))) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    258 pts (637, 11.500000)
    Apple, Endura_carrot x3
    (Food
     ((hearts (Restores (Quarters 52)))
      (stamina (Full_plus_bonus ((potency 6) (wasted 0)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ()))) |}];
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
    280 pts (1023, 5.000000)
    Bird_egg, Goron_spice, Mighty_porgy, Razorclaw_crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    280 pts (1023, 5.000000)
    Bird_egg, Goron_spice, Mighty_carp x2, Mighty_porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    280 pts (1023, 5.000000)
    Bird_egg, Goron_spice, Mighty_carp, Mighty_porgy, Razorclaw_crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];
  test ~kind:Mighty ~category:Any ~algo:Balanced data7;
  [%expect
    {|
    (0s)
    343 pts (2379, 7.000000)
    Bird_egg, Dragon_fangs, Mighty_porgy, Razorclaw_crab x2
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870))))
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    343 pts (2379, 7.000000)
    Bird_egg, Dragon_fangs, Mighty_carp x2, Mighty_porgy
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870))))
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    343 pts (2379, 7.000000)
    Bird_egg, Dragon_fangs, Mighty_carp, Mighty_porgy, Razorclaw_crab
    (Food
     ((hearts (Restores (Quarters 42))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 870))))
      (num_ingredients 5) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts)))) |}];
  test ~kind:Mighty ~category:Any ~algo:Maximize data7;
  [%expect
    {|
    (0s)
    525 pts (2379, 7.000000)
    Dragon_horns, Mighty_porgy x3
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 2) (duration 1950))))
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    525 pts (2379, 7.000000)
    Dragon_horns, Mighty_porgy, Razorclaw_crab x2
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 1950))))
      (num_ingredients 4) (num_effect_ingredients 4)
      (random_effects (Potency Duration Red_hearts))))
    525 pts (2379, 7.000000)
    Dragon_horns, Mighty_porgy x2, Razorclaw_crab
    (Food
     ((hearts (Restores (Quarters 39))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 1) (duration 1950))))
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
    280 pts (1585, 5.000000)
    Bird_egg, Goron_spice, Mighty_porgy, Razorclaw_crab x2
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    280 pts (1585, 5.000000)
    Bird_egg, Goron_spice, Mighty_carp x2, Mighty_porgy
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ())))
    280 pts (1585, 5.000000)
    Bird_egg, Goron_spice, Mighty_carp, Mighty_porgy, Razorclaw_crab
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 0) (duration 330))))
      (num_ingredients 5) (num_effect_ingredients 3) (random_effects ()))) |}];

  let data9 = Glossary.[ Apple, 3; Endura_carrot, 1 ] in
  test ~kind:Enduring ~category:Any ~algo:Balanced data9;
  [%expect
    {|
    (0s)
    233 pts (15, 3.000000)
    Endura_carrot
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    232 pts (15, 3.666667)
    Apple, Endura_carrot
    (Food
     ((hearts (Restores (Quarters 20)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 1) (random_effects ())))
    231 pts (15, 4.333333)
    Apple x2, Endura_carrot
    (Food
     ((hearts (Restores (Quarters 24)))
      (stamina (Full_plus_bonus ((potency 2) (wasted 0)))) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ()))) |}];
  test ~kind:Sunny ~game:TOTK ~max_hearts:5 ~max_stamina:5 ~gloomy_hearts:3 ~category:Any ~algo:Balanced
    [ Apple, 1; Hylian_shroom, 1; Hyrule_herb, 1; Sundelion, 2 ];
  [%expect
    {|
    (0s)
    176 pts (31, -3.500000)
    Apple, Hylian_shroom, Hyrule_herb, Sundelion
    (Food
     ((hearts (Unglooms 3 (Quarters 16))) (stamina Nothing) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ())))
    169 pts (31, -2.500000)
    Hylian_shroom, Hyrule_herb, Sundelion
    (Food
     ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ())))
    169 pts (31, -2.500000)
    Apple, Hyrule_herb, Sundelion
    (Food
     ((hearts (Unglooms 3 (Quarters 12))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 1) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:8 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any ~algo:Balanced
    [ Glowing_cave_fish, 4; Brightcap, 2 ];
  [%expect
    {|
    (0s)
    284 pts (62, 5.750000)
    Brightcap, Glowing_cave_fish x3
    (Food
     ((hearts (Restores (Quarters 28))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 0) (duration 480))))
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    283 pts (62, 7.000000)
    Glowing_cave_fish x4
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 480))))
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    281 pts (62, 6.250000)
    Brightcap x2, Glowing_cave_fish x3
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 600))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 2; Brightcap, 2 ];
  [%expect
    {|
    (0s)
    222 pts (15, 3.500000)
    Brightcap, Glowing_cave_fish x2
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 0) (duration 360))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    219 pts (15, 4.000000)
    Brightcap x2, Glowing_cave_fish x2
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 480))))
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    162 pts (15, 0.500000)
    Brightcap
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 0) (duration 120))))
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 3 ];
  [%expect
    {|
    (0s)
    221 pts (7, 5.000000)
    Glowing_cave_fish x3
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 360))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    161 pts (7, 1.666667)
    Glowing_cave_fish
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 120))))
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    157 pts (7, 3.333333)
    Glowing_cave_fish x2
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 3) (duration 240))))
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Glowing_cave_fish, 4 ];
  [%expect
    {|
    (0s)
    283 pts (15, 7.000000)
    Glowing_cave_fish x4
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Bright ((potency 3) (wasted 1) (duration 480))))
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    221 pts (15, 5.250000)
    Glowing_cave_fish x3
    (Food
     ((hearts (Restores (Quarters 24))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 1) (duration 360))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    161 pts (15, 1.750000)
    Glowing_cave_fish
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 120))))
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Maximize
    [ Brightcap, 4 ];
  [%expect
    {|
    (0s)
    220 pts (15, 3.000000)
    Brightcap x4
    (Food
     ((hearts (Restores (Quarters 16))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 3) (duration 480))))
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ())))
    206 pts (15, 2.250000)
    Brightcap x3
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 2) (duration 360))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ())))
    192 pts (15, 1.500000)
    Brightcap x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 240))))
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];

  test ~kind:Bright ~game:TOTK ~max_hearts:20 ~max_stamina:10 ~gloomy_hearts:0 ~category:Any
    ~algo:Balanced
    [ Brightcap, 5 ];
  [%expect
    {|
    (0s)
    218 pts (31, 4.000000)
    Brightcap x5
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Bright ((potency 2) (wasted 0) (duration 600))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ())))
    162 pts (31, 0.800000)
    Brightcap
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 0) (duration 120))))
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ())))
    159 pts (31, 1.600000)
    Brightcap x2
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Bright ((potency 1) (wasted 1) (duration 240))))
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}]

let%expect_test "Scoring" =
  let test_hearts ?(max_hearts = 20) ?(gloomy_hearts = 3) x =
    print_endline (sprintf !"%{sexp: Cooking.Hearts.t}" x);
    Cooking.Hearts.score ~max_hearts ~gloomy_hearts x |> Int.to_string |> print_endline
  in
  Cooking.debug := true;
  test_hearts (Unglooms (3, Quarters 4));
  [%expect {|
    (Unglooms 3 (Quarters 4))
    156 |}];
  test_hearts (Unglooms (3, Quarters 8));
  [%expect {|
    (Unglooms 3 (Quarters 8))
    164 |}];
  test_hearts (Unglooms (4, Quarters 4));
  [%expect {|
    (Unglooms 4 (Quarters 4))
    148 |}];
  test_hearts ~max_hearts:4 (Unglooms (3, Quarters 20));
  [%expect {|
    (Unglooms 3 (Quarters 20))
    176 |}];
  test_hearts (Full_plus_bonus 3);
  [%expect {|
    (Full_plus_bonus 3)
    146 |}];

  Cooking.debug := false

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
    (((tiebreaker 0.75) (rarity 0.25) (score 50) (recipe ((Stamella_shroom 1))))
     ((tiebreaker 2.25) (rarity 1.75) (score 10)
      (recipe ((Stamella_shroom 3) (Ironshroom 1))))
     ((tiebreaker 1.5) (rarity 0.5) (score 10) (recipe ((Armored_carp 1))))) |}]
