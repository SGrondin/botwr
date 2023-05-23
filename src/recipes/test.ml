open! Core_kernel
open Cooking

let%expect_test "List of ingredients" =
  let open Glossary in
  let list_to_map ll =
    List.fold ll ~init:Glossary.Map.empty
      ~f:(Glossary.Map.update ~f:(Option.value_map ~default:1 ~f:succ))
  in
  let test ll = ll |> list_to_map |> cook |> sprintf !"%{sexp: Cooking.t}" |> print_endline in
  test [];
  [%expect {| (Failed "No ingredients") |}];
  test [ Endura_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 8)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Endura_shroom; Endura_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 16)))
      (stamina (Full_plus_bonus ((potency 1) (wasted 0)))) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Endura_shroom; Endura_shroom; Big_hearty_radish ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 48))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Big_hearty_radish ];
  [%expect
    {|
    (Food
     ((hearts (Full_plus_bonus 5)) (stamina Nothing) (effect Nothing)
      (num_ingredients 1) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Hylian_rice; Bird_egg; Raw_meat; Staminoka_bass ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 32)))
      (stamina (Restores ((potency 5) (wasted 3)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina; Thunderwing_butterfly ];
  [%expect {| Dubious |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Electro ((potency 1) (wasted 2) (duration 450))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Chillshroom; Chillshroom; Hydromelon; Hydromelon; Hydromelon ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Chilly ((potency 2) (wasted 1) (duration 750))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  test [ Monster_fang Moblin_fang; Bladed_rhino_beetle; Bladed_rhino_beetle; Bladed_rhino_beetle ];
  [%expect
    {|
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Mighty ((potency 1) (wasted 2) (duration 260))))
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Stamella_shroom; Stamella_shroom; Stamella_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 12)))
      (stamina (Restores ((potency 4) (wasted 1)))) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Stamella_shroom; Goat_butter; Goat_butter; Goat_butter; Goat_butter ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 4)))
      (stamina (Restores ((potency 1) (wasted 2)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Goron_spice; Voltfin_trout; Voltfin_trout; Bird_egg; Bird_egg ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 32))) (stamina Nothing)
      (effect (Electro ((potency 3) (wasted 0) (duration 510))))
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Apple; Raw_gourmet_meat; Raw_gourmet_meat; Raw_gourmet_meat; Stamella_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 80)))
      (stamina (Restores ((potency 1) (wasted 2)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Staminoka_bass; Staminoka_bass; Staminoka_bass; Stamella_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 28)))
      (stamina (Restores ((potency 18) (wasted 1)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 4) (random_effects ()))) |}];
  test [ Fairy ];
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 28))) (stamina Nothing) (effect Nothing)
      (num_ingredients 1) (num_effect_ingredients 0) (random_effects ()))) |}];
  test [ Fairy; Fairy ];
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 68))) (stamina Nothing) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 0) (random_effects ()))) |}];
  test [ Staminoka_bass; Summerwing_butterfly ];
  [%expect {| Dubious |}];
  test [ Staminoka_bass; Summerwing_butterfly; Fairy ];
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 36))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Staminoka_bass; Restless_cricket; Fairy ];
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 36)))
      (stamina (Restores ((potency 7) (wasted 0)))) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Endura_carrot; Endura_carrot; Endura_carrot; Endura_carrot; Fairy ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 104)))
      (stamina (Full_plus_bonus ((potency 8) (wasted 0)))) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}];
  test [ Staminoka_bass; Staminoka_bass; Staminoka_bass; Fairy ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 64)))
      (stamina (Restores ((potency 16) (wasted 4)))) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Hearty_lizard; Fairy ];
  [%expect
    {|
    (Tonic
     ((hearts (Full_plus_bonus 4)) (stamina Nothing) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Hearty_lizard; Hearty_bass; Fairy ];
  [%expect
    {|
        (Tonic
         ((hearts (Full_plus_bonus 6)) (stamina Nothing) (effect Nothing)
          (num_ingredients 3) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Bird_egg; Blue_nightshade ];
  (* Tested in game *)
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 8))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 210))))
      (num_ingredients 2) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Hearty_lizard; Staminoka_bass; Fairy ];
  (* Tested in game *)
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 68))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Hydromelon; Hydromelon; Chillshroom; Chillshroom; Chillshroom ];
  (* Tested in game *)
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 20))) (stamina Nothing)
      (effect (Chilly ((potency 2) (wasted 2) (duration 750))))
      (num_ingredients 5) (num_effect_ingredients 5) (random_effects ()))) |}];
  test [ Rushroom; Rushroom; Swift_carrot ];
  (* Tested in game *)
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 12))) (stamina Nothing)
      (effect (Hasty ((potency 1) (wasted 2) (duration 180))))
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Fairy; Monster_guts Moblin_guts; Monster_guts Moblin_guts; Silent_shroom; Sunset_firefly ];
  (* Tested in game *)
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 32))) (stamina Nothing) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Fairy; Monster_guts Moblin_guts; Monster_guts Moblin_guts; Sunset_firefly ];
  (* Tested in game *)
  [%expect
    {|
    (Tonic
     ((hearts (Restores (Quarters 28))) (stamina Nothing) (effect Nothing)
      (num_ingredients 4) (num_effect_ingredients 1) (random_effects ()))) |}];
  test [ Fairy; Sneaky_river_snail; Silent_princess; Silent_princess; Silent_princess ];
  (* Tested in game *)
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 72))) (stamina Nothing)
      (effect (Sneaky ((potency 3) (wasted 1) (duration 510))))
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}];
  test [ Fairy; Sneaky_river_snail ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 48))) (stamina Nothing)
      (effect (Sneaky ((potency 1) (wasted 0) (duration 150))))
      (num_ingredients 2) (num_effect_ingredients 1) (random_effects ())))
          |}];
  test [ Hearty_radish; Star_fragment ];
  [%expect
    {|
    (Food
     ((hearts (Full_plus_bonus 4)) (stamina Nothing) (effect Nothing)
      (num_ingredients 2) (num_effect_ingredients 2) (random_effects ()))) |}];
  test [ Swift_carrot; Star_fragment ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 4))) (stamina Nothing)
      (effect (Hasty ((potency 1) (wasted 0) (duration 90)))) (num_ingredients 2)
      (num_effect_ingredients 2) (random_effects (Potency Duration Red_hearts)))) |}];
  test [ Swift_carrot; Hearty_radish; Star_fragment ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 36))) (stamina Nothing) (effect Nothing)
      (num_ingredients 3) (num_effect_ingredients 3) (random_effects ()))) |}];
  test [ Fairy; Razorclaw_crab; Razorclaw_crab; Razorclaw_crab; Razorshroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores (Quarters 68))) (stamina Nothing)
      (effect (Mighty ((potency 3) (wasted 1) (duration 230))))
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}];
  test [ Fairy; Sticky_frog; Sticky_frog; Sticky_frog; Sticky_lizard ];
  [%expect {|
    (Tonic
     ((hearts (Restores (Quarters 28))) (stamina Nothing) (effect Nothing)
      (num_ingredients 5) (num_effect_ingredients 4) (random_effects ()))) |}]

let%expect_test "Combinations" =
  let test r ll =
    let init = Combinations.Recipe.Set.empty in
    let f acc recipe = Combinations.Recipe.Set.add acc recipe in
    Combinations.generate ~init ~f r ll
    |> Combinations.Recipe.Set.to_list
    |> Combinations.Recipe.to_string_many
    |> print_endline
  in
  let open Glossary in
  let list1 = [ Apple; Palm_fruit; Apple; Cane_sugar; Goat_butter ] in
  test 1 list1;
  [%expect {|
    0. Apple
    1. Cane_sugar
    2. Goat_butter
    3. Palm_fruit |}];
  test 2 list1;
  [%expect
    {|
    0. Apple x2
    1. Apple, Cane_sugar
    2. Apple, Goat_butter
    3. Apple, Palm_fruit
    4. Cane_sugar, Goat_butter
    5. Cane_sugar, Palm_fruit
    6. Goat_butter, Palm_fruit |}];
  test 3 list1;
  [%expect
    {|
    0. Apple x2, Cane_sugar
    1. Apple x2, Goat_butter
    2. Apple x2, Palm_fruit
    3. Apple, Cane_sugar, Goat_butter
    4. Apple, Cane_sugar, Palm_fruit
    5. Apple, Goat_butter, Palm_fruit
    6. Cane_sugar, Goat_butter, Palm_fruit |}];
  test 4 list1;
  [%expect
    {|
    0. Apple x2, Cane_sugar, Goat_butter
    1. Apple x2, Cane_sugar, Palm_fruit
    2. Apple x2, Goat_butter, Palm_fruit
    3. Apple, Cane_sugar, Goat_butter, Palm_fruit |}];
  test 5 list1;
  [%expect {| 0. Apple x2, Cane_sugar, Goat_butter, Palm_fruit |}];
  let list2 = [ Apple; Palm_fruit; Apple; Palm_fruit; Wildberry ] in
  test 1 list2;
  [%expect {|
    0. Apple
    1. Palm_fruit
    2. Wildberry |}];
  test 2 list2;
  [%expect
    {|
    0. Apple x2
    1. Apple, Palm_fruit
    2. Apple, Wildberry
    3. Palm_fruit x2
    4. Palm_fruit, Wildberry |}];
  test 3 list2;
  [%expect
    {|
    0. Apple x2, Palm_fruit
    1. Apple x2, Wildberry
    2. Apple, Palm_fruit x2
    3. Apple, Palm_fruit, Wildberry
    4. Palm_fruit x2, Wildberry |}];
  test 4 list2;
  [%expect
    {|
    0. Apple x2, Palm_fruit x2
    1. Apple x2, Palm_fruit, Wildberry
    2. Apple, Palm_fruit x2, Wildberry |}];
  test 5 list2;
  [%expect {| 0. Apple x2, Palm_fruit x2, Wildberry |}];
  let list3 = [ Apple; Palm_fruit; Apple; Palm_fruit; Apple ] in
  test 1 list3;
  [%expect {|
    0. Apple
    1. Palm_fruit |}];
  test 2 list3;
  [%expect {|
    0. Apple x2
    1. Apple, Palm_fruit
    2. Palm_fruit x2 |}];
  test 3 list3;
  [%expect {|
    0. Apple x2, Palm_fruit
    1. Apple x3
    2. Apple, Palm_fruit x2 |}];
  test 4 list3;
  [%expect {|
    0. Apple x2, Palm_fruit x2
    1. Apple x3, Palm_fruit |}];
  test 5 list3;
  [%expect {| 0. Apple x3, Palm_fruit x2 |}];
  test 6 list3;
  [%expect {||}]

let%expect_test "All basic combinations" =
  let test r ll =
    let init = Combinations.Recipe.Set.empty in
    let f acc recipe = Combinations.Recipe.Set.add acc recipe in
    Combinations.generate_all ~init ~f r ll
    |> Combinations.Recipe.Set.to_list
    |> Combinations.Recipe.to_string_many
    |> print_endline
  in
  let count r ll =
    let init = Combinations.Recipe.Set.empty in
    let f acc recipe = Combinations.Recipe.Set.add acc recipe in
    Combinations.generate_all ~init ~f r ll
    |> Combinations.Recipe.Set.length
    |> Int.to_string
    |> print_endline
  in
  test 3 [ Apple; Apple; Palm_fruit; Goat_butter ];
  [%expect
    {|
    0. Apple
    1. Apple x2
    2. Apple x2, Goat_butter
    3. Apple x2, Palm_fruit
    4. Apple, Goat_butter
    5. Apple, Goat_butter, Palm_fruit
    6. Apple, Palm_fruit
    7. Goat_butter
    8. Goat_butter, Palm_fruit
    9. Palm_fruit |}];
  count 5
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
      Fresh_milk;
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
      Hearty_blueshell_snail;
      Hearty_durian;
      Big_hearty_truffle;
      Hearty_salmon;
      Apple;
      Goat_butter;
      Palm_fruit;
    ];
  [%expect {| 50557 |}]
