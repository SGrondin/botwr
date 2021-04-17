open! Core_kernel
open Cooking

let%expect_test "List of ingredients" =
  let open Glossary in
  let max_hearts = 20 in
  let max_stamina = 15 in
  let list_to_map ll =
    List.fold ll ~init:Glossary.Map.empty
      ~f:(Glossary.Map.update ~f:(Option.value_map ~default:1 ~f:succ))
  in
  let test ll =
    ll |> list_to_map |> cook ~max_hearts ~max_stamina |> sprintf !"%{sexp: Cooking.t}" |> print_endline
  in
  test [];
  [%expect {| (Failed "No ingredients") |}];
  test [ Endura_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores 2)) (stamina (Full_plus_bonus 1)) (effect Nothing)
      (num_ingredients 1))) |}];
  test [ Endura_shroom; Endura_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores 4)) (stamina (Full_plus_bonus 1)) (effect Nothing)
      (num_ingredients 2))) |}];
  test [ Endura_shroom; Endura_shroom; Big_hearty_radish ];
  [%expect
    {|
    (Food
     ((hearts (Restores 12)) (stamina Nothing) (effect Nothing)
      (num_ingredients 3))) |}];
  test [ Big_hearty_radish ];
  [%expect
    {|
    (Food
     ((hearts (Full_plus_bonus 5)) (stamina Nothing) (effect Nothing)
      (num_ingredients 1))) |}];
  test [ Hylian_rice; Bird_egg; Raw_meat; Staminoka_bass ];
  [%expect
    {|
    (Food
     ((hearts (Restores 8)) (stamina (Restores 5)) (effect Nothing)
      (num_ingredients 4))) |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina; Thunderwing_butterfly ];
  [%expect {| Dubious |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina ];
  [%expect
    {|
    (Food
     ((hearts (Restores 1)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 450)))) (num_ingredients 3))) |}];
  test [ Chillshroom; Chillshroom; Hydromelon; Hydromelon; Hydromelon ];
  [%expect
    {|
    (Food
     ((hearts (Restores 5)) (stamina Nothing)
      (effect (Chilly ((potency 2) (duration 750)))) (num_ingredients 5))) |}];
  test [ Monster_fang Moblin_fang; Bladed_rhino_beetle; Bladed_rhino_beetle; Bladed_rhino_beetle ];
  [%expect
    {|
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Mighty ((potency 1) (duration 260)))) (num_ingredients 4))) |}];
  test [ Stamella_shroom; Stamella_shroom; Stamella_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores 3)) (stamina (Restores 4)) (effect Nothing)
      (num_ingredients 3))) |}];
  test [ Stamella_shroom; Goat_butter; Goat_butter; Goat_butter; Goat_butter ];
  [%expect
    {|
    (Food
     ((hearts (Restores 1)) (stamina (Restores 1)) (effect Nothing)
      (num_ingredients 5))) |}];
  test [ Apple; Raw_gourmet_meat; Raw_gourmet_meat; Raw_gourmet_meat; Stamella_shroom ];
  [%expect
    {|
    (Food
     ((hearts (Restores 20)) (stamina (Restores 1)) (effect Nothing)
      (num_ingredients 5))) |}]

let%expect_test "Hashing" =
  let test x = x |> Combinations.Recipe.hash |> Int.to_string |> print_endline in
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Apple, 1; Apple, 1 ]);
  [%expect {| 374989641 |}];
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Apple, 2 ]);
  [%expect {| 374989641 |}];
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Apple, 1 ]);
  [%expect {| 877695341 |}];
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Apple, 1; Palm_fruit, 1 ]);
  [%expect {| 209963781 |}];
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Palm_fruit, 1; Apple, 1 ]);
  [%expect {| 209963781 |}];
  test (Glossary.Map.of_alist_reduce ~f:( + ) Glossary.[ Palm_fruit, 1 ]);
  [%expect {| 1002037630 |}]

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
