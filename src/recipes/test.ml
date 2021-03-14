open! Core_kernel
open Cooking

let%expect_test "List of ingredients" =
  let open Glossary in
  let test ll = ll |> cook |> sprintf !"%{sexp: Cooking.t}" |> print_endline in
  test [];
  [%expect {| (Failed "No ingredients") |}];
  test [ Endura_shroom ];
  [%expect {|
    (Food ((hearts (Restores 2)) (stamina (Full_plus_bonus 1)) (effect Nothing))) |}];
  test [ Endura_shroom; Endura_shroom ];
  [%expect {|
    (Food ((hearts (Restores 4)) (stamina (Full_plus_bonus 1)) (effect Nothing))) |}];
  test [ Endura_shroom; Endura_shroom; Big_hearty_radish ];
  [%expect {| (Food ((hearts (Restores 12)) (stamina Nothing) (effect Nothing))) |}];
  test [ Big_hearty_radish ];
  [%expect {| (Food ((hearts (Full_plus_bonus 5)) (stamina Nothing) (effect Nothing))) |}];
  test [ Hylian_rice; Bird_egg; Raw_meat; Staminoka_bass ];
  [%expect {|
    (Food ((hearts (Restores 8)) (stamina (Restores 5)) (effect Nothing))) |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina; Thunderwing_butterfly ];
  [%expect {| Dubious |}];
  test [ Voltfruit; Electric_safflina; Electric_safflina ];
  [%expect
    {|
    (Food
     ((hearts (Restores 1)) (stamina Nothing)
      (effect (Electro ((potency 2) (duration 450)))))) |}];
  test [ Chillshroom; Chillshroom; Hydromelon; Hydromelon; Hydromelon ];
  [%expect
    {|
    (Food
     ((hearts (Restores 5)) (stamina Nothing)
      (effect (Chilly ((potency 2) (duration 750)))))) |}];
  test [ Monster_fang; Bladed_rhino_beetle; Bladed_rhino_beetle; Bladed_rhino_beetle ];
  [%expect
    {|
    (Elixir
     ((hearts Nothing) (stamina Nothing)
      (effect (Mighty ((potency 1) (duration 260)))))) |}]

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
    Combinations.Basic.generate r ll
    |> Combinations.Basic.Table.keys
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
    Combinations.Basic.generate_all r ll
    |> Combinations.Basic.Table.keys
    |> Combinations.Recipe.to_string_many
    |> print_endline
  in
  let count r ll =
    Combinations.Basic.generate_all r ll
    |> Combinations.Basic.Table.length
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
      Hearty_blueshel_snail;
      Hearty_durian;
      Big_hearty_truffle;
      Hearty_salmon;
      Apple;
      Goat_butter;
      Palm_fruit;
    ];
  [%expect {| 50557 |}]

let%expect_test "Combinations of combinations" =
  let test r ll =
    Combinations.Advanced.generate r ll
    |> Combinations.Advanced.Every.Table.data
    |> List.map ~f:(fun map ->
           Combinations.Advanced.Recipes.Map.data map
           |> List.map ~f:(fun { recipe; num } ->
                  sprintf "-- %dx -- %s" num (Combinations.Recipe.to_string recipe))
           |> String.concat ~sep:"\n")
    |> String.concat ~sep:"\n-------------------------------\n"
    |> print_endline
  in
  let count r ll =
    Combinations.Advanced.generate r ll
    |> Combinations.Advanced.Every.Table.length
    |> sprintf "%d"
    |> print_endline
  in
  let open Glossary in
  let list1 = [ Apple; Cane_sugar; Apple; Goat_butter; Palm_fruit ] in
  count 5 list1;
  [%expect {| 63 |}];
  test 5 list1;
  [%expect
    {|
    -- 1x -- Goat_butter, Palm_fruit
    -- 2x -- Apple
    -------------------------------
    -- 1x -- Apple x2, Cane_sugar, Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Cane_sugar
    -- 2x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Palm_fruit
    -- 1x -- Cane_sugar
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple, Goat_butter
    -- 1x -- Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple, Goat_butter, Palm_fruit
    -- 1x -- Cane_sugar
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple, Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple x2, Cane_sugar, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple x2, Palm_fruit
    -------------------------------
    -- 1x -- Apple
    -- 1x -- Cane_sugar, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple x2
    -- 1x -- Palm_fruit
    -------------------------------
    -- 2x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar
    -- 2x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar, Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Apple x2, Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple x2, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar
    -- 1x -- Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter, Palm_fruit
    -- 1x -- Cane_sugar
    -------------------------------
    -- 1x -- Cane_sugar, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple x2, Cane_sugar, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter
    -- 1x -- Cane_sugar
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Cane_sugar, Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Apple, Palm_fruit
    -- 1x -- Cane_sugar
    -------------------------------
    -- 1x -- Cane_sugar
    -- 1x -- Goat_butter, Palm_fruit
    -- 2x -- Apple
    -------------------------------
    -- 1x -- Apple, Palm_fruit
    -- 1x -- Apple, Cane_sugar
    -------------------------------
    -- 1x -- Apple, Goat_butter
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar, Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple x2, Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar, Goat_butter
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 2x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Cane_sugar
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar, Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar
    -- 1x -- Goat_butter, Palm_fruit
    -- 1x -- Apple
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Cane_sugar
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple x2, Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple x2
    -- 1x -- Goat_butter, Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Cane_sugar
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Goat_butter, Palm_fruit
    -- 1x -- Apple x2, Cane_sugar
    -------------------------------
    -- 1x -- Goat_butter
    -- 1x -- Apple, Cane_sugar
    -- 1x -- Apple
    -- 1x -- Palm_fruit
    -------------------------------
    -- 1x -- Apple, Goat_butter, Palm_fruit
    -- 1x -- Apple, Cane_sugar
    -------------------------------
    -- 1x -- Apple x2
    -- 1x -- Goat_butter
    -- 1x -- Palm_fruit |}];
  let list2 =
    [
      Apple;
      Cane_sugar;
      Apple;
      Cane_sugar;
      Apple;
      Cane_sugar;
      (* Raw_prime_meat;
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
         Hearty_blueshel_snail;
         Hearty_durian;
         Big_hearty_truffle;
         Hearty_salmon;
         Apple;
         Goat_butter;
         Palm_fruit; *)
    ]
  in
  count 5 list2;
  [%expect {| 76 |}]

let%expect_test "Cooking by category" =
  let test x = x |> Cooking.run |> sprintf !"%{sexp: Glossary.t list}" |> print_endline in
  test Glossary.[ Apple, 1; Palm_fruit, 7; Apple, 1; Ironshell_crab, 1; Mighty_carp, 1 ];
  [%expect
    {|
    (Ironshell_crab Apple Apple Palm_fruit Palm_fruit Palm_fruit Palm_fruit
     Palm_fruit) |}]
