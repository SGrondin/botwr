open! Core_kernel

let make_food hearts sec = Ingredient.{ hearts; effect = Neutral (Always sec); category = Food }

let make_ingredient hearts first next =
  Ingredient.{ hearts; effect = Neutral (Diminishing { first; next }); category = Food }

let make_spice hearts first next =
  Ingredient.{ hearts; effect = Neutral (Diminishing { first; next }); category = Spice }

let make_hearty hearts bonus = Ingredient.{ hearts; effect = Hearty bonus; category = Food }

let make_energizing hearts ((bonus, _, _, _, _) as scaling) =
  Ingredient.{ hearts; effect = Energizing { bonus; scaling }; category = Food }

let make_enduring hearts ((bonus, _, _, _, _) as scaling) =
  Ingredient.{ hearts; effect = Enduring { bonus; scaling }; category = Food }

let make_critter ?(hearts = 0) effect = Ingredient.{ hearts; effect; category = Critter }

type t =
  | Palm_fruit
  | Apple
  | Wildberry
  | Hylian_shroom
  | Hyrule_herb
  | Hyrule_bass
  | Sanke_carp
  | Raw_gourmet_meat
  | Raw_whole_bird
  | Raw_prime_meat
  | Raw_bird_thigh
  | Raw_meat
  | Raw_bird_drumstick
  | Bird_egg
  | Fresh_milk
  | Acorn
  | Chickaloo_tree_nut
  | Hylian_rice
  | Tabantha_wheat
  | Cane_sugar
  | Goat_butter
  | Goron_spice
  | Rock_salt
  | Hearty_truffle
  | Hearty_bass
  | Hearty_radish
  | Hearty_blueshel_snail
  | Hearty_durian
  | Big_hearty_truffle
  | Hearty_salmon
  | Hearty_lizard
  | Big_hearty_radish
  | Stamella_shroom
  | Restless_cricket
  | Courser_bee_honey
  | Bight_eyed_crab
  | Staminoka_bass
  | Energetic_rhino_beetle
  | Endura_shroom
  | Tireless_frog
  | Endura_carrot
[@@deriving sexp, compare]

let to_ingredient = function
| Palm_fruit -> make_food 2 30
| Apple -> make_food 1 30
| Wildberry -> make_food 1 30
| Hylian_shroom -> make_food 1 30
| Hyrule_herb -> make_food 2 30
| Hyrule_bass -> make_food 2 30
| Sanke_carp -> make_food 2 30
| Raw_gourmet_meat -> make_food 6 30
| Raw_whole_bird -> make_food 6 30
| Raw_prime_meat -> make_food 3 30
| Raw_bird_thigh -> make_food 3 30
| Raw_meat -> make_food 2 30
| Raw_bird_drumstick -> make_food 2 30
| Bird_egg -> make_ingredient 2 60 30
| Fresh_milk -> make_ingredient 1 80 30
| Acorn -> make_ingredient 1 50 30
| Chickaloo_tree_nut -> make_ingredient 1 40 30
| Hylian_rice -> make_spice 2 60 30
| Tabantha_wheat -> make_spice 2 60 30
| Cane_sugar -> make_spice 0 80 30
| Goat_butter -> make_spice 0 80 30
| Goron_spice -> make_spice 0 90 30
| Rock_salt -> make_spice 0 60 30
| Hearty_truffle -> make_hearty 4 1
| Hearty_bass -> make_hearty 4 2
| Hearty_radish -> make_hearty 5 3
| Hearty_blueshel_snail -> make_hearty 6 3
| Hearty_durian -> make_hearty 6 4
| Big_hearty_truffle -> make_hearty 6 4
| Hearty_salmon -> make_hearty 8 4
| Hearty_lizard -> make_critter ~hearts:8 (Hearty 4)
| Big_hearty_radish -> make_hearty 8 5
| Stamella_shroom -> make_energizing 1 (1, 2, 4, 5, 7)
| Restless_cricket -> make_critter (Energizing { bonus = 1; scaling = 1, 2, 4, 5, 5 })
| Courser_bee_honey -> make_energizing 4 (2, 5, 8, 11, 14)
| Bight_eyed_crab -> make_energizing 2 (2, 5, 8, 11, 14)
| Staminoka_bass -> make_energizing 2 (5, 11, 15, 15, 15)
| Energetic_rhino_beetle -> make_critter (Energizing { bonus = 8; scaling = 8, 15, 15, 15, 15 })
| Endura_shroom -> make_enduring 2 (1, 1, 1, 2, 2)
| Tireless_frog -> make_critter ~hearts:4 (Enduring { bonus = 1; scaling = 1, 2, 3, 4, 4 })
| Endura_carrot -> make_enduring 4 (2, 4, 6, 8, 10)
