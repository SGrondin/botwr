open! Core_kernel

let always (min, sec) = Ingredient.Duration.Always ((min * 60) + sec)

let make_food hearts sec = Ingredient.{ hearts; effect = Neutral (Always sec); category = Food }

let make_ingredient hearts first next =
  Ingredient.{ hearts; effect = Neutral (Diminishing { first; next }); category = Food }

let make_spice hearts first next =
  Ingredient.{ hearts; effect = Neutral (Diminishing { first; next }); category = Spice }

let make_hearty hearts bonus = Ingredient.{ hearts; effect = Hearty bonus; category = Food }

let make_effect variant dur hearts ((potency, _, _, _, _) as scaling) =
  Ingredient.
    {
      hearts;
      effect = variant Effect.Activity.{ duration = always dur; potency; scaling };
      category = Food;
    }

let make_energizing hearts ((bonus, _, _, _, _) as scaling) =
  Ingredient.{ hearts; effect = Energizing { bonus; scaling }; category = Food }

let make_enduring hearts ((bonus, _, _, _, _) as scaling) =
  Ingredient.{ hearts; effect = Enduring { bonus; scaling }; category = Food }

let make_spicy = make_effect Ingredient.Effect.spicy (2, 30)

let make_chilly = make_effect Ingredient.Effect.chilly (2, 30)

let make_electro = make_effect Ingredient.Effect.electro (2, 30)

let make_hasty = make_effect Ingredient.Effect.hasty (1, 0)

let make_sneaky = make_effect Ingredient.Effect.sneaky (2, 0)

let make_mighty = make_effect Ingredient.Effect.mighty (0, 50)

let make_tough = make_effect Ingredient.Effect.tough (0, 50)

let bonus_critter ?(hearts = 0) variant ((bonus, _, _, _, _) as scaling) =
  Ingredient.{ hearts; effect = variant Effect.Bonus.{ bonus; scaling }; category = Critter }

let effect_critter ?(hearts = 0) dur variant ((potency, _, _, _, _) as scaling) =
  Ingredient.
    {
      hearts;
      effect = variant Effect.Activity.{ duration = always dur; potency; scaling };
      category = Critter;
    }

let make_monster dur = Ingredient.{ hearts = 0; effect = Neutral (always dur); category = Monster }

let cached_monster_horn = make_monster (1, 10)

let cached_monster_fang = make_monster (1, 50)

let cached_monster_guts = make_monster (3, 10)

include Items

let has_effect = function
| Palm_fruit
 |Apple
 |Wildberry
 |Hylian_shroom
 |Hyrule_herb
 |Hyrule_bass
 |Sanke_carp
 |Raw_gourmet_meat
 |Raw_whole_bird
 |Raw_prime_meat
 |Raw_bird_thigh
 |Raw_meat
 |Raw_bird_drumstick
 |Bird_egg
 |Fresh_milk
 |Acorn
 |Chickaloo_tree_nut
 |Hylian_rice
 |Tabantha_wheat
 |Cane_sugar
 |Goat_butter
 |Goron_spice
 |Rock_salt ->
  false
| Hearty_truffle
 |Hearty_bass
 |Hearty_radish
 |Hearty_blueshell_snail
 |Hearty_durian
 |Big_hearty_truffle
 |Hearty_salmon
 |Hearty_lizard
 |Big_hearty_radish
 |Stamella_shroom
 |Restless_cricket
 |Courser_bee_honey
 |Bright_eyed_crab
 |Staminoka_bass
 |Energetic_rhino_beetle
 |Endura_shroom
 |Tireless_frog
 |Endura_carrot
 |Spicy_pepper
 |Warm_safflina
 |Summerwing_butterfly
 |Sunshroom
 |Warm_darner
 |Sizzlefin_trout
 |Hydromelon
 |Cool_safflina
 |Winterwing_butterfly
 |Chillshroom
 |Cold_darner
 |Chillfin_trout
 |Voltfruit
 |Electric_safflina
 |Thunderwing_butterfly
 |Zapshroom
 |Electric_darner
 |Voltfin_trout
 |Fireproof_lizard
 |Smotherwing_butterfly
 |Rushroom
 |Swift_carrot
 |Hightail_lizard
 |Fleet_lotus_seeds
 |Swift_violet
 |Hot_footed_frog
 |Blue_nightshade
 |Sneaky_river_snail
 |Sunset_firefly
 |Silent_shroom
 |Stealthfin_trout
 |Silent_princess
 |Mighty_thistle
 |Bladed_rhino_beetle
 |Mighty_bananas
 |Razorshroom
 |Mighty_carp
 |Razorclaw_crab
 |Mighty_porgy
 |Armoranth
 |Rugged_rhino_beetle
 |Fortified_pumpkin
 |Ironshroom
 |Armored_carp
 |Ironshell_crab
 |Armored_porgy ->
  true
| Monster_horn _
 |Monster_fang _
 |Monster_guts _ ->
  false

let do_to_ingredient = function
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
| Hearty_blueshell_snail -> make_hearty 6 3
| Hearty_durian -> make_hearty 6 4
| Big_hearty_truffle -> make_hearty 6 4
| Hearty_salmon -> make_hearty 8 4
| Hearty_lizard -> Ingredient.{ hearts = 8; effect = Hearty 4; category = Critter }
| Big_hearty_radish -> make_hearty 8 5
| Stamella_shroom -> make_energizing 1 (1, 2, 4, 5, 7)
| Restless_cricket -> bonus_critter Ingredient.Effect.energizing (1, 2, 4, 5, 5)
| Courser_bee_honey -> make_energizing 4 (2, 5, 8, 11, 14)
| Bright_eyed_crab -> make_energizing 2 (2, 5, 8, 11, 14)
| Staminoka_bass -> make_energizing 2 (5, 11, 15, 15, 15)
| Energetic_rhino_beetle -> bonus_critter Ingredient.Effect.energizing (8, 15, 15, 15, 15)
| Endura_shroom -> make_enduring 2 (1, 1, 1, 2, 2)
| Tireless_frog -> bonus_critter ~hearts:4 Ingredient.Effect.enduring (1, 2, 3, 4, 4)
| Endura_carrot -> make_enduring 4 (2, 4, 6, 8, 10)
| Spicy_pepper -> make_spicy 1 (1, 1, 1, 1, 1)
| Warm_safflina -> make_spicy 0 (1, 1, 1, 1, 1)
| Summerwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.spicy (1, 1, 1, 1, 1)
| Sunshroom -> make_spicy 1 (1, 1, 2, 2, 2)
| Warm_darner -> effect_critter (2, 30) Ingredient.Effect.spicy (1, 1, 2, 2, 2)
| Sizzlefin_trout -> make_spicy 2 (1, 2, 2, 2, 2)
| Hydromelon -> make_chilly 1 (1, 1, 1, 1, 1)
| Cool_safflina -> make_chilly 0 (1, 1, 1, 1, 1)
| Winterwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.chilly (1, 1, 1, 1, 1)
| Chillshroom -> make_chilly 1 (1, 1, 2, 2, 2)
| Cold_darner -> effect_critter (2, 30) Ingredient.Effect.chilly (1, 1, 2, 2, 2)
| Chillfin_trout -> make_chilly 2 (1, 2, 2, 2, 2)
| Voltfruit -> make_electro 1 (1, 1, 1, 2, 2)
| Electric_safflina -> make_electro 0 (1, 1, 1, 2, 2)
| Thunderwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.electro (1, 1, 1, 2, 2)
| Zapshroom -> make_electro 1 (1, 2, 3, 3, 3)
| Electric_darner -> effect_critter (2, 30) Ingredient.Effect.electro (1, 2, 3, 3, 3)
| Voltfin_trout -> make_electro 2 (1, 3, 3, 3, 3)
| Fireproof_lizard -> effect_critter (2, 30) Ingredient.Effect.fireproof (1, 1, 1, 1, 1)
| Smotherwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.fireproof (1, 1, 1, 2, 2)
| Rushroom -> make_hasty 1 (1, 1, 1, 1, 2)
| Swift_carrot -> make_hasty 1 (1, 1, 1, 1, 2)
| Hightail_lizard -> effect_critter (1, 0) Ingredient.Effect.hasty (1, 1, 1, 1, 1)
| Fleet_lotus_seeds -> make_hasty 1 (1, 1, 2, 3, 3)
| Swift_violet -> make_hasty 0 (1, 1, 2, 3, 3)
| Hot_footed_frog -> effect_critter (1, 0) Ingredient.Effect.hasty (1, 1, 2, 3, 3)
| Blue_nightshade -> make_sneaky 0 (1, 1, 1, 1, 1)
| Sneaky_river_snail -> make_sneaky 2 (1, 1, 1, 1, 1)
| Sunset_firefly -> effect_critter (2, 0) Ingredient.Effect.sneaky (1, 1, 1, 1, 1)
| Silent_shroom -> make_sneaky 1 (1, 1, 2, 2, 3)
| Stealthfin_trout -> make_sneaky 2 (1, 1, 2, 2, 3)
| Silent_princess -> make_sneaky 2 (1, 2, 3, 3, 3)
| Mighty_thistle -> make_mighty 0 (1, 1, 1, 1, 2)
| Bladed_rhino_beetle -> effect_critter (0, 50) Ingredient.Effect.mighty (1, 1, 1, 1, 1)
| Mighty_bananas -> make_mighty 1 (1, 1, 2, 3, 3)
| Razorshroom -> make_mighty 1 (1, 1, 2, 3, 3)
| Mighty_carp -> make_mighty 2 (1, 1, 2, 3, 3)
| Razorclaw_crab -> make_mighty 2 (1, 1, 2, 3, 3)
| Mighty_porgy -> make_mighty 2 (1, 2, 3, 3, 3)
| Armoranth -> make_tough 0 (1, 1, 1, 1, 2)
| Rugged_rhino_beetle -> effect_critter (0, 50) Ingredient.Effect.tough (1, 1, 1, 1, 1)
| Fortified_pumpkin -> make_tough 1 (1, 1, 2, 3, 3)
| Ironshroom -> make_tough 1 (1, 1, 2, 3, 3)
| Armored_carp -> make_tough 2 (1, 1, 2, 3, 3)
| Ironshell_crab -> make_tough 2 (1, 1, 2, 3, 3)
| Armored_porgy -> make_tough 2 (1, 2, 3, 3, 3)
| Monster_horn _ -> cached_monster_horn
| Monster_fang _ -> cached_monster_fang
| Monster_guts _ -> cached_monster_guts

let to_ingredient =
  let mapped = Array.of_list_map all ~f:do_to_ingredient in
  function
  | Monster_horn _ -> cached_monster_horn
  | Monster_fang _ -> cached_monster_fang
  | Monster_guts _ -> cached_monster_guts
  | x -> mapped.(Variants.to_rank x)

let to_kind = Fn.compose Ingredient.to_kind to_ingredient

module Category = struct
  type t =
    | Meals
    | Elixirs
    | Any
end

let to_string = function
| Palm_fruit -> "Palm Fruit"
| Apple -> "Apple"
| Wildberry -> "Wildberry"
| Hylian_shroom -> "Hylian Shroom"
| Hyrule_herb -> "Hyrule Herb"
| Hyrule_bass -> "Hyrule Bass"
| Sanke_carp -> "Sanke Carp"
| Raw_gourmet_meat -> "Raw Gourmet Meat"
| Raw_whole_bird -> "Raw Whole Bird"
| Raw_prime_meat -> "Raw Prime Meat"
| Raw_bird_thigh -> "Raw Bird Thigh"
| Raw_meat -> "Raw Meat"
| Raw_bird_drumstick -> "Raw Bird Drumstick"
| Bird_egg -> "Bird Egg"
| Fresh_milk -> "Fresh Milk"
| Acorn -> "Acorn"
| Chickaloo_tree_nut -> "Chickaloo Tree Nut"
| Hylian_rice -> "Hylian Rice"
| Tabantha_wheat -> "Tabantha Wheat"
| Cane_sugar -> "Cane Sugar"
| Goat_butter -> "Goat Butter"
| Goron_spice -> "Goron Spice"
| Rock_salt -> "Rock Salt"
| Hearty_truffle -> "Hearty Truffle"
| Hearty_bass -> "Hearty Bass"
| Hearty_radish -> "Hearty Radish"
| Hearty_blueshell_snail -> "Hearty Blueshell Snail"
| Hearty_durian -> "Hearty Durian"
| Big_hearty_truffle -> "Big Hearty Truffle"
| Hearty_salmon -> "Hearty Salmon"
| Hearty_lizard -> "Hearty Lizard"
| Big_hearty_radish -> "Big Hearty Radish"
| Stamella_shroom -> "Stamella Shroom"
| Restless_cricket -> "Restless Cricket"
| Courser_bee_honey -> "Courser Bee Honey"
| Bright_eyed_crab -> "Bright Eyed Crab"
| Staminoka_bass -> "Staminoka Bass"
| Energetic_rhino_beetle -> "Energetic Rhino Beetle"
| Endura_shroom -> "Endura Shroom"
| Tireless_frog -> "Tireless Frog"
| Endura_carrot -> "Endura Carrot"
| Spicy_pepper -> "Spicy Pepper"
| Warm_safflina -> "Warm Safflina"
| Summerwing_butterfly -> "Summerwing Butterfly"
| Sunshroom -> "Sunshroom"
| Warm_darner -> "Warm Darner"
| Sizzlefin_trout -> "Sizzlefin Trout"
| Hydromelon -> "Hydromelon"
| Cool_safflina -> "Cool Safflina"
| Winterwing_butterfly -> "Winterwing Butterfly"
| Chillshroom -> "Chillshroom"
| Cold_darner -> "Cold Darner"
| Chillfin_trout -> "Chillfin Trout"
| Voltfruit -> "Voltfruit"
| Electric_safflina -> "Electric Safflina"
| Thunderwing_butterfly -> "Thunderwing Butterfly"
| Zapshroom -> "Zapshroom"
| Electric_darner -> "Electric Darner"
| Voltfin_trout -> "Voltfin Trout"
| Fireproof_lizard -> "Fireproof Lizard"
| Smotherwing_butterfly -> "Smotherwing Butterfly"
| Rushroom -> "Rushroom"
| Swift_carrot -> "Swift Carrot"
| Hightail_lizard -> "Hightail Lizard"
| Fleet_lotus_seeds -> "Fleet Lotus Seeds"
| Swift_violet -> "Swift Violet"
| Hot_footed_frog -> "Hot Footed Frog"
| Blue_nightshade -> "Blue Nightshade"
| Sneaky_river_snail -> "Sneaky River Snail"
| Sunset_firefly -> "Sunset Firefly"
| Silent_shroom -> "Silent Shroom"
| Stealthfin_trout -> "Stealthfin Trout"
| Silent_princess -> "Silent Princess"
| Mighty_thistle -> "Mighty Thistle"
| Bladed_rhino_beetle -> "Bladed Rhino Beetle"
| Mighty_bananas -> "Mighty Bananas"
| Razorshroom -> "Razorshroom"
| Mighty_carp -> "Mighty Carp"
| Razorclaw_crab -> "Razorclaw Crab"
| Mighty_porgy -> "Mighty Porgy"
| Armoranth -> "Armoranth"
| Rugged_rhino_beetle -> "Rugged Rhino Beetle"
| Fortified_pumpkin -> "Fortified Pumpkin"
| Ironshroom -> "Ironshroom"
| Armored_carp -> "Armored Carp"
| Ironshell_crab -> "Ironshell Crab"
| Armored_porgy -> "Armored Porgy"
| Monster_horn Bokoblin_horn -> "Bokoblin Horn"
| Monster_horn Moblin_horn -> "Moblin Horn"
| Monster_horn Lizalfos_horn -> "Lizalfos Horn"
| Monster_horn Lynel_horn -> "Lynel Horn"
| Monster_horn Hinox_toenail -> "Hinox Toenail"
| Monster_horn Keese_wing -> "Keese Wing"
| Monster_horn Chuchu_jelly -> "Chuchu Jelly"
| Monster_horn Octorok_tentacle -> "Octorok Tentacle"
| Monster_horn Octo_balloon -> "Octo Balloon"
| Monster_horn Ancient_screw -> "Ancient Screw"
| Monster_horn Ancient_spring -> "Ancient Spring"
| Monster_fang Bokoblin_fang -> "Bokoblin Fang"
| Monster_fang Moblin_fang -> "Moblin Fang"
| Monster_fang Lizalfos_talon -> "Lizalfos Talon"
| Monster_fang Lynel_hoof -> "Lynel Hoof"
| Monster_fang Hinox_tooth -> "Hinox Tooth"
| Monster_fang Molduga_fin -> "Molduga Fin"
| Monster_fang White_chuchu_jelly -> "White Chuchu Jelly"
| Monster_fang Red_chuchu_jelly -> "Red Chuchu Jelly"
| Monster_fang Yellow_chuchu_jelly -> "Yellow Chuchu Jelly"
| Monster_fang Octorok_eyeball -> "Octorok Eyeball"
| Monster_fang Ice_keese_wing -> "Ice Keese Wing"
| Monster_fang Fire_keese_wing -> "Fire Keese Wing"
| Monster_fang Electric_keese_wing -> "Electric Keese Wing"
| Monster_fang Ancient_gear -> "Ancient Gear"
| Monster_fang Ancient_shaft -> "Ancient Shaft"
| Monster_guts Bokoblin_guts -> "Bokoblin Guts"
| Monster_guts Moblin_guts -> "Moblin Guts"
| Monster_guts Lizalfos_tail -> "Lizalfos Tail"
| Monster_guts Lynel_guts -> "Lynel Guts"
| Monster_guts Hinox_guts -> "Hinox Guts"
| Monster_guts Molduga_guts -> "Molduga Guts"
| Monster_guts Keese_eyeball -> "Keese Eyeball"
| Monster_guts Icy_lizalfos_tail -> "Icy Lizalfos Tail"
| Monster_guts Red_lizalfos_tail -> "Red Lizalfos Tail"
| Monster_guts Yellow_lizalfos_tail -> "Yellow Lizalfos Tail"
| Monster_guts Ancient_core -> "Ancient Core"
| Monster_guts Giant_ancient_core -> "Giant Ancient Core"

let ordered =
  [|
    Hearty_durian;
    Palm_fruit;
    Apple;
    Wildberry;
    Hydromelon;
    Spicy_pepper;
    Voltfruit;
    Fleet_lotus_seeds;
    Mighty_bananas;
    Big_hearty_truffle;
    Hearty_truffle;
    Hylian_shroom;
    Endura_shroom;
    Stamella_shroom;
    Chillshroom;
    Sunshroom;
    Zapshroom;
    Rushroom;
    Razorshroom;
    Ironshroom;
    Silent_shroom;
    Big_hearty_radish;
    Hearty_radish;
    Endura_carrot;
    Hyrule_herb;
    Swift_carrot;
    Fortified_pumpkin;
    Cool_safflina;
    Warm_safflina;
    Electric_safflina;
    Swift_violet;
    Mighty_thistle;
    Armoranth;
    Blue_nightshade;
    Silent_princess;
    Raw_gourmet_meat;
    Raw_whole_bird;
    Raw_prime_meat;
    Raw_bird_thigh;
    Raw_meat;
    Raw_bird_drumstick;
    Courser_bee_honey;
    Hylian_rice;
    Acorn;
    Chickaloo_tree_nut;
    Bird_egg;
    Tabantha_wheat;
    Fresh_milk;
    Cane_sugar;
    Goat_butter;
    Goron_spice;
    Rock_salt;
    Hearty_salmon;
    Hearty_bass;
    Hyrule_bass;
    Staminoka_bass;
    Chillfin_trout;
    Sizzlefin_trout;
    Voltfin_trout;
    Stealthfin_trout;
    Mighty_carp;
    Armored_carp;
    Sanke_carp;
    Mighty_porgy;
    Armored_porgy;
    Sneaky_river_snail;
    Hearty_blueshell_snail;
    Razorclaw_crab;
    Ironshell_crab;
    Bright_eyed_crab;
    Winterwing_butterfly;
    Summerwing_butterfly;
    Thunderwing_butterfly;
    Smotherwing_butterfly;
    Cold_darner;
    Warm_darner;
    Electric_darner;
    Restless_cricket;
    Bladed_rhino_beetle;
    Rugged_rhino_beetle;
    Energetic_rhino_beetle;
    Sunset_firefly;
    Hot_footed_frog;
    Tireless_frog;
    Hightail_lizard;
    Hearty_lizard;
    Fireproof_lizard;
    Monster_horn Bokoblin_horn;
    Monster_fang Bokoblin_fang;
    Monster_guts Bokoblin_guts;
    Monster_horn Moblin_horn;
    Monster_fang Moblin_fang;
    Monster_guts Moblin_guts;
    Monster_horn Lizalfos_horn;
    Monster_fang Lizalfos_talon;
    Monster_guts Lizalfos_tail;
    Monster_guts Icy_lizalfos_tail;
    Monster_guts Red_lizalfos_tail;
    Monster_guts Yellow_lizalfos_tail;
    Monster_horn Lynel_horn;
    Monster_fang Lynel_hoof;
    Monster_guts Lynel_guts;
    Monster_horn Chuchu_jelly;
    Monster_fang White_chuchu_jelly;
    Monster_fang Red_chuchu_jelly;
    Monster_fang Yellow_chuchu_jelly;
    Monster_horn Keese_wing;
    Monster_fang Ice_keese_wing;
    Monster_fang Fire_keese_wing;
    Monster_fang Electric_keese_wing;
    Monster_guts Keese_eyeball;
    Monster_horn Octorok_tentacle;
    Monster_fang Octorok_eyeball;
    Monster_horn Octo_balloon;
    Monster_fang Molduga_fin;
    Monster_guts Molduga_guts;
    Monster_horn Hinox_toenail;
    Monster_fang Hinox_tooth;
    Monster_guts Hinox_guts;
    Monster_horn Ancient_screw;
    Monster_horn Ancient_spring;
    Monster_fang Ancient_gear;
    Monster_fang Ancient_shaft;
    Monster_guts Ancient_core;
    Monster_guts Giant_ancient_core;
  |]
  |> Array.foldi ~init:Map.empty ~f:(fun data acc key -> Map.add_exn acc ~key ~data)

let to_img_src x = Map.find_exn Blob.blobs x
