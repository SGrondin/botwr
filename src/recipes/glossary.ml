open! Core_kernel

let always (min, sec) = Ingredient.Duration.Always ((min * 60) + sec)

let make_food quarters sec =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Neutral (Always sec);
      category = Food;
      critical = false;
    }

(* The first one adds a different duration from 2+ *)
let make_ingredient quarters first next =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Neutral (Diminishing { first; next });
      category = Food;
      critical = false;
    }

(* A "spice" needs at least one other non-spice or it's Dubious *)
let make_spice quarters first next =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Neutral (Diminishing { first; next });
      category = Spice;
      critical = false;
    }

let make_hearty quarters bonus =
  Ingredient.
    { hearts = Always (Quarters quarters); effect = Hearty bonus; category = Food; critical = false }

let make_sunny quarters unglooms =
  Ingredient.
    { hearts = Always (Quarters quarters); effect = Sunny unglooms; category = Food; critical = false }

let make_effect variant dur quarters points =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = variant Effect.Activity.{ duration = always dur; points };
      category = Food;
      critical = false;
    }

let make_energizing quarters x =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Energizing (Fifths x);
      category = Food;
      critical = false;
    }

let make_enduring quarters x =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Enduring (Quarters x);
      category = Food;
      critical = false;
    }

let make_spicy = make_effect Ingredient.Effect.spicy (2, 30)

let make_chilly = make_effect Ingredient.Effect.chilly (2, 30)

let make_electro = make_effect Ingredient.Effect.electro (2, 30)

let make_hasty = make_effect Ingredient.Effect.hasty (1, 0)

let make_rapid = make_effect Ingredient.Effect.rapid (2, 0)

let make_sneaky = make_effect Ingredient.Effect.sneaky (2, 0)

let make_mighty = make_effect Ingredient.Effect.mighty (0, 50)

let make_tough = make_effect Ingredient.Effect.tough (0, 50)

let make_bright = make_effect Ingredient.Effect.bright (2, 0)

let energizing_critter x =
  Ingredient.
    { hearts = Always (Quarters 0); effect = Energizing (Fifths x); category = Critter; critical = false }

let enduring_critter ?(quarters = 0) x =
  Ingredient.
    {
      hearts = Always (Quarters quarters);
      effect = Enduring (Quarters x);
      category = Critter;
      critical = false;
    }

let effect_critter dur variant points =
  Ingredient.
    {
      hearts = Always (Quarters 0);
      effect = variant Effect.Activity.{ duration = always dur; points };
      category = Critter;
      critical = false;
    }

let make_monster dur =
  Ingredient.
    { hearts = Always (Quarters 0); effect = Neutral (always dur); category = Monster; critical = false }

let make_dragon hearts first =
  Ingredient.
    {
      hearts = Diminishing { first = hearts; next = Quarters 0 };
      effect = Neutral (Diminishing { first; next = 30 });
      category = Dragon;
      critical = true;
    }

let cached_monster_horn = make_monster (1, 10)

let cached_monster_fang = make_monster (1, 50)

let cached_monster_guts = make_monster (3, 10)

let cached_dragon_scale = make_dragon (Quarters 5) 90

let cached_dragon_claw = make_dragon (Quarters 8) 210

let cached_dragon_fang = make_dragon (Quarters 10) 630

let cached_dragon_horn = make_dragon (Quarters 15) 1800

include Items

let to_ingredient =
  let do_to_ingredient = function
    | Skyshroom -> make_food 2 30
    | Dazzlefruit -> make_food 2 30 (* TODO *)
    | Korok_frond -> make_food 2 30
    | Apple -> make_food 4 30
    | Wildberry -> make_food 4 30
    | Hylian_shroom -> make_food 4 30
    | Palm_fruit -> make_food 8 30
    | Ancient_arowana -> make_food 8 30
    | Hylian_tomato -> make_food 8 30
    | Hyrule_herb -> make_food 8 30
    | Hyrule_bass -> make_food 8 30
    | Sanke_carp -> make_food 8 30
    | Raw_meat -> make_food 8 30
    | Raw_bird_drumstick -> make_food 8 30
    | Golden_apple -> make_food 12 30 (* TODO *)
    | Raw_bird_thigh -> make_food 12 30
    | Raw_gourmet_meat -> make_food 24 30
    | Raw_whole_bird -> make_food 24 30
    | Raw_prime_meat -> make_food 12 30
    | Bird_egg -> make_ingredient 8 90 30
    | Fresh_milk -> make_ingredient 4 80 30
    | Acorn -> make_ingredient 4 50 30
    | Chickaloo_tree_nut -> make_ingredient 4 40 30
    | Hylian_rice -> make_spice 8 60 30
    | Tabantha_wheat -> make_spice 8 60 30
    | Oil_jar -> make_spice 0 80 30
    | Cane_sugar -> make_spice 0 80 30
    | Goat_butter -> make_spice 0 80 30
    | Hateno_cheese -> make_spice 8 80 30
    | Goron_spice -> make_spice 0 90 30
    | Rock_salt -> make_spice 0 60 30
    (* Hearty *)
    | Hearty_truffle -> make_hearty 16 1
    | Hearty_bass -> make_hearty 16 2
    | Hearty_radish -> make_hearty 20 3
    | Hearty_blueshell_snail -> make_hearty 24 3
    | Hearty_durian -> make_hearty 24 4
    | Big_hearty_truffle -> make_hearty 24 4
    | Hearty_salmon -> make_hearty 32 4
    | Hearty_lizard ->
      Ingredient.
        { hearts = Always (Quarters 32); effect = Hearty 4; category = Critter; critical = false }
    | Big_hearty_radish -> make_hearty 32 5
    (* Sunny *)
    | Sun_pumpkin -> make_sunny 4 1
    | Sundelion -> make_sunny 0 3
    (* Energizing *)
    | Stamella_shroom -> make_energizing 4 7
    | Stambulb -> make_energizing 4 7
    | Restless_cricket -> energizing_critter 7
    | Courser_bee_honey -> make_energizing 16 14
    | Bright_eyed_crab -> make_energizing 8 14
    | Staminoka_bass -> make_energizing 8 28
    | Energetic_rhino_beetle -> energizing_critter 42
    (* Enduring *)
    | Endura_shroom -> make_enduring 8 2
    | Tireless_frog -> enduring_critter ~quarters:16 4
    | Endura_carrot -> make_enduring 16 8
    (* Spicy *)
    | Spicy_pepper -> make_spicy 4 1
    | Warm_safflina -> make_spicy 0 1
    | Summerwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.spicy 1
    | Sunshroom -> make_spicy 4 2
    | Warm_darner -> effect_critter (2, 30) Ingredient.Effect.spicy 2
    | Sizzlefin_trout -> make_spicy 8 3
    (* Chilly *)
    | Hydromelon -> make_chilly 4 1
    | Cool_safflina -> make_chilly 0 1
    | Winterwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.chilly 1
    | Chillshroom -> make_chilly 4 2
    | Cold_darner -> effect_critter (2, 30) Ingredient.Effect.chilly 2
    | Chillfin_trout -> make_chilly 8 3
    (* Electro *)
    | Voltfruit -> make_electro 4 1
    | Electric_safflina -> make_electro 0 1
    | Thunderwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.electro 1
    | Zapshroom -> make_electro 4 2
    | Electric_darner -> effect_critter (2, 30) Ingredient.Effect.electro 2
    | Voltfin_trout -> make_electro 8 3
    (* Fireproof *)
    | Fireproof_lizard -> effect_critter (2, 30) Ingredient.Effect.fireproof 1
    | Smotherwing_butterfly -> effect_critter (2, 30) Ingredient.Effect.fireproof 2
    (* Hasty *)
    | Rushroom -> make_hasty 4 1
    | Swift_carrot -> make_hasty 4 1
    | Hightail_lizard -> effect_critter (1, 0) Ingredient.Effect.hasty 1
    | Fleet_lotus_seeds -> make_hasty 4 2
    | Swift_violet -> make_hasty 0 2
    | Hot_footed_frog -> effect_critter (1, 0) Ingredient.Effect.hasty 2
    (* Rapid *)
    | Splash_fruit -> make_rapid 2 1
    (* Sticky *)
    | Sticky_lizard -> effect_critter (2, 0) Ingredient.Effect.sticky 1
    | Sticky_frog ->
      (* TODO *)
      effect_critter (2, 0) Ingredient.Effect.sticky 3
    (* Sneaky *)
    | Blue_nightshade -> make_sneaky 0 1
    | Sneaky_river_snail -> make_sneaky 8 1
    | Sunset_firefly -> effect_critter (2, 0) Ingredient.Effect.sneaky 1
    | Silent_shroom -> make_sneaky 4 2
    | Stealthfin_trout -> make_sneaky 8 2
    | Silent_princess -> make_sneaky 8 3
    (* Mighty *)
    | Mighty_thistle -> make_mighty 0 1
    | Bladed_rhino_beetle -> effect_critter (0, 50) Ingredient.Effect.mighty 1
    | Mighty_bananas -> make_mighty 4 2
    | Razorshroom -> make_mighty 4 2
    | Mighty_carp -> make_mighty 8 2
    | Razorclaw_crab -> make_mighty 8 2
    | Mighty_porgy -> make_mighty 8 3
    (* Tough *)
    | Armoranth -> make_tough 0 1
    | Rugged_rhino_beetle -> effect_critter (0, 50) Ingredient.Effect.tough 1
    | Fortified_pumpkin -> make_tough 4 2
    | Ironshroom -> make_tough 4 2
    | Armored_carp -> make_tough 8 2
    | Ironshell_crab -> make_tough 8 2
    | Armored_porgy -> make_tough 8 3
    (* Bright *)
    | Brightcap -> make_bright 4 1
    | Deep_firefly -> effect_critter (2, 0) Ingredient.Effect.bright 2
    | Glowing_cave_fish -> make_bright 8 2
    (* Other *)
    | Fairy ->
      (* The -3 full hearts is only applied when the final result is a Fairy Tonic *)
      {
        hearts = Always (Quarters 40);
        effect = Neutral (Always 30);
        category = With_fairy Spice;
        critical = false;
      }
    | Star_fragment ->
      { hearts = Always (Quarters 0); effect = Neutral (Always 30); category = Dragon; critical = true }
    | Monster_horn _ -> cached_monster_horn
    | Monster_fang _ -> cached_monster_fang
    | Monster_guts _ -> cached_monster_guts
    | Dragon_scales _ -> cached_dragon_scale
    | Dragon_claws _ -> cached_dragon_claw
    | Dragon_fangs _ -> cached_dragon_fang
    | Dragon_horns _ -> cached_dragon_horn
  in
  let mapped = Array.of_list_map all ~f:do_to_ingredient in
  function
  | Monster_horn _ -> cached_monster_horn
  | Monster_fang _ -> cached_monster_fang
  | Monster_guts _ -> cached_monster_guts
  | Dragon_scales _ -> cached_dragon_scale
  | Dragon_claws _ -> cached_dragon_claw
  | Dragon_fangs _ -> cached_dragon_fang
  | Dragon_horns _ -> cached_dragon_horn
  | x -> mapped.(Variants.to_rank x)

let to_kind = Fn.compose Ingredient.to_kind to_ingredient

module Category = struct
  type t =
    | Meals
    | Elixirs
    | Any
end

let availability : t -> Game.availability = function
| Apple
 |Wildberry
 |Hylian_shroom
 |Palm_fruit
 |Hyrule_herb
 |Hyrule_bass
 |Sanke_carp
 |Raw_meat
 |Raw_bird_drumstick
 |Raw_bird_thigh
 |Raw_prime_meat
 |Raw_gourmet_meat
 |Raw_whole_bird
 |Chickaloo_tree_nut
 |Acorn
 |Rock_salt
 |Hylian_rice
 |Tabantha_wheat
 |Cane_sugar
 |Goat_butter
 |Fresh_milk
 |Goron_spice
 |Bird_egg
 |Hearty_truffle
 |Hearty_bass
 |Hearty_radish
 |Hearty_blueshell_snail ->
  Both
| Hearty_durian -> BOTW
| Big_hearty_truffle
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
 |Zapshroom ->
  Both
| Electric_darner -> BOTW
| Voltfin_trout
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
 |Armored_porgy
 |Fairy
 |Star_fragment ->
  Both
| Monster_horn (Ancient_screw | Ancient_spring) -> BOTW
| Monster_horn _ -> Both
| Monster_fang (Ancient_gear | Ancient_shaft) -> BOTW
| Monster_fang _ -> Both
| Monster_guts (Ancient_core | Giant_ancient_core) -> BOTW
| Monster_guts _ -> Both
| Dragon_scales _
 |Dragon_claws _
 |Dragon_fangs _
 |Dragon_horns _ ->
  BOTW
| Skyshroom
 |Dazzlefruit
 |Korok_frond
 |Ancient_arowana
 |Hylian_tomato
 |Golden_apple
 |Hateno_cheese
 |Oil_jar
 |Sun_pumpkin
 |Sundelion
 |Stambulb
 |Splash_fruit
 |Deep_firefly
 |Glowing_cave_fish
 |Brightcap
 |Sticky_lizard
 |Sticky_frog ->
  TOTK

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
| Fairy -> "Fairy"
| Star_fragment -> "Star Fragment"
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
| Dragon_scales Dinraal -> "Dinraal's Scale"
| Dragon_scales Naydra -> "Naydra's Scale"
| Dragon_scales Farosh -> "Farosh's Scale"
| Dragon_claws Dinraal -> "Dinraal's Claw"
| Dragon_claws Naydra -> "Naydra's Claw"
| Dragon_claws Farosh -> "Farosh's Claw"
| Dragon_fangs Dinraal -> "Shard of Dinraal's Fang"
| Dragon_fangs Naydra -> "Shard of Naydra's Fang"
| Dragon_fangs Farosh -> "Shard of Farosh's Fang"
| Dragon_horns Dinraal -> "Shard of Dinraal's Horn"
| Dragon_horns Naydra -> "Shard of Naydra's Horn"
| Dragon_horns Farosh -> "Shard of Farosh's Horn"
| Skyshroom -> "Skyshroom"
| Dazzlefruit -> "Dazzlefruit"
| Korok_frond -> "Korok Frond"
| Ancient_arowana -> "Ancient Arowana"
| Hylian_tomato -> "Hylian Tomato"
| Golden_apple -> "Golden Apple"
| Hateno_cheese -> "Hateno Cheese"
| Oil_jar -> "Oil Jar"
| Sun_pumpkin -> "Sun Pumpkin"
| Sundelion -> "Sundelion"
| Stambulb -> "Stambulb"
| Splash_fruit -> "Splash Fruit"
| Deep_firefly -> "Deep Firefly"
| Glowing_cave_fish -> "Glowing Cave Fish"
| Brightcap -> "Brightcap"
| Sticky_lizard -> "Sticky Lizard"
| Sticky_frog -> "Sticky Frog"

let ordered_botw =
  lazy
    ([|
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
       Star_fragment;
       Dragon_scales Dinraal;
       Dragon_scales Naydra;
       Dragon_scales Farosh;
       Dragon_claws Dinraal;
       Dragon_claws Naydra;
       Dragon_claws Farosh;
       Dragon_fangs Dinraal;
       Dragon_fangs Naydra;
       Dragon_fangs Farosh;
       Dragon_horns Dinraal;
       Dragon_horns Naydra;
       Dragon_horns Farosh;
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
       Fairy;
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
    |> Array.foldi ~init:Map.empty ~f:(fun data acc key -> Map.add_exn acc ~key ~data))

let ordered_totk =
  lazy
    ([|
       Golden_apple;
       Palm_fruit;
       Hylian_tomato;
       Apple;
       Wildberry;
       Hydromelon;
       Fleet_lotus_seeds;
       Spicy_pepper;
       (* Fire_fruit *)
       (* Ice_fruit *)
       (* Shock_fruit *)
       Splash_fruit;
       Dazzlefruit;
       Voltfruit;
       Mighty_bananas;
       Hearty_truffle;
       Big_hearty_truffle;
       Big_hearty_radish;
       Rushroom;
       Brightcap;
       Stamella_shroom;
       Chillshroom;
       Sunshroom;
       Hylian_shroom;
       Silent_shroom;
       Ironshroom;
       Skyshroom;
       Endura_shroom;
       Zapshroom;
       Razorshroom;
       Hyrule_herb;
       Silent_princess;
       Sun_pumpkin;
       Swift_carrot;
       Stambulb;
       Korok_frond;
       Hearty_radish;
       Endura_carrot;
       Fortified_pumpkin;
       Cool_safflina;
       Warm_safflina;
       Electric_safflina;
       Armoranth;
       Blue_nightshade;
       Swift_violet;
       Mighty_thistle;
       Sundelion;
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
       Hateno_cheese;
       Fresh_milk;
       Cane_sugar;
       Goat_butter;
       Goron_spice;
       Oil_jar;
       (* Dark_clump *)
       Rock_salt;
       Star_fragment;
       Dragon_scales Dinraal;
       Dragon_scales Naydra;
       Dragon_scales Farosh;
       Dragon_claws Dinraal;
       Dragon_claws Naydra;
       Dragon_claws Farosh;
       Dragon_fangs Dinraal;
       Dragon_fangs Naydra;
       Dragon_fangs Farosh;
       Dragon_horns Dinraal;
       Dragon_horns Naydra;
       Dragon_horns Farosh;
       Hearty_salmon;
       Hearty_bass;
       Hyrule_bass;
       Staminoka_bass;
       Chillfin_trout;
       Ancient_arowana;
       Glowing_cave_fish;
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
       Fairy;
       Winterwing_butterfly;
       Summerwing_butterfly;
       Thunderwing_butterfly;
       Smotherwing_butterfly;
       Cold_darner;
       Warm_darner;
       Electric_darner;
       Deep_firefly;
       Restless_cricket;
       Bladed_rhino_beetle;
       Rugged_rhino_beetle;
       Energetic_rhino_beetle;
       Sunset_firefly;
       Hot_footed_frog;
       Tireless_frog;
       Sticky_frog;
       Hightail_lizard;
       Hearty_lizard;
       Fireproof_lizard;
       Sticky_lizard;
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
     |]
    |> Array.foldi ~init:Map.empty ~f:(fun data acc key -> Map.add_exn acc ~key ~data))

let to_img_src x = Map.find_exn Blob.blobs x
