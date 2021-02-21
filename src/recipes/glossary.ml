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

module Self = struct
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
    | Spicy_pepper
    | Warm_safflina
    | Summerswing_butterfly
    | Sunshroom
    | Warm_darner
    | Sizzlefin_trout
    | Hydromelon
    | Cool_safflina
    | Winterwing_butterfly
    | Chillshroom
    | Cold_darner
    | Chillfin_trout
    | Voltfruit
    | Electric_safflina
    | Thunderwing_butterfly
    | Zapshroom
    | Electric_darner
    | Voltfin_trout
    | Fireproof_lizard
    | Smotherwing_butterfly
    | Rushroom
    | Swift_carrot
    | Hightail_lizard
    | Fleet_lotus_seeds
    | Swift_violet
    | Hot_footed_frog
    | Blue_nightshade
    | Sneaky_river_snail
    | Sunset_firefly
    | Silent_shroom
    | Stealthfin_trout
    | Silent_princess
    | Mighty_thistle
    | Bladed_rhino_beetle
    | Mighty_bananas
    | Razorshroom
    | Mighty_carp
    | Razorclaw_crab
    | Mighty_porgy
    | Armoranth
    | Rugged_rhino_beetle
    | Fortified_pumpkin
    | Ironshroom
    | Armored_carp
    | Ironshell_crab
    | Armored_porgy
    | Monster_horn
    | Monster_fang
    | Monster_guts
  (* | Bokoblin_horn
     | Moblin_horn
     | Lizalfos_horn
     | Lynel_horn
     | Hinox_toenail
     | Keese_wing
     | Chuchu_jelly
     | Octorok_tentacle
     | Octo_balloon
     | Ancient_screw
     | Ancient_spring *)
  (* | Bokoblin_fang
     | Moblin_fang
     | Lizalfos_talon
     | Lynel_hoof
     | Hinox_tooth
     | Molduga_fin
     | White_chuchu_jelly
     | Red_chuchu_jelly
     | Yellow_chuchu_jelly
     | Octorok_eyeball
     | Ice_keese_wing
     | Fire_keese_wing
     | Electric_keese_wing
     | Ancient_gear
     | Ancient_shaft *)
  (* | Bokoblin_guts
     | Moblin_guts
     | Lizalfos_tail
     | Lynel_guts
     | Hinox_guts
     | Molduga_guts
     | Keese_eyeball
     | Icy_lizalfos_tail
     | Red_lizalfos_tail
     | Yellow_lizalfos_tail
     | Ancient_core
     | Giant_ancient_core *)
  [@@deriving sexp, compare, hash, variants]
end

module Map = Map.Make (Self)
include Self

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
| Hearty_lizard -> Ingredient.{ hearts = 8; effect = Hearty 4; category = Critter }
| Big_hearty_radish -> make_hearty 8 5
| Stamella_shroom -> make_energizing 1 (1, 2, 4, 5, 7)
| Restless_cricket -> bonus_critter Ingredient.Effect.energizing (1, 2, 4, 5, 5)
| Courser_bee_honey -> make_energizing 4 (2, 5, 8, 11, 14)
| Bight_eyed_crab -> make_energizing 2 (2, 5, 8, 11, 14)
| Staminoka_bass -> make_energizing 2 (5, 11, 15, 15, 15)
| Energetic_rhino_beetle -> bonus_critter Ingredient.Effect.energizing (8, 15, 15, 15, 15)
| Endura_shroom -> make_enduring 2 (1, 1, 1, 2, 2)
| Tireless_frog -> bonus_critter ~hearts:4 Ingredient.Effect.enduring (1, 2, 3, 4, 4)
| Endura_carrot -> make_enduring 4 (2, 4, 6, 8, 10)
| Spicy_pepper -> make_spicy 1 (1, 1, 1, 1, 1)
| Warm_safflina -> make_spicy 0 (1, 1, 1, 1, 1)
| Summerswing_butterfly -> effect_critter (2, 30) Ingredient.Effect.spicy (1, 1, 1, 1, 1)
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
| Monster_horn -> make_monster (1, 10)
| Monster_fang -> make_monster (1, 50)
| Monster_guts -> make_monster (3, 10)
