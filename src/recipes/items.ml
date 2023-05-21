open! Core_kernel

module Self = struct
  type monster_horn =
    | Bokoblin_horn
    | Moblin_horn
    | Lizalfos_horn
    | Lynel_horn
    | Hinox_toenail
    | Keese_wing
    | Chuchu_jelly
    | Octorok_tentacle
    | Octo_balloon
    | Ancient_screw
    | Ancient_spring
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type monster_fang =
    | Bokoblin_fang
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
    | Ancient_shaft
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type monster_guts =
    | Bokoblin_guts
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
    | Giant_ancient_core
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type dragon =
    | Dinraal
    | Naydra
    | Farosh
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  (* IMPORTANT: sorted by strength within their own category *)
  type t =
    | Skyshroom
    | Dazzlefruit
    | Korok_frond
    | Apple
    | Wildberry
    | Hylian_shroom
    | Palm_fruit
    | Ancient_arowana
    | Hylian_tomato
    | Hyrule_herb
    | Hyrule_bass
    | Sanke_carp
    | Raw_meat
    | Raw_bird_drumstick
    | Golden_apple
    | Raw_bird_thigh
    | Raw_prime_meat
    | Raw_gourmet_meat
    | Raw_whole_bird
    | Chickaloo_tree_nut
    | Acorn
    | Rock_salt
    | Hylian_rice
    | Tabantha_wheat
    | Oil_jar
    | Cane_sugar
    | Goat_butter
    | Fresh_milk
    | Hateno_cheese
    | Goron_spice
    | Bird_egg
    (* Hearty *)
    | Hearty_truffle
    | Hearty_bass
    | Hearty_radish
    | Hearty_blueshell_snail
    | Hearty_durian
    | Big_hearty_truffle
    | Hearty_salmon
    | Hearty_lizard
    | Big_hearty_radish
    (* Sunny *)
    | Sun_pumpkin
    | Sundelion
    (* Energizing *)
    | Stamella_shroom
    | Stambulb
    | Restless_cricket
    | Courser_bee_honey
    | Bright_eyed_crab
    | Staminoka_bass
    | Energetic_rhino_beetle
    (* Enduring *)
    | Endura_shroom
    | Tireless_frog
    | Endura_carrot
    (* Spicy *)
    | Spicy_pepper
    | Warm_safflina
    | Summerwing_butterfly
    | Sunshroom
    | Warm_darner
    | Sizzlefin_trout
    (* Chilly *)
    | Hydromelon
    | Cool_safflina
    | Winterwing_butterfly
    | Chillshroom
    | Cold_darner
    | Chillfin_trout
    (* Electro *)
    | Voltfruit
    | Electric_safflina
    | Thunderwing_butterfly
    | Zapshroom
    | Electric_darner
    | Voltfin_trout
    (* Fireproof *)
    | Fireproof_lizard
    | Smotherwing_butterfly
    (* Hasty *)
    | Rushroom
    | Swift_carrot
    | Hightail_lizard
    | Fleet_lotus_seeds
    | Swift_violet
    | Hot_footed_frog
    (* Rapid *)
    | Splash_fruit
    (* Sticky *)
    | Sticky_lizard
    | Sticky_frog
    (* Sneaky *)
    | Blue_nightshade
    | Sneaky_river_snail
    | Sunset_firefly
    | Silent_shroom
    | Stealthfin_trout
    | Silent_princess
    (* Mighty *)
    | Mighty_thistle
    | Bladed_rhino_beetle
    | Mighty_bananas
    | Razorshroom
    | Mighty_carp
    | Razorclaw_crab
    | Mighty_porgy
    (* Tough *)
    | Armoranth
    | Rugged_rhino_beetle
    | Fortified_pumpkin
    | Ironshroom
    | Armored_carp
    | Ironshell_crab
    | Armored_porgy
    (* Bright *)
    | Brightcap
    | Deep_firefly
    | Glowing_cave_fish
    (* Other *)
    | Fairy
    | Star_fragment
    | Monster_horn           of monster_horn
    | Monster_fang           of monster_fang
    | Monster_guts           of monster_guts
    | Dragon_scales          of dragon
    | Dragon_claws           of dragon
    | Dragon_fangs           of dragon
    | Dragon_horns           of dragon
  [@@deriving sexp, compare, equal, hash, variants, enumerate]
end

module Map = Map.Make (Self)
module Table = Hashtbl.Make (Self)
include Self
