open! Core_kernel

module Self = struct
  type monster_horn =
    | Octo_balloon (* Fused: 1 *)
    | Keese_wing (* Fused: 1 *)
    | Chuchu_jelly (* Fused: 1 *)
    | Octorok_tentacle (* Fused: 3 *)
    | Bokoblin_horn (* Fused: 4 *)
    | Aerocuda_wing (* Fused: 4 *)
    | Moblin_horn (* Fused: 6 *)
    | Hinox_toenail (* Fused: 7 *)
    | Lizalfos_horn (* Fused: 8 *)
    | Gibdo_wing (* Fused: 8 *)
    | Horriblin_horn (* ? *)
    | Gibdo_bone (* Fused: 40 *)
    | Lynel_horn (* ? *)
    | Ancient_screw (* BOTW *)
    | Ancient_spring (* BOTW *)
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type monster_fang =
    | Bokoblin_fang (* Fused: 2 *)
    | Gibdo_guts (* ? *)
    | White_chuchu_jelly (* Fused: 1 *)
    | Red_chuchu_jelly (* Fused: 1 *)
    | Yellow_chuchu_jelly (* Fused: 1 *)
    | Octorok_eyeball (* Fused: 3 *)
    | Moblin_fang (* Fused: 4 *)
    | Aerocuda_eyeball (* Fused: 4 *)
    | Lizalfos_talon (* Fused: 5 *)
    | Lynel_hoof (* ? *)
    | Hinox_tooth (* ? *)
    | Molduga_fin (* ? *)
    | Ice_keese_wing (* ? *)
    | Fire_keese_wing (* ? *)
    | Electric_keese_wing (* ? *)
    | Horriblin_claw (* ? *)
    | Ancient_gear (* BOTW *)
    | Ancient_shaft (* BOTW *)
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type monster_guts =
    | Bokoblin_guts (* Fused: 1 *)
    | Moblin_guts (* Fused:  *)
    | Lynel_guts (* Fused:  *)
    | Hinox_guts (* Fused:  *)
    | Molduga_guts (* Fused:  *)
    | Horriblin_guts (* Fused:  *)
    | Keese_eyeball (* Fused: 1 *)
    | Lizalfos_tail (* Fused: 6 *)
    | Icy_lizalfos_tail (* BOTW *)
    | Red_lizalfos_tail (* BOTW *)
    | Yellow_lizalfos_tail (* BOTW *)
    | Ancient_core (* BOTW *)
    | Giant_ancient_core (* BOTW *)
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  type dragon =
    | Dinraal
    | Naydra
    | Farosh
  [@@deriving sexp, compare, equal, hash, variants, enumerate]

  (* IMPORTANT: sorted by strength within their own category *)
  type t =
    (* Food (Always _) *)
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
    | Raw_bird_thigh
    | Raw_prime_meat
    | Raw_gourmet_meat
    | Raw_whole_bird
    (* Ingredient (Diminishing _) *)
    (* and Spice (Diminishing _) *)
    | Chickaloo_tree_nut
    | Acorn
    | Golden_apple
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
