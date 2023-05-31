open! Core_kernel

module Table = Hashtbl.Make (struct
  type t = Items.t * Game.t [@@deriving sexp, compare, hash]
end)

let compute blob =
  let b64 =
    match Base64.encode ~pad:true blob with
    | Ok x -> x
    | Error (`Msg msg) -> failwith msg
  in
  sprintf "data:image/png;base64,%s" b64

let dubious = lazy ([%blob "images/dubious.png"] |> compute)

let cache = Table.create ()

let get item game =
  Table.find_or_add cache (item, game) ~default:(fun () ->
      let blob =
        match item, game with
        | Palm_fruit, _ -> [%blob "images/2.png"]
        | Apple, _ -> [%blob "images/3.png"]
        | Wildberry, _ -> [%blob "images/4.png"]
        | Hylian_shroom, _ -> [%blob "images/12.png"]
        | Hyrule_herb, _ -> [%blob "images/25.png"]
        | Hyrule_bass, _ -> [%blob "images/69.png"]
        | Sanke_carp, _ -> [%blob "images/77.png"]
        | Raw_gourmet_meat, _ -> [%blob "images/36.png"]
        | Raw_whole_bird, _ -> [%blob "images/37.png"]
        | Raw_prime_meat, _ -> [%blob "images/38.png"]
        | Raw_bird_thigh, _ -> [%blob "images/39.png"]
        | Raw_meat, _ -> [%blob "images/40.png"]
        | Raw_bird_drumstick, _ -> [%blob "images/41.png"]
        | Bird_egg, _ -> [%blob "images/46.png"]
        | Fresh_milk, _ -> [%blob "images/48.png"]
        | Acorn, _ -> [%blob "images/44.png"]
        | Chickaloo_tree_nut, _ -> [%blob "images/45.png"]
        | Hylian_rice, _ -> [%blob "images/43.png"]
        | Tabantha_wheat, _ -> [%blob "images/47.png"]
        | Cane_sugar, _ -> [%blob "images/49.png"]
        | Goat_butter, _ -> [%blob "images/50.png"]
        | Goron_spice, _ -> [%blob "images/51.png"]
        | Rock_salt, _ -> [%blob "images/52.png"]
        | Hearty_truffle, _ -> [%blob "images/11.png"]
        | Hearty_bass, _ -> [%blob "images/68.png"]
        | Hearty_radish, _ -> [%blob "images/23.png"]
        | Hearty_blueshell_snail, _ -> [%blob "images/81.png"]
        | Hearty_durian, _ -> [%blob "images/1.png"]
        | Big_hearty_truffle, _ -> [%blob "images/10.png"]
        | Hearty_salmon, _ -> [%blob "images/67.png"]
        | Hearty_lizard, _ -> [%blob "images/101.png"]
        | Big_hearty_radish, _ -> [%blob "images/22.png"]
        | Stamella_shroom, _ -> [%blob "images/14.png"]
        | Restless_cricket, _ -> [%blob "images/93.png"]
        | Courser_bee_honey, _ -> [%blob "images/42.png"]
        | Bright_eyed_crab, _ -> [%blob "images/84.png"]
        | Staminoka_bass, _ -> [%blob "images/70.png"]
        | Energetic_rhino_beetle, _ -> [%blob "images/96.png"]
        | Endura_shroom, _ -> [%blob "images/13.png"]
        | Tireless_frog, _ -> [%blob "images/99.png"]
        | Endura_carrot, _ -> [%blob "images/24.png"]
        | Spicy_pepper, _ -> [%blob "images/6.png"]
        | Warm_safflina, _ -> [%blob "images/29.png"]
        | Summerwing_butterfly, _ -> [%blob "images/87.png"]
        | Sunshroom, _ -> [%blob "images/16.png"]
        | Warm_darner, _ -> [%blob "images/91.png"]
        | Sizzlefin_trout, _ -> [%blob "images/72.png"]
        | Hydromelon, _ -> [%blob "images/5.png"]
        | Cool_safflina, _ -> [%blob "images/28.png"]
        | Winterwing_butterfly, _ -> [%blob "images/86.png"]
        | Chillshroom, _ -> [%blob "images/15.png"]
        | Cold_darner, _ -> [%blob "images/90.png"]
        | Chillfin_trout, _ -> [%blob "images/71.png"]
        | Voltfruit, _ -> [%blob "images/7.png"]
        | Electric_safflina, _ -> [%blob "images/30.png"]
        | Thunderwing_butterfly, _ -> [%blob "images/88.png"]
        | Zapshroom, _ -> [%blob "images/17.png"]
        | Electric_darner, _ -> [%blob "images/92.png"]
        | Voltfin_trout, _ -> [%blob "images/73.png"]
        | Fireproof_lizard, _ -> [%blob "images/102.png"]
        | Smotherwing_butterfly, _ -> [%blob "images/89.png"]
        | Rushroom, _ -> [%blob "images/18.png"]
        | Swift_carrot, _ -> [%blob "images/26.png"]
        | Hightail_lizard, _ -> [%blob "images/100.png"]
        | Fleet_lotus_seeds, _ -> [%blob "images/8.png"]
        | Swift_violet, _ -> [%blob "images/31.png"]
        | Hot_footed_frog, _ -> [%blob "images/98.png"]
        | Blue_nightshade, _ -> [%blob "images/34.png"]
        | Sneaky_river_snail, _ -> [%blob "images/80.png"]
        | Sunset_firefly, _ -> [%blob "images/97.png"]
        | Silent_shroom, _ -> [%blob "images/21.png"]
        | Stealthfin_trout, _ -> [%blob "images/74.png"]
        | Silent_princess, _ -> [%blob "images/35.png"]
        | Mighty_thistle, _ -> [%blob "images/32.png"]
        | Bladed_rhino_beetle, _ -> [%blob "images/94.png"]
        | Mighty_bananas, _ -> [%blob "images/9.png"]
        | Razorshroom, _ -> [%blob "images/19.png"]
        | Mighty_carp, _ -> [%blob "images/75.png"]
        | Razorclaw_crab, _ -> [%blob "images/82.png"]
        | Mighty_porgy, _ -> [%blob "images/78.png"]
        | Armoranth, _ -> [%blob "images/33.png"]
        | Rugged_rhino_beetle, _ -> [%blob "images/95.png"]
        | Fortified_pumpkin, _ -> [%blob "images/27.png"]
        | Ironshroom, _ -> [%blob "images/20.png"]
        | Armored_carp, _ -> [%blob "images/76.png"]
        | Ironshell_crab, _ -> [%blob "images/83.png"]
        | Armored_porgy, _ -> [%blob "images/79.png"]
        | Fairy, _ -> [%blob "images/85.png"]
        | Star_fragment, _ -> [%blob "images/54.png"]
        | Monster_horn Bokoblin_horn, BOTW -> [%blob "images/111.png"]
        | Monster_horn Bokoblin_horn, TOTK -> [%blob "images/bokoblin-horn.png"]
        | Monster_horn Moblin_horn, BOTW -> [%blob "images/114.png"]
        | Monster_horn Moblin_horn, TOTK -> [%blob "images/moblin-horn.png"]
        | Monster_horn Lizalfos_horn, BOTW -> [%blob "images/117.png"]
        | Monster_horn Lizalfos_horn, TOTK -> [%blob "images/lizalfos-horn.png"]
        | Monster_horn Lynel_horn, _ -> [%blob "images/123.png"]
        | Monster_horn Hinox_toenail, _ -> [%blob "images/140.png"]
        | Monster_horn Keese_wing, _ -> [%blob "images/130.png"]
        | Monster_horn Chuchu_jelly, _ -> [%blob "images/126.png"]
        | Monster_horn Octorok_tentacle, _ -> [%blob "images/135.png"]
        | Monster_horn Octo_balloon, _ -> [%blob "images/137.png"]
        | Monster_horn Ancient_screw, _ -> [%blob "images/143.png"]
        | Monster_horn Ancient_spring, _ -> [%blob "images/144.png"]
        | Monster_horn Gibdo_bone, _ -> [%blob "images/gibdo-bone.png"]
        | Monster_horn Gibdo_wing, _ -> [%blob "images/gibdo-wing.png"]
        | Monster_horn Horriblin_horn, _ -> [%blob "images/horriblin-horn.png"]
        | Monster_horn Blue_bokoblin_horn, _ -> [%blob "images/blue-bokoblin-horn.png"]
        | Monster_horn Black_bokoblin_horn, _ -> [%blob "images/black-bokoblin-horn.png"]
        | Monster_horn Blue_horriblin_horn, _ -> [%blob "images/blue-horriblin-horn.png"]
        | Monster_horn Black_horriblin_horn, _ -> [%blob "images/black-horriblin-horn.png"]
        | Monster_horn Blue_moblin_horn, _ -> [%blob "images/blue-moblin-horn.png"]
        | Monster_horn Black_moblin_horn, _ -> [%blob "images/black-moblin-horn.png"]
        | Monster_horn Electric_lizalfos_horn, _ -> [%blob "images/electric-lizalfos-horn.png"]
        | Monster_horn Fire_breath_lizalfos_horn, _ -> [%blob "images/fire-breath-lizalfos-horn.png"]
        | Monster_horn Ice_breath_lizalfos_horn, _ -> [%blob "images/ice-breath-lizalfos-horn.png"]
        | Monster_horn Blue_lizalfos_horn, _ -> [%blob "images/blue-lizalfos-horn.png"]
        | Monster_horn Black_lizalfos_horn, _ -> [%blob "images/black-lizalfos-horn.png"]
        | Monster_horn Aerocuda_wing, _ -> [%blob "images/aerocuda-wing.png"]
        | Monster_fang Bokoblin_fang, _ -> [%blob "images/112.png"]
        | Monster_fang Moblin_fang, _ -> [%blob "images/115.png"]
        | Monster_fang Lizalfos_talon, _ -> [%blob "images/118.png"]
        | Monster_fang Lynel_hoof, _ -> [%blob "images/124.png"]
        | Monster_fang Hinox_tooth, _ -> [%blob "images/141.png"]
        | Monster_fang Molduga_fin, _ -> [%blob "images/138.png"]
        | Monster_fang White_chuchu_jelly, _ -> [%blob "images/127.png"]
        | Monster_fang Red_chuchu_jelly, _ -> [%blob "images/128.png"]
        | Monster_fang Yellow_chuchu_jelly, _ -> [%blob "images/129.png"]
        | Monster_fang Octorok_eyeball, _ -> [%blob "images/136.png"]
        | Monster_fang Ice_keese_wing, _ -> [%blob "images/131.png"]
        | Monster_fang Fire_keese_wing, _ -> [%blob "images/132.png"]
        | Monster_fang Electric_keese_wing, _ -> [%blob "images/133.png"]
        | Monster_fang Ancient_gear, _ -> [%blob "images/145.png"]
        | Monster_fang Ancient_shaft, _ -> [%blob "images/146.png"]
        | Monster_fang Gibdo_guts, _ -> [%blob "images/gibdo-guts.png"]
        | Monster_fang Horriblin_claw, _ -> [%blob "images/horriblin-claw.png"]
        | Monster_fang Aerocuda_eyeball, _ -> [%blob "images/aerocuda-eyeball.png"]
        | Monster_fang Fire_keese_eyeball, _ -> [%blob "images/fire-keese-eyeball.png"]
        | Monster_fang Ice_keese_eyeball, _ -> [%blob "images/ice-keese-eyeball.png"]
        | Monster_fang Electric_keese_eyeball, _ -> [%blob "images/electric-keese-eyeball.png"]
        | Monster_fang Like_like_stone, _ -> [%blob "images/like-like-stone.png"]
        | Monster_fang Fire_like_stone, _ -> [%blob "images/fire-like-stone.png"]
        | Monster_fang Ice_like_stone, _ -> [%blob "images/ice-like-stone.png"]
        | Monster_fang Shock_like_stone, _ -> [%blob "images/shock-like-stone.png"]
        | Monster_guts Bokoblin_guts, _ -> [%blob "images/113.png"]
        | Monster_guts Moblin_guts, _ -> [%blob "images/116.png"]
        | Monster_guts Lizalfos_tail, _ -> [%blob "images/119.png"]
        | Monster_guts Lynel_guts, _ -> [%blob "images/125.png"]
        | Monster_guts Hinox_guts, _ -> [%blob "images/142.png"]
        | Monster_guts Molduga_guts, _ -> [%blob "images/139.png"]
        | Monster_guts Keese_eyeball, _ -> [%blob "images/134.png"]
        | Monster_guts Icy_lizalfos_tail, _ -> [%blob "images/120.png"]
        | Monster_guts Red_lizalfos_tail, _ -> [%blob "images/121.png"]
        | Monster_guts Yellow_lizalfos_tail, _ -> [%blob "images/122.png"]
        | Monster_guts Ancient_core, _ -> [%blob "images/147.png"]
        | Monster_guts Giant_ancient_core, _ -> [%blob "images/148.png"]
        | Monster_guts Horriblin_guts, _ -> [%blob "images/horriblin-guts.png"]
        | Monster_guts Electric_lizalfos_tail, _ -> [%blob "images/electric-lizalfos-tail.png"]
        | Monster_guts Fire_breath_lizalfos_tail, _ -> [%blob "images/fire-breath-lizalfos-tail.png"]
        | Monster_guts Ice_breath_lizalfos_tail, _ -> [%blob "images/ice-breath-lizalfos-tail.png"]
        | Monster_guts Blue_lizalfos_tail, _ -> [%blob "images/blue-lizalfos-tail.png"]
        | Monster_guts Black_lizalfos_tail, _ -> [%blob "images/black-lizalfos-tail.png"]
        | Dragon_scales Dinraal, _ -> [%blob "images/55.png"]
        | Dragon_scales Naydra, _ -> [%blob "images/59.png"]
        | Dragon_scales Farosh, _ -> [%blob "images/63.png"]
        | Dragon_claws Dinraal, _ -> [%blob "images/56.png"]
        | Dragon_claws Naydra, _ -> [%blob "images/60.png"]
        | Dragon_claws Farosh, _ -> [%blob "images/64.png"]
        | Dragon_fangs Dinraal, _ -> [%blob "images/57.png"]
        | Dragon_fangs Naydra, _ -> [%blob "images/61.png"]
        | Dragon_fangs Farosh, _ -> [%blob "images/65.png"]
        | Dragon_horns Dinraal, _ -> [%blob "images/58.png"]
        | Dragon_horns Naydra, _ -> [%blob "images/62.png"]
        | Dragon_horns Farosh, _ -> [%blob "images/66.png"]
        | Skyshroom, _ -> [%blob "images/skyshroom.png"]
        | Dazzlefruit, _ -> [%blob "images/dazzlefruit.png"]
        | Korok_frond, _ -> [%blob "images/korok-frond.png"]
        | Ancient_arowana, _ -> [%blob "images/ancient-arowana.png"]
        | Hylian_tomato, _ -> [%blob "images/hylian-tomato.png"]
        | Golden_apple, _ -> [%blob "images/golden-apple.png"]
        | Hateno_cheese, _ -> [%blob "images/hateno-cheese.png"]
        | Oil_jar, _ -> [%blob "images/oil-jar.png"]
        | Sun_pumpkin, _ -> [%blob "images/sun-pumpkin.png"]
        | Sundelion, _ -> [%blob "images/sundelion.png"]
        | Stambulb, _ -> [%blob "images/stambulb.png"]
        | Splash_fruit, _ -> [%blob "images/splash-fruit.png"]
        | Deep_firefly, _ -> [%blob "images/deep-firefly.png"]
        | Glowing_cave_fish, _ -> [%blob "images/glowing-cave-fish.png"]
        | Brightcap, _ -> [%blob "images/brightcap.png"]
        | Sticky_lizard, _ -> [%blob "images/sticky-lizard.png"]
        | Sticky_frog, _ -> [%blob "images/sticky-frog.png"]
      in
      compute blob)
