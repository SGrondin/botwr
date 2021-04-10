open! Core_kernel
open Items

let blobs =
  List.fold all ~init:Map.empty ~f:(fun acc key ->
      let blob =
        match key with
        | Palm_fruit -> [%blob "images/2.png"]
        | Apple -> [%blob "images/3.png"]
        | Wildberry -> [%blob "images/4.png"]
        | Hylian_shroom -> [%blob "images/12.png"]
        | Hyrule_herb -> [%blob "images/25.png"]
        | Hyrule_bass -> [%blob "images/69.png"]
        | Sanke_carp -> [%blob "images/77.png"]
        | Raw_gourmet_meat -> [%blob "images/36.png"]
        | Raw_whole_bird -> [%blob "images/37.png"]
        | Raw_prime_meat -> [%blob "images/38.png"]
        | Raw_bird_thigh -> [%blob "images/39.png"]
        | Raw_meat -> [%blob "images/40.png"]
        | Raw_bird_drumstick -> [%blob "images/41.png"]
        | Bird_egg -> [%blob "images/46.png"]
        | Fresh_milk -> [%blob "images/48.png"]
        | Acorn -> [%blob "images/44.png"]
        | Chickaloo_tree_nut -> [%blob "images/45.png"]
        | Hylian_rice -> [%blob "images/43.png"]
        | Tabantha_wheat -> [%blob "images/47.png"]
        | Cane_sugar -> [%blob "images/49.png"]
        | Goat_butter -> [%blob "images/50.png"]
        | Goron_spice -> [%blob "images/51.png"]
        | Rock_salt -> [%blob "images/52.png"]
        | Hearty_truffle -> [%blob "images/11.png"]
        | Hearty_bass -> [%blob "images/68.png"]
        | Hearty_radish -> [%blob "images/23.png"]
        | Hearty_blueshell_snail -> [%blob "images/81.png"]
        | Hearty_durian -> [%blob "images/1.png"]
        | Big_hearty_truffle -> [%blob "images/10.png"]
        | Hearty_salmon -> [%blob "images/67.png"]
        | Hearty_lizard -> [%blob "images/101.png"]
        | Big_hearty_radish -> [%blob "images/22.png"]
        | Stamella_shroom -> [%blob "images/14.png"]
        | Restless_cricket -> [%blob "images/93.png"]
        | Courser_bee_honey -> [%blob "images/42.png"]
        | Bright_eyed_crab -> [%blob "images/84.png"]
        | Staminoka_bass -> [%blob "images/70.png"]
        | Energetic_rhino_beetle -> [%blob "images/96.png"]
        | Endura_shroom -> [%blob "images/13.png"]
        | Tireless_frog -> [%blob "images/99.png"]
        | Endura_carrot -> [%blob "images/24.png"]
        | Spicy_pepper -> [%blob "images/6.png"]
        | Warm_safflina -> [%blob "images/29.png"]
        | Summerwing_butterfly -> [%blob "images/87.png"]
        | Sunshroom -> [%blob "images/16.png"]
        | Warm_darner -> [%blob "images/91.png"]
        | Sizzlefin_trout -> [%blob "images/72.png"]
        | Hydromelon -> [%blob "images/5.png"]
        | Cool_safflina -> [%blob "images/28.png"]
        | Winterwing_butterfly -> [%blob "images/86.png"]
        | Chillshroom -> [%blob "images/15.png"]
        | Cold_darner -> [%blob "images/90.png"]
        | Chillfin_trout -> [%blob "images/71.png"]
        | Voltfruit -> [%blob "images/7.png"]
        | Electric_safflina -> [%blob "images/30.png"]
        | Thunderwing_butterfly -> [%blob "images/88.png"]
        | Zapshroom -> [%blob "images/17.png"]
        | Electric_darner -> [%blob "images/92.png"]
        | Voltfin_trout -> [%blob "images/73.png"]
        | Fireproof_lizard -> [%blob "images/102.png"]
        | Smotherwing_butterfly -> [%blob "images/89.png"]
        | Rushroom -> [%blob "images/18.png"]
        | Swift_carrot -> [%blob "images/26.png"]
        | Hightail_lizard -> [%blob "images/100.png"]
        | Fleet_lotus_seeds -> [%blob "images/8.png"]
        | Swift_violet -> [%blob "images/31.png"]
        | Hot_footed_frog -> [%blob "images/98.png"]
        | Blue_nightshade -> [%blob "images/34.png"]
        | Sneaky_river_snail -> [%blob "images/80.png"]
        | Sunset_firefly -> [%blob "images/97.png"]
        | Silent_shroom -> [%blob "images/21.png"]
        | Stealthfin_trout -> [%blob "images/74.png"]
        | Silent_princess -> [%blob "images/35.png"]
        | Mighty_thistle -> [%blob "images/32.png"]
        | Bladed_rhino_beetle -> [%blob "images/94.png"]
        | Mighty_bananas -> [%blob "images/9.png"]
        | Razorshroom -> [%blob "images/19.png"]
        | Mighty_carp -> [%blob "images/75.png"]
        | Razorclaw_crab -> [%blob "images/82.png"]
        | Mighty_porgy -> [%blob "images/78.png"]
        | Armoranth -> [%blob "images/33.png"]
        | Rugged_rhino_beetle -> [%blob "images/95.png"]
        | Fortified_pumpkin -> [%blob "images/27.png"]
        | Ironshroom -> [%blob "images/20.png"]
        | Armored_carp -> [%blob "images/76.png"]
        | Ironshell_crab -> [%blob "images/83.png"]
        | Armored_porgy -> [%blob "images/79.png"]
        | Monster_horn -> [%blob "images/114.png"]
        | Monster_fang -> [%blob "images/115.png"]
        | Monster_guts -> [%blob "images/113.png"]
        (* | Fairy -> {|85|} *)
        (* | Monster_extract -> {|53|} *)
        (* | Star_fragment -> {|54|} *)
      in
      let b64 =
        match Base64.encode ~pad:true blob with
        | Ok x -> x
        | Error (`Msg msg) -> failwith msg
      in
      Map.set acc ~key ~data:(sprintf "data:image/png;base64,%s" b64))