open! Core_kernel

let ( << ), ( >> ) = ( lsl ), ( lsr )

let debug = ref false

module Algo = struct
  type t =
    | Balanced
    | Maximize
  [@@deriving sexp, equal, enumerate]

  let to_string = function
  | Balanced -> "Max power + Balanced duration"
  | Maximize -> "Max power + Max duration"
end

module Hearts = struct
  type t =
    | Nothing
    | Restores        of Ingredient.Hearts.quarters
    | Full_plus_bonus of int
    | Unglooms        of int * Ingredient.Hearts.quarters
  [@@deriving sexp, compare, equal]

  let score ~max_hearts ~gloomy_hearts ~(algo : Algo.t) = function
  | Nothing -> 0
  | Restores (Quarters theoretical_gain) ->
    let actual_gain = min theoretical_gain (max_hearts << 2) in
    let wasted = theoretical_gain - actual_gain in
    0 - wasted
  | Full_plus_bonus x ->
    let theoretical_gain = max_hearts + x in
    let actual_gain = min theoretical_gain 30 in
    let wasted = theoretical_gain - actual_gain in
    100 + (actual_gain << 2) - (wasted << 1)
  | Unglooms (unglooms_theoretical_gain, Quarters quarters_theoretical_gain) ->
    let unglooms_actual_gain = min unglooms_theoretical_gain gloomy_hearts in
    let unglooms_wasted = unglooms_theoretical_gain - unglooms_actual_gain in
    let real_max_hearts =
      match algo with
      | Balanced -> unglooms_actual_gain
      | Maximize -> max_hearts - (gloomy_hearts - unglooms_actual_gain)
    in
    let quarters_actual_gain = min quarters_theoretical_gain (real_max_hearts << 2) in
    let quarters_wasted =
      match algo with
      | Balanced -> quarters_theoretical_gain - quarters_actual_gain
      | Maximize -> (quarters_theoretical_gain - quarters_actual_gain) * 3
    in

    (* print_endline "------";
       print_endline (sprintf !"unglooms_theoretical_gain: %{sexp#hum: int}" unglooms_theoretical_gain);
       print_endline (sprintf !"quarters_theoretical_gain: %{sexp#hum: int}" quarters_theoretical_gain);
       print_endline (sprintf !"unglooms_actual_gain: %{sexp#hum: int}" unglooms_actual_gain);
       print_endline (sprintf !"unglooms_wasted: %{sexp#hum: int}" unglooms_wasted);
       print_endline (sprintf !"real_max_hearts: %{sexp#hum: int}" real_max_hearts);
       print_endline (sprintf !"quarters_actual_gain: %{sexp#hum: int}" quarters_actual_gain);
       print_endline (sprintf !"quarters_wasted: %{sexp#hum: int}" quarters_wasted); *)
    100
    + (unglooms_actual_gain << 4)
    + (quarters_actual_gain << 1)
    - ((unglooms_wasted << 3) + quarters_wasted)
end

module Stamina = struct
  type restoration = {
    potency: int;
    wasted: int;
  }
  [@@deriving sexp, compare, equal]

  type t =
    | Nothing
    | Restores        of restoration
    | Full_plus_bonus of restoration
  [@@deriving sexp, compare, equal]

  let score ~max_stamina ~num_effect_ingredients ~random_bonus = function
  | Nothing -> 0
  | Restores { potency; wasted } ->
    let actual_gain = min potency max_stamina in
    let wasted = potency - actual_gain + wasted in
    100 + (actual_gain << 3) + (if random_bonus then 2 else 0) - wasted - (num_effect_ingredients << 1)
  | Full_plus_bonus { potency; wasted } ->
    let theoretical_gain = max_stamina + potency in
    let actual_gain = min theoretical_gain (max_stamina + 10) in
    let wasted = theoretical_gain - actual_gain + wasted in
    100 + (actual_gain << 3) + (if random_bonus then 2 else 0) - wasted - (num_effect_ingredients << 1)
end

module Effect = struct
  type bonus = {
    potency: int;
    wasted: int;
    duration: int;
  }
  [@@deriving sexp, compare, equal]

  type t =
    | Nothing
    | Spicy     of bonus
    | Chilly    of bonus
    | Electro   of bonus
    | Fireproof of bonus
    | Hasty     of bonus
    | Rapid     of bonus
    | Sticky    of bonus
    | Sneaky    of bonus
    | Mighty    of bonus
    | Tough     of bonus
    | Bright    of bonus
  [@@deriving sexp, compare, equal, variants]

  let max_potency = function
  | Nothing -> true
  | Spicy { potency; _ } -> potency = 2
  | Chilly { potency; _ } -> potency = 2
  | Electro { potency; _ } -> potency = 3
  | Fireproof { potency; _ } -> potency = 2
  | Hasty { potency; _ } -> potency = 3
  | Rapid { potency; _ } -> potency = 1
  | Sticky { potency; _ } -> potency = 3
  | Sneaky { potency; _ } -> potency = 3
  | Mighty { potency; _ } -> potency = 3
  | Tough { potency; _ } -> potency = 3
  | Bright { potency; _ } -> potency = 3

  let duration = function
  | Nothing -> 0
  | Spicy { duration; _ }
   |Chilly { duration; _ }
   |Electro { duration; _ }
   |Fireproof { duration; _ }
   |Hasty { duration; _ }
   |Rapid { duration; _ }
   |Sticky { duration; _ }
   |Sneaky { duration; _ }
   |Mighty { duration; _ }
   |Tough { duration; _ }
   |Bright { duration; _ } ->
    duration

  let score ~num_effect_ingredients ~random_bonus ~(algo : Algo.t) = function
  | Nothing -> 0
  | Spicy { potency; wasted; duration }
   |Chilly { potency; wasted; duration }
   |Electro { potency; wasted; duration }
   |Fireproof { potency; wasted; duration }
   |Hasty { potency; wasted; duration }
   |Rapid { potency; wasted; duration }
   |Sticky { potency; wasted; duration }
   |Sneaky { potency; wasted; duration }
   |Mighty { potency; wasted; duration }
   |Tough { potency; wasted; duration }
   |Bright { potency; wasted; duration } ->
    let actual_duration = min duration 1800 in
    let wasted_duration = duration - actual_duration in
    let penalty =
      match algo with
      | Balanced ->
        (num_effect_ingredients << 5)
        + (wasted << 4)
        + (wasted_duration << 1)
        + (max 0 (actual_duration - 500) >> 1)
      | Maximize -> wasted + num_effect_ingredients
    in
    100 + (potency << 8) + (actual_duration >> 3) + (if random_bonus then 12 else 0) - penalty
end

module Special_bonus = struct
  type t =
    | Red_hearts
    | Yellow_hearts
    | Green_wheels
    | Yellow_wheels
    | Potency
    | Duration
  [@@deriving sexp, compare, equal]
end

module Meal = struct
  type t = {
    hearts: Hearts.t;
    stamina: Stamina.t;
    effect: Effect.t;
    fused: int;
    num_ingredients: int;
    num_effect_ingredients: int;
    random_effects: Special_bonus.t list;
  }
  [@@deriving sexp, compare, equal, fields]

  let score ~max_hearts ~max_stamina ~gloomy_hearts ~algo ~(kind : Ingredient.Effect.Kind.t)
     { hearts; stamina; effect; num_ingredients = _; fused; num_effect_ingredients; random_effects } =
    let random_bonus =
      match random_effects with
      | _ :: _ -> true
      | _ -> false
    in
    match kind, hearts with
    | Neutral, Restores (Quarters theoretical_gain) ->
      (* Neutral is a special case fully scored here *)
      let actual_gain = min theoretical_gain (max_hearts << 2) in
      let wasted = theoretical_gain - actual_gain in
      let unwanted_effect =
        match effect with
        | Nothing -> 0
        | _ -> 8
      in
      actual_gain - (wasted + num_effect_ingredients + unwanted_effect)
    | Neutral, _ -> -1000
    | _ ->
      Hearts.score ~max_hearts ~gloomy_hearts ~algo hearts
      + Stamina.score ~max_stamina ~num_effect_ingredients ~random_bonus stamina
      + Effect.score ~num_effect_ingredients ~random_bonus ~algo effect
      - fused
end

type t =
  | Food    of Meal.t
  | Elixir  of Meal.t
  | Tonic   of Meal.t
  | Dubious
  | Failed  of string
[@@deriving sexp, compare, equal]

let cook map =
  let ingredients, num_ingredients, num_effect_ingredients =
    Ingredient.Map.fold map ~init:([], 0, 0) ~f:(fun ~key:ingredient ~data:count (acc, num, num_effect) ->
        let effect_or_special = Ingredient.has_effect_or_special ingredient in
        ( Ingredient.merge ingredient ~count :: acc,
          num + count,
          if effect_or_special then num_effect + count else num_effect ))
  in
  match List.reduce ingredients ~f:Ingredient.combine with
  | None -> Failed "No ingredients"
  | Some { category = Spice; _ }
   |Some { category = Critter; _ }
   |Some { category = Monster; _ }
   |Some { category = Dubious; _ }
   |Some { category = Dragon; _ } ->
    Dubious
  | Some res -> (
    let is_tonic =
      match res.category with
      | With_fairy Food -> false
      | With_fairy _ -> true
      | _ -> false
    in
    let hearts =
      match res with
      | { effect = Hearty x; _ } -> Hearts.Full_plus_bonus x
      | { effect = Sunny x; hearts = Always (Quarters _ as q); _ }
       |{ effect = Sunny x; hearts = Diminishing { first = Quarters _ as q; _ }; _ } ->
        Hearts.Unglooms (x, q)
      | { hearts = Always (Quarters 0); _ } -> Nothing
      | { hearts = Always (Quarters x); _ }
       |{ hearts = Diminishing { first = Quarters x; _ }; _ }
        when is_tonic ->
        Restores (Quarters (x - 12))
      | { hearts = Always x; _ }
       |{ hearts = Diminishing { first = x; _ }; _ } ->
        Restores x
    in
    let stamina =
      match res.effect with
      | Energizing (Fifths bonus) ->
        let wasted = bonus % 5 in
        let potency = (bonus - wasted) / 5 in
        Stamina.Restores { wasted; potency }
      | Enduring (Quarters quarters) ->
        let potency = max 1 (quarters >> 2) in
        let wasted = max 0 (quarters - (potency << 2)) in
        Stamina.Full_plus_bonus { potency; wasted }
      | _ -> Nothing
    in
    let effect : Effect.t =
      let convert_duration : Ingredient.Duration.t -> int = function
        | Always x
         |Diminishing { first = x; _ } ->
          x
      in
      let convert lvl2 lvl3 (constructor : Effect.bonus -> Effect.t) :
         Ingredient.Effect.Activity.t -> Effect.t = function
        | { points; duration; _ } when points < lvl2 ->
          constructor { potency = 1; wasted = points - 1; duration = convert_duration duration }
        | { points; duration; _ } when points < lvl3 ->
          constructor { potency = 2; wasted = points - lvl2; duration = convert_duration duration }
        | { points; duration; _ } ->
          constructor { potency = 3; wasted = points - lvl3; duration = convert_duration duration }
      in
      match res.effect with
      | _ when is_tonic -> Nothing
      | Nothing
       |Neutral _
       |Hearty _
       |Sunny _
       |Energizing _
       |Enduring _ ->
        Nothing
      | Spicy x -> convert 6 99 Effect.spicy x
      | Chilly x -> convert 6 99 Effect.chilly x
      | Electro x -> convert 4 6 Effect.electro x
      | Fireproof x -> convert 7 99 Effect.fireproof x
      | Hasty x -> convert 5 7 Effect.hasty x
      | Rapid x -> convert 99 99 Effect.rapid x
      | Sticky x -> convert 5 7 Effect.sticky x
      | Sneaky x -> convert 6 9 Effect.sneaky x
      | Mighty x -> convert 5 7 Effect.mighty x
      | Tough x -> convert 5 9 Effect.tough x
      | Bright x -> convert 5 7 Effect.bright x
    in
    let random_effects : Special_bonus.t list =
      match res.critical, hearts, stamina, effect with
      | false, _, _, _ -> []
      | true, Full_plus_bonus _, _, _ -> [ Yellow_hearts ]
      | true, _, Full_plus_bonus _, _ -> [ Yellow_wheels; Red_hearts ]
      | true, _, Restores _, _ -> [ Green_wheels; Red_hearts ]
      | true, _, _, Nothing -> [ Red_hearts ]
      | true, _, _, _ -> [ Potency; Duration; Red_hearts ]
    in
    let meal : Meal.t =
      match random_effects, hearts with
      | [ Yellow_hearts ], Full_plus_bonus x ->
        {
          hearts = Full_plus_bonus (x + 1);
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects = [];
          fused = res.fused;
        }
      | [ Red_hearts ], Restores (Quarters x) ->
        {
          hearts = Restores (Quarters (x + 12));
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects = [];
          fused = res.fused;
        }
      | [ Red_hearts ], Nothing ->
        {
          hearts = Restores (Quarters 12);
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects = [];
          fused = res.fused;
        }
      | _ ->
        {
          hearts;
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects;
          fused = res.fused;
        }
    in
    match res.category with
    | Food
     |With_fairy Food ->
      Food meal
    | Elixir -> Elixir meal
    | With_fairy _ -> Tonic meal
    | Spice
     |Critter
     |Monster
     |Dragon
     |Dubious ->
      Dubious)
