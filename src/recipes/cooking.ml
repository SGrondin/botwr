open! Core_kernel

let ( << ), ( >> ) = ( lsl ), ( lsr )

module Algo = struct
  type t =
    | Balanced
    | Maximize
  [@@deriving sexp, equal]
end

module Hearts = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_hearts = function
  | Nothing
   |Restores _ ->
    0
  | Full_plus_bonus x ->
    let theoretical_gain = max_hearts + x in
    let actual_gain = min theoretical_gain 30 in
    let wasted = theoretical_gain - actual_gain in
    100 + (actual_gain << 1) - wasted
end

module Stamina = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_stamina ~num_effect_ingredients ~random_bonus = function
  | Nothing -> 0
  | Restores theoretical_gain ->
    let actual_gain = min theoretical_gain max_stamina in
    let wasted = theoretical_gain - actual_gain in
    100 + (actual_gain << 2) + (if random_bonus then 2 else 0) - wasted - (num_effect_ingredients << 1)
  | Full_plus_bonus x ->
    let theoretical_gain = max_stamina + x in
    let actual_gain = min theoretical_gain (max_stamina + 10) in
    let wasted = theoretical_gain - actual_gain in
    100 + (actual_gain << 2) + (if random_bonus then 2 else 0) - wasted - (num_effect_ingredients << 1)
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
    | Sneaky    of bonus
    | Mighty    of bonus
    | Tough     of bonus
  [@@deriving sexp, compare, equal, variants]

  let potency = function
  | Nothing -> 0
  | Spicy { potency; _ } -> potency
  | Chilly { potency; _ } -> potency
  | Electro { potency; _ } -> potency
  | Fireproof { potency; _ } -> potency
  | Hasty { potency; _ } -> potency
  | Sneaky { potency; _ } -> potency
  | Mighty { potency; _ } -> potency
  | Tough { potency; _ } -> potency

  let duration = function
  | Nothing -> 0
  | Spicy { duration; _ } -> duration
  | Chilly { duration; _ } -> duration
  | Electro { duration; _ } -> duration
  | Fireproof { duration; _ } -> duration
  | Hasty { duration; _ } -> duration
  | Sneaky { duration; _ } -> duration
  | Mighty { duration; _ } -> duration
  | Tough { duration; _ } -> duration

  let score ~num_effect_ingredients ~random_bonus ~algo = function
  | Nothing -> 0
  | Spicy { potency; wasted; duration }
   |Chilly { potency; wasted; duration }
   |Electro { potency; wasted; duration }
   |Fireproof { potency; wasted; duration }
   |Hasty { potency; wasted; duration }
   |Sneaky { potency; wasted; duration }
   |Mighty { potency; wasted; duration }
   |Tough { potency; wasted; duration } ->
    let actual_duration = min duration 1800 in
    let wasted_duration = duration - actual_duration in
    let penalty =
      match algo with
      | Algo.Balanced -> (num_effect_ingredients << 4) + wasted + wasted_duration
      | Algo.Maximize -> 0
    in
    100 + (potency << 6) + (actual_duration >> 3) + (if random_bonus then 12 else 0) - penalty
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
    num_ingredients: int;
    num_effect_ingredients: int;
    random_effects: Special_bonus.t list;
  }
  [@@deriving sexp, compare, equal, fields]

  let score ~max_hearts ~max_stamina ~algo = function
  | { hearts; stamina; effect; num_ingredients; num_effect_ingredients; random_effects } ->
    let random_bonus =
      match random_effects with
      | _ :: _ -> true
      | _ -> false
    in
    Hearts.score ~max_hearts hearts
    + Stamina.score ~max_stamina ~num_effect_ingredients ~random_bonus stamina
    + Effect.score ~num_effect_ingredients ~random_bonus ~algo effect
    - num_ingredients
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
    Glossary.Map.fold map ~init:([], 0, 0) ~f:(fun ~key:g ~data:count (acc, num, num_effect) ->
        let ingredient = Glossary.to_ingredient g in
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
      | { hearts = Always 0; _ } -> Nothing
      | { hearts = Always x; _ }
       |{ hearts = Diminishing { first = x; _ }; _ }
        when is_tonic ->
        Restores (x - 3)
      | { hearts = Always x; _ }
       |{ hearts = Diminishing { first = x; _ }; _ } ->
        Restores x
    in
    let stamina =
      match res.effect with
      | Energizing (Flat bonus)
       |Energizing (Scaling (bonus, _, _, _, _)) ->
        Stamina.Restores bonus
      | Enduring (Flat bonus)
       |Enduring (Scaling (bonus, _, _, _, _)) ->
        Stamina.Full_plus_bonus bonus
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
       |Energizing _
       |Enduring _ ->
        Nothing
      | Spicy x -> convert 6 99 Effect.spicy x
      | Chilly x -> convert 6 99 Effect.chilly x
      | Electro x -> convert 4 6 Effect.electro x
      | Fireproof x -> convert 7 99 Effect.fireproof x
      | Hasty x -> convert 5 7 Effect.hasty x
      | Sneaky x -> convert 6 9 Effect.sneaky x
      | Mighty x -> convert 5 7 Effect.mighty x
      | Tough x -> convert 5 9 Effect.tough x
    in
    let random_effects : Special_bonus.t list =
      match res.critical, hearts, stamina, effect with
      | false, _, _, _ -> []
      | true, Full_plus_bonus _, _, _ -> [ Yellow_hearts ]
      | true, _, Full_plus_bonus _, _ -> [ Yellow_wheels; Red_hearts ]
      | true, _, Restores _, _ -> [ Green_wheels; Red_hearts ]
      | true, _, _, Nothing -> [ Red_hearts ]
      | true, _, _, _ -> [ Potency; Duration ]
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
        }
      | [ Red_hearts ], Restores x ->
        {
          hearts = Restores (x + 3);
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects = [];
        }
      | [ Red_hearts ], Nothing ->
        {
          hearts = Restores 3;
          stamina;
          effect;
          num_ingredients;
          num_effect_ingredients;
          random_effects = [];
        }
      | _ -> { hearts; stamina; effect; num_ingredients; num_effect_ingredients; random_effects }
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
      Dubious
  )
