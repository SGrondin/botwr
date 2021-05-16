open! Core_kernel

let ( << ), ( >> ) = ( lsl ), ( lsr )

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
    (actual_gain << 1) - wasted
end

module Stamina = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_stamina = function
  | Nothing -> 0
  | Restores theoretical_gain ->
    let actual_gain = min theoretical_gain max_stamina in
    let wasted = theoretical_gain - actual_gain in
    (actual_gain << 1) - wasted
  | Full_plus_bonus x ->
    let theoretical_gain = max_stamina + x in
    let actual_gain = min theoretical_gain 20 in
    let wasted = theoretical_gain - actual_gain in
    (actual_gain << 1) - wasted
end

module Effect = struct
  type bonus = {
    potency: int;
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
  [@@deriving sexp, compare, equal]

  let score = function
  | Nothing -> 0
  | Spicy { potency; duration }
   |Chilly { potency; duration }
   |Electro { potency; duration }
   |Fireproof { potency; duration }
   |Hasty { potency; duration }
   |Sneaky { potency; duration }
   |Mighty { potency; duration }
   |Tough { potency; duration } ->
    let actual = min potency 3 in
    let wasted = potency - actual in
    (actual * (duration >> 3)) - (wasted << 4)
end

module Meal = struct
  type t = {
    hearts: Hearts.t;
    stamina: Stamina.t;
    effect: Effect.t;
    num_ingredients: int;
  }
  [@@deriving sexp, compare, equal, fields]

  let score ~max_hearts ~max_stamina = function
  | { hearts; stamina; effect; num_ingredients } ->
    Hearts.score ~max_hearts hearts
    + Stamina.score ~max_stamina stamina
    + Effect.score effect
    - num_ingredients
end

type t =
  | Food    of Meal.t
  | Elixir  of Meal.t
  | Dubious
  | Failed  of string
[@@deriving sexp, compare, equal]

let cook ~max_hearts ~max_stamina map =
  let ingredients, num_ingredients =
    Glossary.Map.fold map ~init:([], 0) ~f:(fun ~key:g ~data:count (acc, i) ->
        Ingredient.merge (Glossary.to_ingredient g) ~count :: acc, i + count)
  in
  match List.reduce ingredients ~f:Ingredient.combine with
  | None -> Failed "No ingredients"
  | Some { category = Spice; _ }
   |Some { category = Critter; _ }
   |Some { category = Monster; _ }
   |Some { category = Dubious; _ } ->
    Dubious
  | Some res -> (
    let hearts =
      match res.effect, res.hearts with
      | Hearty x, _ -> Hearts.Full_plus_bonus x
      | _, 0 -> Nothing
      | _, x -> Restores (min x max_hearts)
    in
    let stamina =
      match res.effect with
      | Energizing { bonus; _ } -> Stamina.Restores (min bonus max_stamina)
      | Enduring { bonus; _ } -> Stamina.Full_plus_bonus bonus
      | _ -> Nothing
    in
    let convert_duration : Ingredient.Duration.t -> int = function
      | Always x -> x
      | Diminishing { first; _ } -> first
    in
    let effect =
      match res.effect with
      | Nothing
       |Neutral _
       |Hearty _
       |Energizing _
       |Enduring _ ->
        Effect.Nothing
      | Spicy { potency; duration; _ } -> Spicy { potency; duration = convert_duration duration }
      | Chilly { potency; duration; _ } -> Chilly { potency; duration = convert_duration duration }
      | Electro { potency; duration; _ } -> Electro { potency; duration = convert_duration duration }
      | Fireproof { potency; duration; _ } -> Fireproof { potency; duration = convert_duration duration }
      | Hasty { potency; duration; _ } -> Hasty { potency; duration = convert_duration duration }
      | Sneaky { potency; duration; _ } -> Sneaky { potency; duration = convert_duration duration }
      | Mighty { potency; duration; _ } -> Mighty { potency; duration = convert_duration duration }
      | Tough { potency; duration; _ } -> Tough { potency; duration = convert_duration duration }
    in
    match res.category with
    | Food -> Food { hearts; stamina; effect; num_ingredients }
    | Elixir -> Elixir { hearts; stamina; effect; num_ingredients }
    | _ -> Dubious
  )
