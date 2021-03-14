open! Core_kernel

module Hearts = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_hearts = function
  | Nothing -> 0
  | Restores x -> x
  | Full_plus_bonus x -> max_hearts + x
end

module Stamina = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_stamina = function
  | Nothing -> 0
  | Restores x -> x
  | Full_plus_bonus x -> max_stamina + x
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
    potency * duration
end

module Meal = struct
  type t = {
    hearts: Hearts.t;
    stamina: Stamina.t;
    effect: Effect.t;
  }
  [@@deriving sexp, compare, equal]

  let score ~max_hearts ~max_stamina = function
  | { hearts; stamina; effect } ->
    Hearts.score ~max_hearts hearts + Stamina.score ~max_stamina stamina + Effect.score effect
end

type t =
  | Food    of Meal.t
  | Elixir  of Meal.t
  | Dubious
  | Failed  of string
[@@deriving sexp, compare, equal]

let cook map =
  let ingredients =
    Glossary.Map.fold map ~init:[] ~f:(fun ~key:g ~data:count acc ->
        Ingredient.merge (Glossary.to_ingredient g) ~count :: acc)
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
      | _, x -> Restores x
    in
    let stamina =
      match res.effect with
      | Energizing { bonus; _ } -> Stamina.Restores bonus
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
    | Food -> Food { hearts; stamina; effect }
    | Elixir -> Elixir { hearts; stamina; effect }
    | _ -> Dubious
  )

module KindMap = Map.Make (Ingredient.Effect.Kind)
module GlossaryTable = Hashtbl.Make (Glossary)
module IngredientTable = Hashtbl.Make (Ingredient)

module Compute = struct
  let filter ~kind items =
    let all =
      let table = GlossaryTable.create () in
      List.iter items ~f:(fun (x, n) ->
          GlossaryTable.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
      table
    in
    GlossaryTable.fold all ~init:[] ~f:(fun ~key ~data acc ->
        match key |> Glossary.to_ingredient |> Ingredient.to_kind with
        | Neutral -> Fn.apply_n_times ~n:(min data 4) (List.cons key) acc
        | x when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 5) (List.cons key) acc
        | _ -> acc)

  open Combinations

  let combine list = Advanced.generate 5 list

  let best ~max_hearts ~max_stamina combinations =
    Advanced.best combinations ~score:(fun (recipe : Recipe.t) ->
        match cook recipe with
        | Food meal
         |Elixir meal ->
          Meal.score ~max_hearts ~max_stamina meal
        | Dubious
         |Failed _ ->
          -1000)

  type timings = {
    t_filter: Int64.t;
    t_combine: Int64.t;
    t_count: Int64.t;
    t_cook: Int64.t;
  }
  [@@deriving sexp]

  type t = {
    score: int;
    count: int;
    best: Advanced.Recipes.book Advanced.Recipes.Map.t;
    timings: timings;
  }
  [@@deriving sexp]

  let to_string { score; count; best; timings } =
    sprintf
      !"Best out of %d with %d points:\n%{Combinations.Advanced.Recipes}\nTimings: %{sexp: timings}"
      count score best timings

  let time () = ref (Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64)

  let diff_time r =
    let t0 = !r in
    let t1 = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
    r := t1;
    Int64.((t1 - t0) / 1_000_000L)

  let run ~max_hearts ~max_stamina ~kind items =
    let r = time () in
    let list = filter ~kind items in
    let t_filter = diff_time r in
    let combinations = combine list in
    let t_combine = diff_time r in
    let count = Advanced.count combinations in
    let t_count = diff_time r in
    let score, best = best ~max_hearts ~max_stamina combinations in
    let t_cook = diff_time r in
    { score; count; best; timings = { t_filter; t_combine; t_count; t_cook } }
end
