open! Core_kernel

module Hearts = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_hearts ~factor = function
  | Nothing
   |Restores _ ->
    0
  | Full_plus_bonus x -> (max_hearts + x) * factor
end

module Stamina = struct
  type t =
    | Nothing
    | Restores        of int
    | Full_plus_bonus of int
  [@@deriving sexp, compare, equal]

  let score ~max_stamina ~factor = function
  | Nothing -> 0
  | Restores x -> x * factor
  | Full_plus_bonus x -> (max_stamina + x) * factor
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

  let score ~factor = function
  | Nothing -> 0
  | Spicy { potency; duration }
   |Chilly { potency; duration }
   |Electro { potency; duration }
   |Fireproof { potency; duration }
   |Hasty { potency; duration }
   |Sneaky { potency; duration }
   |Mighty { potency; duration }
   |Tough { potency; duration } ->
    potency * (duration / 30) * factor
end

module Meal = struct
  type t = {
    hearts: Hearts.t;
    stamina: Stamina.t;
    effect: Effect.t;
    num_ingredients: int;
  }
  [@@deriving sexp, compare, equal]

  let score ~max_hearts ~max_stamina ~factor = function
  | { hearts; stamina; effect; num_ingredients } ->
    Hearts.score ~max_hearts ~factor hearts
    + Stamina.score ~max_stamina ~factor stamina
    + Effect.score effect ~factor
    - num_ingredients
end

type t =
  | Food    of Meal.t
  | Elixir  of Meal.t
  | Dubious
  | Failed  of string
[@@deriving sexp, compare, equal]

let cook map =
  let ingredients, num_ingredients =
    Glossary.Map.fold map ~init:([], 0) ~f:(fun ~key:g ~data:count (acc, i) ->
        Ingredient.merge (Glossary.to_ingredient g) ~count :: acc, i + 1)
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
    | Food -> Food { hearts; stamina; effect; num_ingredients }
    | Elixir -> Elixir { hearts; stamina; effect; num_ingredients }
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

  let combine ~max_hearts ~max_stamina ~factor list =
    let cache = Int.Table.create () in
    let init = (0, Advanced.Recipes.Map.empty), 0 in
    let f (((best_score, _) as best), i) (recipes : Advanced.Recipes.t) =
      let score =
        Advanced.Recipes.Map.fold ~init:0 recipes ~f:(fun ~key ~data:{ recipe; num } acc ->
            let score =
              Int.Table.find_or_add cache key ~default:(fun () ->
                  match cook recipe with
                  | Food meal
                   |Elixir meal ->
                    Meal.score ~max_hearts ~max_stamina ~factor meal
                  | Dubious
                   |Failed _ ->
                    -1000)
            in
            (score * num) + acc)
      in
      if score > best_score then (score, recipes), i + 1 else best, i + 1
    in
    Advanced.generate ~init ~f 5 list

  type t = {
    score: int;
    count: int;
    best: Advanced.Recipes.book Advanced.Recipes.Map.t;
    duration: Int64.t;
  }
  [@@deriving sexp]

  let to_string { score; count; best; duration } =
    sprintf
      !"Best of %d with %d points (%Lds) :\n%{Combinations.Advanced.Recipes}"
      count score
      Int64.(duration / 1_000_000L)
      best

  let time () = ref (Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64)

  let diff_time r =
    let t0 = !r in
    let t1 = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
    r := t1;
    Int64.((t1 - t0) / 1_000L)

  let run ~max_hearts ~max_stamina ~factor ~kind items =
    let r = time () in
    let (score, best), count = filter ~kind items |> combine ~max_hearts ~max_stamina ~factor in
    let duration = diff_time r in
    { score; count; best; duration }
end
