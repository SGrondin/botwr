open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let best (x, _) (y, _) = [%compare: int] y x

let worst (x, _) (y, _) = [%compare: int] x y

let filter ~kind ~(category : Glossary.Category.t) ~use_special grouped =
  let open Glossary in
  let neutrals = Queue.create () in
  let neutrals_wasteful = Queue.create () in
  let monsters = Queue.create () in
  let dragons = Queue.create () in
  let add_to ~n q key = Fn.apply_n_times ~n (fun () -> Queue.enqueue q (Variants.to_rank key, key)) () in
  let basics =
    Table.fold grouped ~init:[] ~f:(fun ~key ~data acc ->
        let ingredient = to_ingredient key in
        match Ingredient.to_kind ingredient, ingredient.category, category with
        | Nothing, Food, Meals
         |Nothing, Food, Any
         |Nothing, Spice, Meals
         |Nothing, Spice, Any
         |Neutral, Food, Meals
         |Neutral, Food, Any
         |Neutral, Spice, Meals
         |Neutral, Spice, Any
          when not ([%equal: Ingredient.Effect.Kind.t] kind Hearty) ->
          let () =
            match ingredient with
            | { effect = Neutral (Diminishing _); _ } when data > 0 ->
              (* Add up to 1 to neutrals, up to 3 to neutrals_wasteful *)
              add_to ~n:1 neutrals key;
              add_to ~n:(min 3 (data - 1)) neutrals_wasteful key;
              ()
            | _ -> add_to ~n:(min data 4) neutrals key
          in
          acc
        | _, Dragon, _ when use_special ->
          add_to ~n:data dragons key;
          acc
        | _, Monster, Elixirs
         |_, Monster, Any ->
          add_to ~n:(min data 4) monsters key;
          acc
        | x, Food, Any
         |x, Food, Meals
          when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 5) (List.cons key) acc
        | x, Critter, Any
         |x, Critter, Elixirs
          when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 4) (List.cons key) acc
        | _, With_fairy _, _ when use_special -> Fn.apply_n_times ~n:(min data 4) (List.cons key) acc
        | _ -> acc)
  in
  let top ?(up_to = 4) queue init =
    let up_to = max 0 up_to in
    if up_to <= 0
    then init
    else (
      let compare =
        match kind with
        | Nothing
         |Neutral
         |Enduring
         |Energizing
         |Hearty ->
          worst
        | Chilly
         |Electro
         |Fireproof
         |Hasty
         |Mighty
         |Sneaky
         |Spicy
         |Tough ->
          best
      in
      Queue.to_array queue |> Array.sorted_copy ~compare |> fun arr ->
      Array.slice arr 0 (min up_to (Array.length arr))
      |> Array.fold_right ~init ~f:(fun (_, x) acc -> x :: acc)
    )
  in
  Queue.to_array dragons |> Array.sorted_copy ~compare:best |> fun arr ->
  Array.slice arr 0 (min 4 (Array.length arr))
  |> Array.fold_until ~init:(basics, 0) ~finish:fst ~f:(fun (acc, time) (_, x) ->
         let new_time =
           match to_ingredient x with
           | { effect = Neutral (Diminishing { first; next = 30 }); _ } -> time + first
           | _ -> failwithf !"Invalid dragon part at %{Source_code_position}" [%here] ()
         in
         if new_time < 1800 then Continue (x :: acc, new_time) else Stop (x :: acc))
  |> top neutrals
  |> top neutrals_wasteful ~up_to:(4 - Queue.length neutrals)
  |> top monsters

open Combinations
open Cooking

let time () = ref (Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64)

let diff_time r =
  let t0 = !r in
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
  r := t1;
  Int64.((t1 - t0) // 1_000_000_000L)

type folder = {
  first: int * Recipe.t list;
  second: int * Recipe.t list;
  third: int * Recipe.t list;
}

let combine ~max_hearts ~max_stamina ~algo list =
  let cache = Recipe.Table.create () in
  let f (({ first = score1, ll1; second = score2, ll2; third = score3, ll3 } as acc), i)
     (recipe : Recipe.t) =
    let score =
      Recipe.Table.find_or_add cache recipe ~default:(fun () ->
          match cook recipe with
          | Food meal
           |Elixir meal
           |Tonic meal ->
            Meal.score ~max_hearts ~max_stamina ~algo meal
          | Dubious
           |Failed _ ->
            -1_000_000)
    in
    let updated =
      if score < score3
      then acc
      else if score = score3
      then { acc with third = score, recipe :: ll3 }
      else if score < score2
      then { acc with third = score, [ recipe ] }
      else if score = score2
      then { acc with second = score, recipe :: ll2 }
      else if score < score1
      then { acc with second = score, [ recipe ]; third = acc.second }
      else if score = score1
      then { acc with first = score, recipe :: ll1 }
      else { first = score, [ recipe ]; second = acc.first; third = acc.second }
    in
    updated, i + 1
  in
  generate_all ~init:({ first = 0, []; second = 0, []; third = 0, [] }, 0) ~f 5 list

let rarity_score grouped recipe =
  Glossary.Map.fold recipe ~init:0.0 ~f:(fun ~key ~data acc ->
      let remaining = Glossary.Table.find grouped key |> Option.value_exn ~here:[%here] in
      let removing = data // remaining in
      Float.(removing + acc))

let break_ties grouped recipe =
  let rarity = rarity_score grouped recipe in
  let hearts =
    match cook recipe with
    | Food { hearts = Restores (Quarters x); _ }
     |Elixir { hearts = Restores (Quarters x); _ } ->
      x lsr 2
    | _ -> 0
  in
  rarity, Float.(of_int hearts - rarity)

let top_most_common ?(n = 3) grouped recipes =
  let sorted =
    List.fold recipes ~init:Float.Map.empty ~f:(fun acc recipe ->
        let rarity, tiebreaker = break_ties grouped recipe in
        Float.Map.add_multi acc ~key:tiebreaker ~data:(recipe, rarity))
  in
  Float.Map.to_sequence sorted ~order:`Decreasing_key
  |> Sequence.concat_map ~f:(fun (x, ll) -> List.map ll ~f:(Tuple2.create x) |> Sequence.of_list)
  |> Fn.flip Sequence.take n
  |> Sequence.to_list

type iteration = {
  tiebreaker: float;
  rarity: float;
  score: int;
  recipe: Recipe.t;
}
[@@deriving sexp, equal]

type t = {
  iterations: iteration list;
  count: int;
  duration: float;
}
[@@deriving sexp, equal]

let to_string { iterations; count; duration } =
  let buf = Buffer.create 128 in
  bprintf buf "(%ds)" (Float.to_int duration);
  List.iter iterations ~f:(fun { rarity = _; tiebreaker; score; recipe } ->
      bprintf buf
        !"\n%d pts (%d, %f)\n%{Recipe}\n%{sexp: Cooking.t}"
        score count tiebreaker recipe (cook recipe));
  Buffer.contents buf

let top_sort grouped ll =
  let rec loop from (ll, count) =
    match from with
    | [] -> List.rev ll |> List.concat
    | (score, recipes) :: rest ->
      let next =
        List.dedup_and_sort recipes ~compare:[%compare: Recipe.t]
        |> top_most_common ~n:(3 - count) grouped
        |> List.map ~f:(fun (tiebreaker, (recipe, rarity)) -> { tiebreaker; rarity; score; recipe })
      in
      let len = count + List.length next in
      if len >= 3 then List.rev (next :: ll) |> List.concat else loop rest (next :: ll, len)
  in
  loop ll ([], 0)

type settings = {
  max_hearts: int;
  max_stamina: int;
  algo: Algo.t;
  kind: Ingredient.Effect.Kind.t;
  category: Glossary.Category.t;
  use_special: bool;
}

let run { max_hearts; max_stamina; algo; kind; category; use_special } items =
  let r = time () in
  let grouped = group items in
  let { first; second; third }, count =
    filter ~kind ~category ~use_special grouped |> combine ~max_hearts ~max_stamina ~algo
  in
  let iterations = top_sort grouped [ first; second; third ] in
  let duration = diff_time r in
  { iterations; count; duration }
