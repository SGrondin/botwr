open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let filter ~kind ~(category : Glossary.Category.t) grouped =
  Glossary.Table.fold grouped ~init:[] ~f:(fun ~key ~data acc ->
      match Glossary.kind_and_category key, category with
      | (Neutral, _), Any
       |(Neutral, Meals), Meals
       |(Neutral, Elixirs), Elixirs ->
        Fn.apply_n_times ~n:(min data 4) (List.cons key) acc
      | (x, _), Any
       |(x, Meals), Meals
       |(x, Elixirs), Elixirs
        when [%equal: Ingredient.Effect.Kind.t] x kind ->
        Fn.apply_n_times ~n:(min data 5) (List.cons key) acc
      | _ -> acc)

open Combinations
open Cooking

let time () = ref (Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64)

let diff_time r =
  let t0 = !r in
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
  r := t1;
  Int64.((t1 - t0) // 1_000_000_000L)

let combine ~max_hearts ~max_stamina list =
  let cache = Recipe.Table.create () in
  let f (((best_score, all_best) as best), i) (recipe : Recipe.t) =
    let score =
      Recipe.Table.find_or_add cache recipe ~default:(fun () ->
          match cook ~max_hearts ~max_stamina recipe with
          | Food meal
           |Elixir meal ->
            Meal.score ~max_hearts ~max_stamina meal
          | Dubious
           |Failed _ ->
            -1_000_000)
    in
    match score with
    | _ when score > best_score -> (score, [ recipe ]), i + 1
    | _ when score = best_score -> (best_score, recipe :: all_best), i + 1
    | _ -> best, i + 1
  in
  generate_all ~init:((0, []), 0) ~f 5 list

let rarity_score grouped recipe =
  Glossary.Map.fold recipe ~init:0.0 ~f:(fun ~key ~data acc ->
      let remaining = Glossary.Table.find grouped key |> Option.value_exn ~here:[%here] in
      let removing = data // remaining in
      Float.(removing + acc))

let break_tie grouped recipes =
  List.fold recipes ~init:(Float.max_value, Glossary.Map.empty)
    ~f:(fun ((smallest_impact, _) as smallest) recipe ->
      let impact = rarity_score grouped recipe in
      if Float.(impact < smallest_impact) then impact, recipe else smallest)

let top_most_common ?(n = 3) grouped recipes =
  let sorted =
    List.fold recipes ~init:Float.Map.empty ~f:(fun acc recipe ->
        Float.Map.add_multi acc ~key:(rarity_score grouped recipe) ~data:recipe)
  in
  Float.Map.to_sequence sorted ~order:`Increasing_key
  |> Sequence.concat_map ~f:(fun (x, ll) -> List.map ll ~f:(Tuple2.create x) |> Sequence.of_list)
  |> Fn.flip Sequence.take n
  |> Sequence.to_list

type iteration = {
  rarity: float;
  best: Recipe.t;
}
[@@deriving sexp]

type t = {
  iterations: iteration list;
  score: int;
  count: int;
  duration: float;
}
[@@deriving sexp]

let to_string ~max_hearts ~max_stamina { iterations; score; count; duration } =
  let buf = Buffer.create 128 in
  bprintf buf "(%ds)" (Float.to_int duration);
  List.iter iterations ~f:(fun { rarity; best } ->
      bprintf buf
        !"\n%d pts (%d, %f) -- %{Recipe} -- %{sexp: Cooking.t}"
        score count rarity best
        (cook ~max_hearts ~max_stamina best));
  Buffer.contents buf

let run ~max_hearts ~max_stamina ~kind ~category items =
  let r = time () in
  let grouped = group items in
  let output = filter ~kind ~category grouped |> combine ~max_hearts ~max_stamina in
  match output with
  | ((0 as score), _), count ->
    let duration = diff_time r in
    { iterations = []; score; count; duration }
  | (score, ties), count ->
    let iterations =
      List.dedup_and_sort ties ~compare:[%compare: Recipe.t]
      |> top_most_common grouped
      |> List.map ~f:(fun (rarity, best) -> { rarity; best })
    in
    let duration = diff_time r in
    { iterations; score; count; duration }
