open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let filter ~kind ~(category : Glossary.Category.t) grouped =
  let open Glossary in
  let neutrals = Queue.create () in
  let monsters = Queue.create () in
  let all =
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
         |Neutral, Spice, Any ->
          Fn.apply_n_times ~n:(min data 4)
            (fun () -> Queue.enqueue neutrals (Variants.to_rank key, key))
            ();
          acc
        | _, Monster, Elixirs
         |_, Monster, Any ->
          Fn.apply_n_times ~n:(min data 4)
            (fun () -> Queue.enqueue monsters (Variants.to_rank key, key))
            ();
          acc
        | x, _, Any
         |x, Food, Meals
         |x, Spice, Meals
         |x, Critter, Elixirs
         |x, Elixir, Elixirs
          when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 5) (List.cons key) acc
        | _ -> acc)
  in
  let top queue init =
    let arr = Queue.to_array queue in
    let compare =
      match kind with
      | Nothing
       |Neutral
       |Enduring
       |Energizing
       |Hearty ->
        (fun (x, _) (y, _) -> [%compare: int] x y)
      | Chilly
       |Electro
       |Fireproof
       |Hasty
       |Mighty
       |Sneaky
       |Spicy
       |Tough ->
        (fun (x, _) (y, _) -> [%compare: int] y x)
    in
    Array.sort arr ~compare;
    Array.slice arr 0 (min 4 (Array.length arr)) |> Array.fold_right ~init ~f:(fun (_, x) acc -> x :: acc)
  in
  all |> top neutrals |> top monsters

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

let combine ~max_hearts ~max_stamina list =
  let cache = Recipe.Table.create () in
  let f (({ first = score1, ll1; second = score2, ll2; third = score3, ll3 } as acc), i)
     (recipe : Recipe.t) =
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

let to_string ~max_hearts ~max_stamina { iterations; count; duration } =
  let buf = Buffer.create 128 in
  bprintf buf "(%ds)" (Float.to_int duration);
  List.iter iterations ~f:(fun { rarity; score; recipe } ->
      bprintf buf
        !"\n%d pts (%d, %f) -- %{Recipe} -- %{sexp: Cooking.t}"
        score count rarity recipe
        (cook ~max_hearts ~max_stamina recipe));
  Buffer.contents buf

let top_sort grouped ll =
  let rec loop from (ll, count) =
    match from with
    | [] -> List.rev ll |> List.concat
    | (score, recipes) :: rest ->
      let next =
        List.dedup_and_sort recipes ~compare:[%compare: Recipe.t]
        |> top_most_common ~n:(3 - count) grouped
        |> List.map ~f:(fun (rarity, recipe) -> { rarity; score; recipe })
      in
      let len = count + List.length next in
      if len >= 3 then List.rev (next :: ll) |> List.concat else loop rest (next :: ll, len)
  in
  loop ll ([], 0)

let run ~max_hearts ~max_stamina ~kind ~category items =
  let r = time () in
  let grouped = group items in
  let { first; second; third }, count =
    filter ~kind ~category grouped |> combine ~max_hearts ~max_stamina
  in
  let iterations = top_sort grouped [ first; second; third ] in
  let duration = diff_time r in
  { iterations; count; duration }
