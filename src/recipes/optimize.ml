open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let best (x, _) (y, _) = [%compare: int] y x

let worst (x, _) (y, _) = [%compare: int] x y

let get_sort_by_kind : Ingredient.Effect.Kind.t -> int * 'a -> int * 'b -> int = function
| Nothing
 |Neutral
 |Enduring
 |Energizing
 |Hearty
 |Sunny ->
  worst
| Chilly
 |Electro
 |Fireproof
 |Hasty
 |Rapid
 |Sticky
 |Mighty
 |Sneaky
 |Spicy
 |Tough
 |Bright ->
  best

let filter ~game ~kind ~(category : Glossary.Category.t) ~use_special grouped =
  let open Glossary in
  let neutrals = Queue.create () in
  let neutrals_wasteful = Queue.create () in
  let monsters = Queue.create () in
  let dragon_scales = Queue.create () in
  let dragon_claws = Queue.create () in
  let dragon_fangs = Queue.create () in
  let dragon_horns = Queue.create () in
  let dragons_wasteful = Queue.create () in
  let star_fragments = Queue.create () in
  let add_to ~n q key = Fn.apply_n_times ~n (fun () -> Queue.enqueue q (Variants.to_rank key, key)) () in
  let basics =
    Table.fold grouped ~init:[] ~f:(fun ~key ~data -> function
      | acc when not (Game.is_in_game (availability key) ~game) -> acc
      | acc -> (
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
          (match ingredient with
          | { effect = Neutral (Diminishing _); _ } when data > 0 ->
            (* Add up to 1 to neutrals, up to 3 to neutrals_wasteful *)
            add_to ~n:1 neutrals key;
            add_to ~n:(min 3 (data - 1)) neutrals_wasteful key;
            ()
          | _ -> add_to ~n:(min data 4) neutrals key);
          acc
        | _, Dragon, _ when use_special && [%equal: t] key Star_fragment ->
          add_to ~n:data star_fragments key;
          acc
        | _, Dragon, _ when use_special && data > 0 ->
          (match key with
          | Dragon_scales _ -> add_to ~n:1 dragon_scales key
          | Dragon_claws _ -> add_to ~n:1 dragon_claws key
          | Dragon_fangs _ -> add_to ~n:1 dragon_fangs key
          | Dragon_horns _ -> add_to ~n:1 dragon_horns key
          | k -> failwithf !"Invalid dragon part '%{Glossary}' at %{Source_code_position}" k [%here] ());
          add_to ~n:(data - 1) dragons_wasteful key;
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
        | _ -> acc))
  in
  let top ?(up_to = 4) ?compare ?move queue init =
    match max 0 up_to with
    | up_to when up_to <= 0 -> init
    | up_to ->
      let compare = Option.value compare ~default:(get_sort_by_kind kind) in
      let rec loop ~into = function
        | 0 -> ()
        | len when len > up_to ->
          Queue.dequeue_exn queue |> Queue.enqueue into;
          loop ~into (len - 1)
        | _ -> ()
      in
      Option.iter move ~f:(fun into -> loop ~into (Queue.length queue));
      Queue.to_array queue |> Array.sorted_copy ~compare |> fun arr ->
      Array.slice arr 0 (min up_to (Array.length arr))
      |> Array.fold_right ~init ~f:(fun (_, x) acc -> x :: acc)
  in
  let horns_up_to = 1 in
  let num_horns = min horns_up_to (Queue.length dragon_horns) in

  let fangs_up_to = if Queue.is_empty dragon_horns then 3 else 2 in
  let num_fangs = min fangs_up_to (Queue.length dragon_fangs) in

  let claws_up_to = 4 - num_fangs in
  let num_claws = min claws_up_to (Queue.length dragon_claws) in

  let scales_up_to = 4 - (num_fangs + num_claws) in
  let num_scales = min scales_up_to (Queue.length dragon_scales) in

  let num_dragons = num_scales + num_claws + num_fangs + num_horns in

  basics
  |> top ~up_to:scales_up_to ~move:dragons_wasteful dragon_scales
  |> top ~up_to:claws_up_to ~move:dragons_wasteful dragon_claws
  |> top ~up_to:fangs_up_to ~move:dragons_wasteful dragon_fangs
  |> top ~up_to:horns_up_to ~move:dragons_wasteful dragon_horns
  |> top ~up_to:(if num_dragons = 0 then 1 else 0) ~move:dragons_wasteful star_fragments
  |> top neutrals
  |> top neutrals_wasteful ~up_to:(4 - Queue.length neutrals)
  |> top dragons_wasteful ~compare:worst
       ~up_to:(4 - Queue.(length neutrals + length neutrals_wasteful + num_dragons))
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
  i: int;
  first: int * Recipe.t list;
  second: int * Recipe.t list;
  third: int * Recipe.t list;
}

let combine ~max_hearts ~max_stamina ~gloomy_hearts ~algo list =
  let cache = Recipe.Table.create () in
  let f ({ i; first = score1, ll1; second = score2, ll2; third = score3, ll3 } as acc) (recipe : Recipe.t)
      =
    let score =
      Recipe.Table.find_or_add cache recipe ~default:(fun () ->
          match cook recipe with
          | Food meal
           |Elixir meal
           |Tonic meal ->
            Meal.score ~max_hearts ~max_stamina ~gloomy_hearts ~algo meal
          | Dubious
           |Failed _ ->
            -1_000_000)
    in
    if score < score3
    then { acc with i = i + 1 }
    else if score = score3
    then { acc with third = score, recipe :: ll3; i = i + 1 }
    else if score < score2
    then { acc with third = score, [ recipe ]; i = i + 1 }
    else if score = score2
    then { acc with second = score, recipe :: ll2; i = i + 1 }
    else if score < score1
    then { acc with second = score, [ recipe ]; third = acc.second; i = i + 1 }
    else if score = score1
    then { acc with first = score, recipe :: ll1; i = i + 1 }
    else { first = score, [ recipe ]; second = acc.first; third = acc.second; i = i + 1 }
  in
  generate_all ~init:{ i = 0; first = 0, []; second = 0, []; third = 0, [] } ~f 5 list

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
  game: Game.t;
  max_hearts: int;
  max_stamina: int;
  gloomy_hearts: int;
  algo: Algo.t;
  kind: Ingredient.Effect.Kind.t;
  category: Glossary.Category.t;
  use_special: bool;
}

let run { game; max_hearts; max_stamina; gloomy_hearts; algo; kind; category; use_special } items =
  let gloomy_hearts =
    match kind with
    | Sunny when gloomy_hearts >= max_hearts -> max_hearts - 1
    | Sunny -> gloomy_hearts
    | _ -> 0
  in
  let r = time () in
  let grouped = group items in
  let { i = count; first; second; third } =
    filter ~game ~kind ~category ~use_special grouped
    |> combine ~max_hearts ~max_stamina ~gloomy_hearts ~algo
  in
  let iterations = top_sort grouped [ first; second; third ] in
  let duration = diff_time r in
  { iterations; count; duration }
