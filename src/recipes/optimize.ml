open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let best (x : Ingredient.t) (y : Ingredient.t) =
  [%compare: int] (Glossary.Variants.to_rank y.item) (Glossary.Variants.to_rank x.item)

let worst (x : Ingredient.t) (y : Ingredient.t) =
  [%compare: int] (Glossary.Variants.to_rank x.item) (Glossary.Variants.to_rank y.item)

let lowest_fused (x : Ingredient.t) (y : Ingredient.t) =
  match [%compare: int] x.fused y.fused with
  | 0 -> Ingredient.compare_item x y
  | x -> x

let get_sort_by_kind : Ingredient.Effect.Kind.t -> Ingredient.t -> Ingredient.t -> int = function
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

let filter ~game ~(kind : Ingredient.Effect.Kind.t) ~(category : Glossary.Category.t) ~use_special grouped
   : Ingredient.t list list =
  let open Glossary in
  let limit_neutrals, limit_plenty =
    match kind with
    | Neutral -> 5, 7
    | _ -> 4, 4
  in
  let neutrals = Queue.create () in
  let neutrals_wasteful = Queue.create () in
  let hearts = Int.Table.create () in
  let hearts_wasteful = Int.Table.create () in
  let monster_horns = Queue.create () in
  let monster_fangs = Queue.create () in
  let monster_guts = Queue.create () in
  let monster_all = Queue.create () in
  let dragon_scales = Queue.create () in
  let dragon_claws = Queue.create () in
  let dragon_fangs = Queue.create () in
  let dragon_horns = Queue.create () in
  let dragons_wasteful = Queue.create () in
  let star_fragments = Queue.create () in
  let add_to ~n q key = Fn.apply_n_times ~n (fun () -> Queue.enqueue q key) () in
  let add_to_table ~n table ~bucket key =
    Fn.apply_n_times ~n (fun () -> Int.Table.add_multi table ~key:bucket ~data:key) ()
  in
  let basics =
    Table.fold grouped ~init:[] ~f:(fun ~key ~data -> function
      | acc when data = 0 -> acc
      | acc when not (Game.is_in_game (availability key) ~game) -> acc
      | acc -> (
        let ingredient = to_ingredient key in
        match Ingredient.to_kind ingredient, ingredient.category, category, kind with
        | (Nothing | Neutral), Food, (Meals | Any), (Sunny | Neutral) ->
          (match ingredient, Ingredient.Hearts.base ingredient.hearts with
          | _, 0 -> ()
          | { effect = Neutral (Always _); _ }, q ->
            add_to_table ~n:(min data limit_neutrals) hearts ~bucket:q ingredient
          | { effect = Neutral (Diminishing _); _ }, q ->
            add_to_table ~n:(min data limit_neutrals) hearts_wasteful ~bucket:q ingredient
          | _ -> ());
          acc
        | (Nothing | Neutral), Spice, (Meals | Any), (Sunny | Neutral)
         |_, (Food | Spice), (Meals | Any), Neutral
         |_, (Critter | Monster), (Elixirs | Any), Neutral ->
          (match Ingredient.Hearts.base ingredient.hearts with
          | 0 -> ()
          | q -> add_to_table ~n:(min data limit_neutrals) hearts_wasteful ~bucket:q ingredient);
          acc
        | (Nothing | Neutral), (Food | Spice), (Meals | Any), kind
          when Ingredient.Effect.Kind.has_duration kind ->
          (match ingredient with
          | { effect = Neutral (Diminishing _); _ } ->
            (* Add up to 1 to neutrals, up to 3 to neutrals_wasteful *)
            add_to ~n:1 neutrals ingredient;
            add_to ~n:(min (limit_neutrals - 1) (data - 1)) neutrals_wasteful ingredient;
            ()
          | _ -> add_to ~n:(min data limit_neutrals) neutrals ingredient);
          acc
        | _, Dragon, _, _ when use_special && [%equal: t] key Star_fragment ->
          add_to ~n:data star_fragments ingredient;
          acc
        | _, Dragon, _, _ when use_special ->
          (match key with
          | Dragon_scales _ -> add_to ~n:1 dragon_scales ingredient
          | Dragon_claws _ -> add_to ~n:1 dragon_claws ingredient
          | Dragon_fangs _ -> add_to ~n:1 dragon_fangs ingredient
          | Dragon_horns _ -> add_to ~n:1 dragon_horns ingredient
          | k -> failwithf !"Invalid dragon part '%{Glossary}' at %{Source_code_position}" k [%here] ());
          add_to ~n:(data - 1) dragons_wasteful ingredient;
          acc
        | _, Monster, (Elixirs | Any), _ ->
          add_to ~n:(min data 4) monster_all ingredient;
          (match key with
          | Monster_horn _ -> add_to ~n:(min data 4) monster_horns ingredient
          | Monster_fang _ -> add_to ~n:(min data 4) monster_fangs ingredient
          | Monster_guts _ -> add_to ~n:(min data 4) monster_guts ingredient
          | k -> failwithf !"Invalid monster part '%{Glossary}' at %{Source_code_position}" k [%here] ());
          acc
        | x, Food, (Meals | Any), kind when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 5) (List.cons ingredient) acc
        | x, Critter, (Elixirs | Any), kind when [%equal: Ingredient.Effect.Kind.t] x kind ->
          Fn.apply_n_times ~n:(min data 4) (List.cons ingredient) acc
        | _, With_fairy _, _, _ when use_special ->
          Fn.apply_n_times ~n:(min data 4) (List.cons ingredient) acc
        | _ -> acc))
  in
  let top ?(up_to = limit_neutrals) ?compare ?move queue init =
    match max 0 up_to with
    | up_to when up_to <= 0 -> init
    | up_to ->
      Option.iter move ~f:(fun dst ->
          Queue.blit_transfer ~src:queue ~dst ~len:(max 0 (Queue.length queue - up_to)) ());
      let compare = Option.value compare ~default:(get_sort_by_kind kind) in
      Queue.to_array queue |> Array.sorted_copy ~compare |> fun arr ->
      Array.slice arr 0 (min up_to (Array.length arr))
      |> Array.fold_right ~init ~f:(fun x acc -> x :: acc)
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

  let hearts =
    let plenty =
      Int.Table.fold hearts ~init:0 ~f:(fun ~key:_ ~data acc ->
          match data with
          | [] -> acc
          | [ _ ] -> acc + 1
          | _ :: _ :: _ -> acc + 2)
    in
    if plenty >= limit_plenty
    then hearts
    else
      Int.Table.merge hearts hearts_wasteful ~f:(fun ~key:_ -> function
        | `Left x
         |`Right x ->
          Some x
        | `Both (x, y) -> Some (List.concat_no_order [ x; y ]))
  in
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
  |> top monster_horns ~compare:lowest_fused
  |> top monster_fangs ~compare:lowest_fused
  |> top monster_guts ~compare:lowest_fused
  |> (fun init -> Int.Table.fold hearts ~init ~f:(fun ~key:_ ~data acc -> top (Queue.of_list data) acc))
  |> List.fold ~init:([], []) ~f:(fun (acc_meals, acc_elixirs) -> function
       | { category = Food | Spice; _ } as x -> x :: acc_meals, acc_elixirs
       | { category = Critter | Monster | Elixir; _ } as x -> acc_meals, x :: acc_elixirs
       | { category = With_fairy _ | Dragon; _ } as x -> x :: acc_meals, x :: acc_elixirs
       | { category = Dubious; _ } -> acc_meals, acc_elixirs)
  |> fun (acc_meals, acc_elixirs) ->
  (* Re-add one monster part to meals in case we can make a tonic  *)
  [ acc_meals |> top monster_all ~up_to:1 ~compare:lowest_fused; acc_elixirs ]

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

let combine ~max_hearts ~max_stamina ~gloomy_hearts ~algo ~kind (list : Ingredient.t list list) =
  let f ({ i; first = score1, ll1; second = score2, ll2; third = score3, ll3 } as acc) (recipe : Recipe.t)
      =
    match cook recipe with
    | Dubious
     |Failed _ ->
      { acc with i = i + 1 }
    | Food meal
     |Elixir meal
     |Tonic meal ->
      let score = Meal.score ~max_hearts ~max_stamina ~gloomy_hearts ~algo ~kind meal in
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
  List.fold list
    ~init:{ i = 0; first = 0, []; second = 0, []; third = 0, [] }
    ~f:(fun acc ll -> generate_all ~init:acc ~f 5 ll)

let rarity_score grouped recipe =
  Ingredient.Map.fold recipe ~init:0.0 ~f:(fun ~key ~data acc ->
      let remaining = Glossary.Table.find grouped key.item |> Option.value_exn ~here:[%here] in
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
  let r = time () in
  let grouped = group items in
  let { i = count; first; second; third } =
    filter ~game ~kind ~category ~use_special grouped
    |> combine ~max_hearts ~max_stamina ~gloomy_hearts ~algo ~kind
  in
  let iterations = top_sort grouped [ first; second; third ] in
  let duration = diff_time r in
  { iterations; count; duration }
