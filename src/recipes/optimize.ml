open! Core_kernel

let group items =
  let table = Glossary.Table.create () in
  List.iter items ~f:(fun (x, n) ->
      Glossary.Table.update table x ~f:(Option.value_map ~default:n ~f:(( + ) n)));
  table

let filter ~kind grouped =
  Glossary.Table.fold grouped ~init:[] ~f:(fun ~key ~data acc ->
      match key |> Glossary.to_ingredient |> Ingredient.to_kind with
      | Neutral -> Fn.apply_n_times ~n:(min data 4) (List.cons key) acc
      | x when [%equal: Ingredient.Effect.Kind.t] x kind ->
        Fn.apply_n_times ~n:(min data 5) (List.cons key) acc
      | _ -> acc)

open Combinations
open Cooking

let time () = ref (Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64)

let diff_time r =
  let t0 = !r in
  let t1 = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
  r := t1;
  Int64.((t1 - t0) / 1_000L)

module Basic = struct
  let combine ~max_hearts ~max_stamina ~factor list =
    let cache = Recipe.Table.create () in
    let f (((best_score, all_best) as best), i) (recipe : Recipe.t) =
      let score =
        Recipe.Table.find_or_add cache recipe ~default:(fun () ->
            match cook recipe with
            | Food meal
             |Elixir meal ->
              Meal.score ~max_hearts ~max_stamina ~factor meal
            | Dubious
             |Failed _ ->
              -1_000_000)
      in
      match score with
      | _ when score > best_score -> (score, [ recipe ]), i + 1
      | _ when score = best_score -> (best_score, recipe :: all_best), i + 1
      | _ -> best, i + 1
    in
    Basic.generate_all ~init:((0, []), 0) ~f 5 list

  let break_tie grouped recipes =
    let score recipe =
      Glossary.Map.fold recipe ~init:0.0 ~f:(fun ~key ~data acc ->
          let remaining = Glossary.Table.find grouped key |> Option.value_exn ~here:[%here] in
          let removing = data // remaining in
          Float.(removing + acc))
    in
    List.fold recipes ~init:(Float.max_value, Glossary.Map.empty)
      ~f:(fun ((smallest_impact, _) as smallest) recipe ->
        let impact = score recipe in
        if Float.(impact < smallest_impact) then impact, recipe else smallest)
    |> snd

  type iteration = {
    score: int;
    count: int;
    best: Recipe.t;
  }
  [@@deriving sexp]

  type t = {
    iterations: iteration list;
    duration: Int64.t;
  }
  [@@deriving sexp]

  let to_string { iterations; duration } =
    let buf = Buffer.create 128 in
    bprintf buf "(%Lds)" Int64.(duration / 1_000_000L);
    List.iter iterations ~f:(fun { score; count; best } ->
        bprintf buf !"\n%d pts (%d) -- %{Recipe} -- %{sexp: Cooking.t}" score count best (cook best));
    Buffer.contents buf

  let run ~max_hearts ~max_stamina ~factor ~kind items =
    let rec loop remaining acc =
      let (score, ties), count = filter ~kind remaining |> combine ~max_hearts ~max_stamina ~factor in
      let best = break_tie remaining ties in
      Glossary.Map.iteri best ~f:(fun ~key ~data ->
          Glossary.Table.decr remaining key ~by:data ~remove_if_zero:true);
      if score > 0 then (loop [@tailcall]) remaining ({ score; count; best } :: acc) else acc
    in
    let r = time () in
    let iterations = loop (group items) [] |> List.rev in
    let duration = diff_time r in
    { iterations; duration }
end

module Advanced = struct
  let combine ~max_hearts ~max_stamina ~factor list =
    let cache = Int.Table.create () in
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
                    -1_000_000)
            in
            (score * num) + acc)
      in
      if score > best_score then (score, recipes), i + 1 else best, i + 1
    in
    Advanced.generate ~init:((0, Advanced.Recipes.Map.empty), 0) ~f 5 list

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

  let run ~max_hearts ~max_stamina ~factor ~kind items =
    let r = time () in
    let (score, best), count = group items |> filter ~kind |> combine ~max_hearts ~max_stamina ~factor in
    let duration = diff_time r in
    { score; count; best; duration }
end
