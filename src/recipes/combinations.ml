open! Core

(* A recipe is a collection of ingredients and how many times to use each *)
module Recipe = struct
  type t = int Glossary.Map.t [@@deriving sexp, compare]

  let to_string recipe =
    Glossary.Map.to_alist recipe
    |> List.map ~f:(function
         | k, 1 -> Glossary.Variants.to_name k
         | k, v -> sprintf !"%{Glossary.Variants.to_name} x%d" k v)
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:", "

  let to_string_many ll =
    List.map ll ~f:to_string
    |> List.sort ~compare:String.compare
    |> List.mapi ~f:(sprintf "%d. %s")
    |> String.concat ~sep:"\n"

  let hash map =
    Glossary.Map.fold map ~init:(Hash.create ()) ~f:(fun ~key ~data acc ->
        [%hash_fold: int] ([%hash_fold: Glossary.t] acc key) data)
    |> Hash.get_hash_value
end

module Basic = struct
  (* The output of the basic combinations is deduped *)
  module Table = Hashtbl.Make (struct
    type t = Recipe.t [@@deriving sexp, compare]

    let hash = Recipe.hash
  end)

  let rec sideloop dn t acc = function
  | x :: rest ->
    downloop dn t (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ)) rest;
    (sideloop [@tailcall]) dn t acc rest
  | _ -> ()

  and downloop dn t acc = function
  | _ when dn = 0 -> Table.set t ~key:acc ~data:()
  | items -> (sideloop [@tailcall]) (dn - 1) t acc items

  let generate ?(t = Table.create ()) r items =
    downloop r t Glossary.Map.empty items;
    t

  let generate_all r items =
    List.init r ~f:(( + ) 1)
    |> List.fold ~init:(Table.create ()) ~f:(fun acc r -> generate ~t:acc r items)
end

module Advanced = struct
  module Recipes = struct
    module Map = Int.Map

    type book = {
      recipe: Recipe.t;
      num: int;
    }
    [@@deriving sexp]

    type t = book Map.t

    let hash map =
      Map.fold map ~init:(Hash.create ()) ~f:(fun ~key ~data acc ->
          [%hash_fold: int] ([%hash_fold: int] acc key) data.num)
      |> Hash.get_hash_value

    let to_string map =
      Map.data map
      |> List.map ~f:(fun { recipe; num } -> sprintf "-- %dx -- %s" num (Recipe.to_string recipe))
      |> String.concat ~sep:"\n"
  end

  let rec sideloop r dn t ~f all (has_effect, map) = function
  | x :: rest ->
    let has_effect = Glossary.has_effect x || has_effect in
    downloop r dn t ~f all
      (has_effect, Glossary.Map.update map x ~f:(Option.value_map ~default:1 ~f:succ))
      rest;
    (sideloop [@tailcall]) r dn t ~f all (has_effect, map) rest
  | _ -> ()

  and downloop r dn t ~f all ((has_effect, map) as acc) = function
  | items when dn = 0 && has_effect ->
    let recipes =
      Recipes.Map.update all (Recipe.hash map) ~f:(function
        | None -> Recipes.{ recipe = map; num = 1 }
        | Some x -> Recipes.{ x with num = x.num + 1 })
    in
    (toploop [@tailcall]) r r t ~f recipes items
  | items -> (sideloop [@tailcall]) r (dn - 1) t ~f all acc items

  and toploop r tn t ~f all = function
  | [] -> t := f !t all
  | items when tn > 0 ->
    downloop r tn t ~f all (false, Glossary.Map.empty) items;
    (toploop [@tailcall]) r (tn - 1) t ~f all items
  | _ -> ()

  let generate ~init ~f r items =
    let t = ref init in
    toploop r r t ~f Recipes.Map.empty items;
    !t
end
