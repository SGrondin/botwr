open! Core_kernel

(* A recipe is a collection of ingredients and how many times to use each *)
module Recipe = struct
  module Self = struct
    type t = int Glossary.Map.t [@@deriving sexp, compare, equal]

    let hash_fold_t init map =
      Glossary.Map.fold map ~init ~f:(fun ~key ~data acc ->
          [%hash_fold: int] ([%hash_fold: Glossary.t] acc key) data)

    let hash map = hash_fold_t (Hash.create ()) map |> Hash.get_hash_value
  end

  module Table = Hashtbl.Make (Self)
  module Set = Set.Make (Self)
  include Self

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
end

module Basic = struct
  let rec sideloop dn t ~f acc = function
  | x :: rest ->
    downloop dn t ~f (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ)) rest;
    (sideloop [@tailcall]) dn t ~f acc rest
  | _ -> ()

  and downloop dn t ~f acc = function
  | _ when dn = 0 -> t := f !t acc
  | items -> (sideloop [@tailcall]) (dn - 1) t ~f acc items

  let generate ~init ~f r items =
    let t = ref init in
    downloop r t ~f Glossary.Map.empty items;
    !t

  let generate_all ~init ~f r items =
    List.init r ~f:(( + ) 1) |> List.fold ~init ~f:(fun acc r -> generate ~init:acc ~f r items)
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
