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

    type t = book Map.t

    let hash map =
      Map.fold map ~init:(Hash.create ()) ~f:(fun ~key ~data acc ->
          [%hash_fold: int] ([%hash_fold: int] acc key) data.num)
      |> Hash.get_hash_value
  end

  module Every = struct
    module Table = Int.Table
  end

  let rec sideloop r dn t all acc = function
  | x :: rest ->
    downloop r dn t all (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ)) rest;
    (sideloop [@tailcall]) r dn t all acc rest
  | _ -> ()

  and downloop r dn t all acc = function
  | items when dn = 0 ->
    let recipes =
      Recipes.Map.update all (Recipe.hash acc) ~f:(function
        | None -> Recipes.{ recipe = acc; num = 1 }
        | Some x -> Recipes.{ x with num = x.num + 1 })
    in
    (toploop [@tailcall]) r r t recipes items
  | items -> (sideloop [@tailcall]) r (dn - 1) t all acc items

  and toploop r tn t all = function
  | [] -> Every.Table.set t ~key:(Recipes.hash all) ~data:all
  | items when tn > 0 ->
    downloop r tn t all Glossary.Map.empty items;
    (toploop [@tailcall]) r (tn - 1) t all items
  | _ -> ()

  let generate r items =
    let t = Every.Table.create ~size:10_000 () in
    toploop r r t Recipes.Map.empty items;
    t
end
