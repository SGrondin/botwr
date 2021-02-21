open! Core

type recipe = int Glossary.Map.t [@@deriving sexp, compare]

let string_of_recipe recipe =
  Glossary.Map.to_alist recipe
  |> List.map ~f:(function
       | k, 1 -> Glossary.Variants.to_name k
       | k, v -> sprintf !"%{Glossary.Variants.to_name} x%d" k v)
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:", "

let string_of_recipe_list ll =
  List.map ll ~f:string_of_recipe
  |> List.sort ~compare:String.compare
  |> List.mapi ~f:(sprintf "%d. %s")
  |> String.concat ~sep:"\n"

module Basic = struct
  module Table = Hashtbl.Make (struct
    type t = recipe [@@deriving sexp, compare]

    let hash map =
      Glossary.Map.fold map ~init:(Hash.create ()) ~f:(fun ~key ~data acc ->
          [%hash_fold: int] ([%hash_fold: Glossary.t] acc key) data)
      |> Hash.get_hash_value
  end)

  let rec sideloop dn t acc = function
  | x :: rest ->
    downloop dn t (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ)) rest;
    (sideloop [@tailcall]) dn t acc rest
  | _ -> ()

  and downloop dn t acc = function
  | _ when dn = 0 -> Table.set t ~key:acc ~data:()
  | items -> (sideloop [@tailcall]) (dn - 1) t acc items

  let generate r items =
    let t = Table.create ~size:((r + 1) / 2) () in
    downloop r t Glossary.Map.empty items;
    t
end

module Advanced = struct
  module Recipes = Map.Make (struct
    type t = int Glossary.Map.t [@@deriving sexp, compare]
  end)

  module Every = Map.Make (struct
    type t = int Recipes.t [@@deriving sexp, compare]
  end)

  let rec sideloop r dn t all acc = function
  | x :: rest ->
    downloop r dn t all (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ)) rest;
    (sideloop [@tailcall]) r dn t all acc rest
  | _ -> ()

  and downloop r dn t all acc = function
  | items when dn = 0 ->
    let recipes = Recipes.update all acc ~f:(Option.value_map ~default:1 ~f:succ) in
    toploop r r t recipes items
  | items -> (sideloop [@tailcall]) r (dn - 1) t all acc items

  and toploop r tn t all = function
  | [] -> (
    match Every.add !t ~key:all ~data:() with
    | `Ok x -> t := x
    | `Duplicate ->
      (* print_endline (sprintf !"!!!!!!! %{sexp: int Recipes.t}" all) *)
      ()
  )
  | items when tn > 2 ->
    downloop r tn t all Glossary.Map.empty items;
    (toploop [@tailcall]) r (tn - 1) t all items
  | _ -> ()

  let generate r items =
    let t = ref Every.empty in
    toploop r r t Recipes.empty items;
    !t
end
