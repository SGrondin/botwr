open! Core_kernel

(* A recipe is a collection of ingredients and how many times to use each *)
module Recipe = struct
  module Self = struct
    type t = int Glossary.Map.t [@@deriving sexp, compare, equal]

    let hash_fold_t init map =
      Glossary.Map.fold map ~init ~f:(fun ~key ~data acc ->
          [%hash_fold: int] ([%hash_fold: Glossary.t] acc key) data)

    let hash map = Hash.of_fold hash_fold_t map
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
