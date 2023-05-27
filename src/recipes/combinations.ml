open! Core_kernel

(* A recipe is a collection of ingredients and how many times to use each *)
module Recipe = struct
  module Self = struct
    type t = int Glossary.Map.t [@@deriving sexp, compare, equal]
  end

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

let rec sideloop dn t set ~f acc hash = function
| x :: rest ->
  downloop dn t set ~f
    (Glossary.Map.update acc x ~f:(Option.value_map ~default:1 ~f:succ))
    ([%hash_fold: Glossary.t] hash x)
    rest;
  (sideloop [@tailcall]) dn t set ~f acc hash rest
| _ -> ()

and downloop dn t set ~f acc hash = function
| _ when dn = 0 ->
  let k = Hash.get_hash_value hash in
  if Hash_set.mem set k
  then ()
  else (
    Hash_set.add set k;
    t := f !t acc)
| items -> (sideloop [@tailcall]) (dn - 1) t set ~f acc hash items

let generate ~init ~f r items =
  let t = ref init in
  downloop r t (Int.Hash_set.create ()) ~f Glossary.Map.empty (Hash.create ()) items;
  !t

let generate_all ~init ~f r items =
  List.init r ~f:(( + ) 1) |> List.fold ~init ~f:(fun acc r -> generate ~init:acc ~f r items)
