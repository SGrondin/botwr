open! Core_kernel

type t =
  | BOTW
  | TOTK
[@@deriving sexp, compare, equal, hash, enumerate]

let to_string = function
| BOTW -> "Breath of the Wild"
| TOTK -> "Tears of the Kingdom"

let max_hearts = function
| BOTW -> 30
| TOTK -> 40

type availability =
  | BOTW
  | TOTK
  | Both

let is_in_game (availability : availability) ~(game : t) =
  match availability, game with
  | Both, _
   |BOTW, BOTW
   |TOTK, TOTK ->
    true
  | BOTW, TOTK
   |TOTK, BOTW ->
    false
