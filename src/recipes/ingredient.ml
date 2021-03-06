open! Core_kernel

module Duration = struct
  type t =
    | Always      of int
    | Diminishing of {
        first: int;
        next: int;
      }
  [@@deriving sexp, compare, equal, hash]

  let merge ~count = function
  | Always x -> Always (x * count)
  | Diminishing { first; next } -> Always (first + (next * (count - 1)))

  let combine left right =
    match left, right with
    | Always x, Always y -> Always (x + y)
    | Always x, Diminishing { first; _ } -> Always (x + first)
    | Diminishing { first; _ }, Always y -> Always (first + y)
    | Diminishing { first = x; _ }, Diminishing { first = y; _ } -> Always (x + y)
end

module Hearts = struct
  type quarters = Quarters of int [@@deriving sexp, compare, equal, hash] [@@unboxed]

  type t =
    | Always      of quarters
    | Diminishing of {
        first: quarters;
        next: quarters;
      }
  [@@deriving sexp, compare, equal, hash]

  let merge ~count = function
  | Always (Quarters x) -> Always (Quarters (x * count))
  | Diminishing { first = Quarters first; next = Quarters next } ->
    Always (Quarters (first + (next * (count - 1))))

  let combine left right =
    match left, right with
    | Always (Quarters x), Always (Quarters y) -> Always (Quarters (x + y))
    | Always (Quarters x), Diminishing { first = Quarters first; _ } -> Always (Quarters (x + first))
    | Diminishing { first = Quarters first; _ }, Always (Quarters y) -> Always (Quarters (first + y))
    | Diminishing { first = Quarters x; _ }, Diminishing { first = Quarters y; _ } ->
      Always (Quarters (x + y))
end

module Effect = struct
  module Activity = struct
    type t = {
      duration: Duration.t;
      points: int;
    }
    [@@deriving sexp, compare, equal, hash]

    let merge ~count { duration; points } =
      { duration = Duration.merge ~count duration; points = points * count }

    let combine left right =
      { duration = Duration.combine left.duration right.duration; points = left.points + right.points }
  end

  module Quarters = struct
    type t = Quarters of int [@@deriving sexp, compare, equal, hash] [@@unboxed]

    let merge ~count (Quarters x) = Quarters (x * count)

    let combine (Quarters x) (Quarters y) = Quarters (x + y)
  end

  module Fifths = struct
    type t = Fifths of int [@@deriving sexp, compare, equal, hash] [@@unboxed]

    let merge ~count (Fifths x) = Fifths (x * count)

    let combine (Fifths x) (Fifths y) = Fifths (x + y)
  end

  type t =
    | Nothing
    | Neutral    of Duration.t
    | Hearty     of int
    | Energizing of Fifths.t
    | Enduring   of Quarters.t
    | Spicy      of Activity.t
    | Chilly     of Activity.t
    | Electro    of Activity.t
    | Fireproof  of Activity.t
    | Hasty      of Activity.t
    | Sneaky     of Activity.t
    | Mighty     of Activity.t
    | Tough      of Activity.t
  [@@deriving sexp, compare, equal, hash, variants]

  let merge ~count = function
  | Nothing -> Nothing
  | Neutral x -> Neutral (Duration.merge ~count x)
  | Hearty x -> Hearty (x * count)
  | Energizing x -> Energizing (Fifths.merge ~count x)
  | Enduring x -> Enduring (Quarters.merge ~count x)
  | Spicy x -> Spicy (Activity.merge ~count x)
  | Chilly x -> Chilly (Activity.merge ~count x)
  | Electro x -> Electro (Activity.merge ~count x)
  | Fireproof x -> Fireproof (Activity.merge ~count x)
  | Hasty x -> Hasty (Activity.merge ~count x)
  | Sneaky x -> Sneaky (Activity.merge ~count x)
  | Mighty x -> Mighty (Activity.merge ~count x)
  | Tough x -> Tough (Activity.merge ~count x)

  let combine left right =
    match left, right with
    | Nothing, _
     |_, Nothing ->
      Nothing
    | Neutral x, Neutral y -> Neutral (Duration.combine x y)
    | Hearty x, Hearty y -> Hearty (x + y)
    | Energizing x, Energizing y -> Energizing (Fifths.combine x y)
    | Enduring x, Enduring y -> Enduring (Quarters.combine x y)
    | Spicy x, Spicy y -> Spicy (Activity.combine x y)
    | Chilly x, Chilly y -> Chilly (Activity.combine x y)
    | Electro x, Electro y -> Electro (Activity.combine x y)
    | Fireproof x, Fireproof y -> Fireproof (Activity.combine x y)
    | Hasty x, Hasty y -> Hasty (Activity.combine x y)
    | Sneaky x, Sneaky y -> Sneaky (Activity.combine x y)
    | Mighty x, Mighty y -> Mighty (Activity.combine x y)
    | Tough x, Tough y -> Tough (Activity.combine x y)
    | Neutral dur, x
     |x, Neutral dur -> (
      match x with
      | Nothing -> Nothing
      | Neutral x -> Neutral (Duration.combine dur x)
      | (Hearty _ as x)
       |(Energizing _ as x)
       |(Enduring _ as x) ->
        x
      | Spicy x -> Spicy { x with duration = Duration.combine dur x.duration }
      | Chilly x -> Chilly { x with duration = Duration.combine dur x.duration }
      | Electro x -> Electro { x with duration = Duration.combine dur x.duration }
      | Fireproof x -> Fireproof { x with duration = Duration.combine dur x.duration }
      | Hasty x -> Hasty { x with duration = Duration.combine dur x.duration }
      | Sneaky x -> Sneaky { x with duration = Duration.combine dur x.duration }
      | Mighty x -> Mighty { x with duration = Duration.combine dur x.duration }
      | Tough x -> Tough { x with duration = Duration.combine dur x.duration }
    )
    | _ -> Nothing

  module Kind = struct
    module Self = struct
      type t =
        | Nothing
        | Neutral
        | Chilly
        | Electro
        | Enduring
        | Energizing
        | Fireproof
        | Hasty
        | Hearty
        | Mighty
        | Sneaky
        | Spicy
        | Tough
      [@@deriving sexp, compare, equal, hash]
    end

    module Map = Map.Make (Self)
    include Self
  end
end

module Category = struct
  type t =
    | Food
    | Spice
    | Critter
    | Monster
    | Elixir
    | With_fairy of t
    | Dragon
    | Dubious
  [@@deriving sexp, compare, equal, hash]

  let rec combine left right =
    match left, right with
    | x, Dragon
     |Dragon, x ->
      x
    | With_fairy x, With_fairy y
     |With_fairy x, y
     |y, With_fairy x ->
      With_fairy (combine x y)
    | Food, Food -> Food
    | Spice, Spice -> Spice
    | Food, Spice
     |Spice, Food ->
      Food
    | Food, _
     |_, Food
     |Spice, _
     |_, Spice
     |Dubious, _
     |_, Dubious ->
      Dubious
    | Monster, Monster -> Monster
    | Critter, Critter -> Critter
    | Elixir, Elixir
     |Critter, Monster
     |Monster, Critter
     |Elixir, Critter
     |Critter, Elixir
     |Elixir, Monster
     |Monster, Elixir ->
      Elixir
end

type t = {
  hearts: Hearts.t;
  effect: Effect.t;
  category: Category.t;
  critical: bool;
}
[@@deriving sexp, compare, equal, hash]

let to_kind : t -> Effect.Kind.t = function
| { effect = Nothing; _ } -> Nothing
| { effect = Neutral _; _ } -> Neutral
| { effect = Hearty _; _ } -> Hearty
| { effect = Energizing _; _ } -> Energizing
| { effect = Enduring _; _ } -> Enduring
| { effect = Spicy _; _ } -> Spicy
| { effect = Chilly _; _ } -> Chilly
| { effect = Electro _; _ } -> Electro
| { effect = Fireproof _; _ } -> Fireproof
| { effect = Hasty _; _ } -> Hasty
| { effect = Sneaky _; _ } -> Sneaky
| { effect = Mighty _; _ } -> Mighty
| { effect = Tough _; _ } -> Tough

let has_effect_or_special : t -> bool = function
| { category = Dragon; _ } -> true
| { effect = Nothing; _ }
 |{ effect = Neutral _; _ } ->
  false
| { effect = Hearty _; _ }
 |{ effect = Energizing _; _ }
 |{ effect = Enduring _; _ }
 |{ effect = Spicy _; _ }
 |{ effect = Chilly _; _ }
 |{ effect = Electro _; _ }
 |{ effect = Fireproof _; _ }
 |{ effect = Hasty _; _ }
 |{ effect = Sneaky _; _ }
 |{ effect = Mighty _; _ }
 |{ effect = Tough _; _ } ->
  true

let merge ({ hearts; effect; category; critical } as ingredient) ~count =
  match count with
  | 1 -> ingredient
  | _ -> { hearts = Hearts.merge ~count hearts; effect = Effect.merge ~count effect; category; critical }

let combine left right =
  {
    hearts = Hearts.combine left.hearts right.hearts;
    effect = Effect.combine left.effect right.effect;
    category = Category.combine left.category right.category;
    critical = left.critical || right.critical;
  }
