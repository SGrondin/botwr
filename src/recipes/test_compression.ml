open! Core_kernel
open Compression

let data1 = Items.[ Dazzlefruit, 3; Star_fragment, 1; Dragon_horns Farosh, 1 ]

let data2 = Items.[ Hylian_shroom, 1200; Cane_sugar, 1001 ]

let%expect_test "Merge" =
  let test x = x |> merge |> sprintf !"%{sexp: data list}" |> print_endline in
  test data1;
  [%expect {| ((Skip 1) (Item 3) (Skip 103) (Item 1) (Skip 49) (Item 1)) |}];
  test data2;
  [%expect {| ((Skip 5) (Item 1200) (Skip 19) (Item 1001)) |}]

let%expect_test "Compress" =
  let test x = x |> compress |> print_endline in
  test data1;
  [%expect {| AYNngTGB |}];
  test data2;
  [%expect {| BcSwE8Pp |}]

let%expect_test "Decompress" =
  let test x = x |> decompress |> sprintf !"%{sexp: int Map.t Or_error.t}" |> print_endline in
  test "AYNngTGB";
  [%expect {| (Ok ((Dazzlefruit 3) (Star_fragment 1) ((Dragon_horns Farosh) 1))) |}];
  test "BcSwE8Pp";
  [%expect {| (Ok ((Hylian_shroom 999) (Cane_sugar 999))) |}]
