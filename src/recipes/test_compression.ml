open! Core_kernel
open Compression

let data1 = Items.[ Hylian_shroom, 3; Cane_sugar, 1 ]

let data2 = Items.[ Hylian_shroom, 1200; Cane_sugar, 1001 ]

let%expect_test "Merge" =
  let test x = x |> merge |> sprintf !"%{sexp: data list}" |> print_endline in
  test data1;
  [%expect {| ((Skip 2) (Item 3) (Skip 15) (Item 1)) |}];
  test data2;
  [%expect {| ((Skip 2) (Item 1200) (Skip 15) (Item 1001)) |}]

let%expect_test "Compress" =
  let test x = x |> compress |> print_endline in
  test data1;
  [%expect {| AoMPgQ |}];
  test data2;
  [%expect {| AsSwD8Pp |}]

let%expect_test "Decompress" =
  let test x = x |> decompress |> sprintf !"%{sexp: int Map.t Or_error.t}" |> print_endline in
  test "AoMPgQ";
  [%expect {| (Ok ((Hylian_shroom 3) (Cane_sugar 1))) |}];
  test "AsSwD8Pp";
  [%expect {| (Ok ((Hylian_shroom 999) (Cane_sugar 999))) |}]
