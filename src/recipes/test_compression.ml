open! Core_kernel
open Compression

let data1 = Items.[ Dazzlefruit, 3; Dragon_horns Farosh, 1 ]

let data2 = Items.[ Wildberry, 1; Hylian_shroom, 1200; Cane_sugar, 1001 ]

let%expect_test "Merge" =
  let test x = x |> merge |> sprintf !"%{sexp: data list}" |> print_endline in
  test data1;
  [%expect {| ((Skip 1) (Item 3) (Skip 184) (Item 1)) |}];
  test data2;
  [%expect {| ((Skip 4) (Item 1) (Item 1200) (Skip 19) (Item 1001)) |}]

let%expect_test "Compress" =
  let test x = x |> compress ~max_hearts:15 ~max_stamina:10 |> print_endline in
  test data1;
  [%expect {| AAIPCgGDfzmB |}];
  test data2;
  [%expect {| AAIPCgSBxLATw+k |}]

let%expect_test "Decompress" =
  let test x = x |> decompress |> sprintf !"%{sexp: t Or_error.t}" |> print_endline in
  test "AAIPCgGDfzmB";
  [%expect
    {|
    (Ok
     ((items ((Dazzlefruit 3) ((Dragon_horns Farosh) 1))) (max_hearts (15))
      (max_stamina (10)))) |}];
  test "AAIPCgSBxLATw+k";
  [%expect
    {|
    (Ok
     ((items ((Wildberry 1) (Hylian_shroom 999) (Cane_sugar 999)))
      (max_hearts (15)) (max_stamina (10)))) |}]
