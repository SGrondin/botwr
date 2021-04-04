open! Core_kernel
open! Bonsai_web
open! Vdom

type t =
  | Up
  | Down
  | Arrow_up
  | Arrow_down
  | Wait
  | Elixir
  | Meal
  | Heart1
  | Heart2
  | Heart3
  | Heart
  | Energizing
  | Energizing1
  | Energizing2
  | Energizing3
  | Energizing4
  | Enduring
  | Enduring1
  | Enduring2
  | Enduring3
  | Enduring4
  | Hearty
  | Chilly
  | Electro
  | Fireproof
  | Hasty
  | Sneaky
  | Spicy
  | Mighty
  | Tough
  | Check_all
  | Reception_0
  | Reception_1
  | Reception_2
  | Reception_3
  | Reception_4
  | X
[@@deriving sexp]

let of_kind : Recipes.Ingredient.Effect.Kind.t -> t = function
| Nothing -> Meal
| Neutral -> Meal
| Hearty -> Hearty
| Energizing -> Energizing
| Enduring -> Enduring
| Spicy -> Spicy
| Chilly -> Chilly
| Electro -> Electro
| Fireproof -> Fireproof
| Hasty -> Hasty
| Sneaky -> Sneaky
| Mighty -> Mighty
| Tough -> Tough

let get_fill = function
| Check_all
 |Up
 |Down
 |X ->
  "currentColor"
| Arrow_up
 |Arrow_down
 |Chilly
 |Reception_1 ->
  "#ff9902"
| Energizing1
 |Energizing2
 |Energizing3
 |Energizing4
 |Energizing
 |Reception_3
 |Reception_4 ->
  "#05e704"
| Heart1
 |Heart2
 |Heart3
 |Heart ->
  "#ec2434"
| Electro
 |Enduring
 |Enduring1
 |Enduring2
 |Enduring3
 |Enduring4
 |Hearty
 |Reception_2 ->
  "#e8e527"
| Fireproof
 |Reception_0 ->
  "#ec444a"
| Hasty -> "#108fff"
| Sneaky -> "#d03bfe"
| Spicy -> "#71d6f0"
| Elixir
 |Meal
 |Wait
 |Mighty
 |Tough ->
  "#949487"

let get_viewbox = function
| Up
 |Down
 |Arrow_up
 |Arrow_down
 |Wait
 |Elixir
 |Meal
 |Heart1
 |Heart2
 |Heart3
 |Heart
 |Energizing
 |Energizing1
 |Energizing2
 |Energizing3
 |Energizing4
 |Enduring
 |Enduring1
 |Enduring2
 |Enduring3
 |Enduring4
 |Hearty
 |Chilly
 |Electro
 |Fireproof
 |Hasty
 |Sneaky
 |Spicy
 |Mighty
 |Tough ->
  -10, 0, 1034, 1024
| Check_all
 |Reception_0
 |Reception_1
 |Reception_2
 |Reception_3
 |Reception_4
 |X ->
  0, 0, 16, 16

type container =
  | Div
  | Span

let svg ?(width = 1.0) ?(height = 1.0) ?(bold = false) ?fill ?(container = Div) ?(raw_extra_classes = [])
   icon attrs =
  let fill =
    Option.value fill ~default:(get_fill icon) |> fun fill -> Yojson.Basic.to_string (`String fill)
  in
  let v1, v2, v3, v4 = get_viewbox icon in
  let paths =
    match icon with
    | Up -> {svg|<path d="M802 728h-110l-180 -260l-180 260h-110l290 -430z" />|svg}
    | Down -> {svg|<path d="M222 298h110l180 260l180 -260h110l-290 430z" />|svg}
    | Arrow_up -> {svg|<path d="M558 720l210 -280l210 280h-140v170h-140v-170h-140z" />|svg}
    | Arrow_down -> {svg|<path d="M696 534h140v170h140l-210 280l-210 -280h140v-170z" />|svg}
    | Wait ->
      {svg|<path d="M242 760q0 -7 4.5 -11.5t11.5 -4.5h508q7 0 11.5 4.5t4.5 11.5v88q0 7 -4.5 11.5t-11.5 4.5h-508q-7 0 -11.5 -4.5t-4.5 -11.5v-88zM302 290h420q0 67 -25 106q-24 39 -54 60.5t-54 32.5q-25 11 -25 23t25 23q24 11 54 32.5t54 60.5q25 38 25 106h-84h-115h-121h-100 q0 -67 25 -106q24 -39 54 -60.5t54 -32.5q25 -11 25 -23t-25 -23q-24 -11 -54 -32.5t-54 -60.5q-25 -38 -25 -106zM242 176q0 -7 4.5 -11.5t11.5 -4.5h508q7 0 11.5 4.5t4.5 11.5v88q0 7 -4.5 11.5t-11.5 4.5h-508q-7 0 -11.5 -4.5t-4.5 -11.5v-88z" />|svg}
    | Elixir ->
      {svg|<path d="M356 810q-17 0 -28.5 -11.5t-11.5 -28.5v-278q0 -16 11.5 -28t28.5 -12h8q17 0 28.5 -11.5t11.5 -28.5v-22q0 -9 -5.5 -14.5t-14.5 -5.5h-12q-12 0 -21 -9t-9 -21v-18q0 -13 9 -21.5t21 -8.5h44v-80h196v80h40q12 0 21 8.5t9 21.5v18q0 12 -9 21t-21 9h-8q-9 0 -14.5 5.5 t-5.5 14.5v22q0 17 11.5 28.5t28.5 11.5h4q17 0 28.5 12t11.5 28v278q0 17 -11.5 28.5t-28.5 11.5h-312v0zM612 292h-195v38h195v-38z" />|svg}
    | Meal ->
      {svg|<path d="M512 800q111 0 171.5 -49.5t60.5 -114.5q0 -33 -14 -67q-14 -35 -36 -63.5t-48 -47.5q-26 -18 -50 -18q-48 0 -46.5 20t-37.5 20t-37.5 -20t-46.5 -20q-24 0 -50 18q-26 17 -48 45t-36 63t-14 70q0 70 60.5 117t171.5 47zM474 416q-2 -11 -1 -41q0 -29 5 -61.5t14 -59.5 q10 -27 27 -34q16 -6 47 8q31 13 59 35t45 45t7 36q-14 17 -53.5 25t-65.5 47q-16 23 -48.5 22t-35.5 -22v0z" />|svg}
    | Heart1 ->
      {svg|<path d="M510 512v-68.5v-107.5q-15 0 -33 -21q-19 -21 -45 -46t-61 -46t-83 -21q-55 0 -120.5 25.5t-59.5 148.5q2 27 20 62t47 74h335v0z" />|svg}
    | Heart2 ->
      {svg|<path d="M510 824v-488q-15 0 -33 -21q-19 -21 -45 -46t-61 -46t-83 -21q-55 0 -120.5 25.5t-59.5 148.5q3 52 61 128q58 77 129.5 148t135.5 121q63 51 76 51z" />|svg}
    | Heart3 ->
      {svg|<path d="M510 824q10 0 49 -29q39 -28 90 -73t105 -101q54 -55 94 -109h-86h-112h-98h-42v-176q-15 0 -33 -21q-19 -21 -45 -46t-61 -46t-83 -21q-55 0 -120.5 25.5t-59.5 148.5q3 52 61 128q58 77 129.5 148t135.5 121q63 51 76 51z" />|svg}
    | Hearty
     |Heart ->
      {svg|<path d="M510 824q14 0 78 -51q64 -50 136 -121t130 -148q58 -76 58 -128q2 -123 -64 -148.5t-122 -25.5q-47 0 -81 21t-59 46t-43 46t-33 21t-33 -21q-19 -21 -45 -46t-61 -46t-83 -21q-55 0 -120.5 25.5t-59.5 148.5q3 52 61 128q58 77 129.5 148t135.5 121q63 51 76 51z" />|svg}
    | Energizing1
     |Enduring1 ->
      {svg|<path d="M141 391q16 -50 45 -93q28 -43 65.5 -77t83.5 -58t97 -34q21 -5 41 -7t41 -2v282q-29 0 -55 14.5t-40 40.5q-3 5 -5.5 10.5t-4.5 10.5z" />|svg}
    | Energizing2
     |Enduring2 ->
      {svg|<path d="M141 391q16 -50 45 -93q28 -43 65.5 -77t83.5 -58t97 -34q21 -5 41 -7t41 -2v282q-29 0 -55 14.5t-40 40.5q-3 5 -5.5 10.5t-4.5 10.5z" />
<path d="M284 829q-43 -31 -75 -71t-53 -86.5t-29 -97.5t-3 -103q2 -20 6.5 -40.5t10.5 -39.5l268 87q-9 28 -2.5 57t25.5 51q4 4 8.5 8t8.5 7z" />|svg}
    | Energizing3
     |Enduring3 ->
      {svg|<path d="M141 391q16 -50 45 -93q28 -43 65.5 -77t83.5 -58t97 -34q21 -5 41 -7t41 -2v282q-29 0 -55 14.5t-40 40.5q-3 5 -5.5 10.5t-4.5 10.5z" />
<path d="M284 829q-43 -31 -75 -71t-53 -86.5t-29 -97.5t-3 -103q2 -20 6.5 -40.5t10.5 -39.5l268 87q-9 28 -2.5 57t25.5 51q4 4 8.5 8t8.5 7z" />
<path d="M744 829q-42 31 -90 49q-49 19 -99.5 24t-100.5 -3q-51 -8 -99 -29q-19 -8 -37 -18.5t-34 -22.5l165 -228q24 17 53.5 20t56.5 -8l10 -6l10 -6l165 228v0z" />|svg}
    | Energizing4
     |Enduring4 ->
      {svg|<path d="M141 391q16 -50 45 -93q28 -43 65.5 -77t83.5 -58t97 -34q21 -5 41 -7t41 -2v282q-29 0 -55 14.5t-40 40.5q-3 5 -5.5 10.5t-4.5 10.5z" />
<path d="M284 829q-43 -31 -75 -71t-53 -86.5t-29 -97.5t-3 -103q2 -20 6.5 -40.5t10.5 -39.5l268 87q-9 28 -2.5 57t25.5 51q4 4 8.5 8t8.5 7z" />
<path d="M744 829q-42 31 -90 49q-49 19 -99.5 24t-100.5 -3q-51 -8 -99 -29q-19 -8 -37 -18.5t-34 -22.5l165 -228q24 17 53.5 20t56.5 -8l10 -6l10 -6l165 228v0z" />
<path d="M887 391q16 50 19 101q2 52 -8.5 101.5t-33.5 95.5q-24 46 -59 85q-13 16 -28.5 29.5t-32.5 25.5l-165 -228q24 -17 35.5 -44t8.5 -56q0 -6 -1.5 -12t-2.5 -11l268 -87v0z" />|svg}
    | Energizing
     |Enduring ->
      {svg|<path d="M514 120q53 0 102 14q50 13 94 38.5t81 61.5q36 37 62 82q11 18 19 36.5t15 38.5l-268 87q-9 -28 -31 -47.5t-51 -26.5q-6 -1 -11.5 -1.5t-11.5 -0.5v-282v0zM887 391q16 50 19 101q2 52 -8.5 101.5t-33.5 95.5q-24 46 -59 85q-13 16 -28.5 29.5t-32.5 25.5l-165 -228 q24 -17 35.5 -44t8.5 -56q0 -6 -1.5 -12t-2.5 -11l268 -87v0zM744 829q-42 31 -90 49q-49 19 -99.5 24t-100.5 -3q-51 -8 -99 -29q-19 -8 -37 -18.5t-34 -22.5l165 -228q24 17 53.5 20t56.5 -8l10 -6l10 -6l165 228v0zM284 829q-43 -31 -75 -71t-53 -86.5t-29 -97.5t-3 -103 q2 -20 6.5 -40.5t10.5 -39.5l268 87q-9 28 -2.5 57t25.5 51q4 4 8.5 8t8.5 7zM141 391q16 -50 45 -93q28 -43 65.5 -77t83.5 -58t97 -34q21 -5 41 -7t41 -2v282q-29 0 -55 14.5t-40 40.5q-3 5 -5.5 10.5t-4.5 10.5z" />|svg}
    | Chilly ->
      {svg|<path d="M794 368q0 33 -23.5 56.5t-56.5 23.5t-56.5 -23.5t-23.5 -56.5t23.5 -56.5t56.5 -23.5t56.5 23.5t23.5 56.5z" />
<path d="M524 822q0 33 -23.5 56.5t-56.5 23.5t-56.5 -23.5t-23.5 -56.5t23.5 -56.5t56.5 -23.5t56.5 23.5t23.5 56.5z" />
<path d="M264 662q0 33 -23.5 56.5t-56.5 23.5t-56.5 -23.5t-23.5 -56.5t23.5 -56.5t56.5 -23.5t56.5 23.5t23.5 56.5z" />
<path d="M264 368q0 33 -23.5 56.5t-56.5 23.5t-56.5 -23.5t-23.5 -56.5t23.5 -56.5t56.5 -23.5t56.5 23.5t23.5 56.5z" />
<path d="M524 202q0 33 -23.5 56.5t-56.5 23.5t-56.5 -23.5t-23.5 -56.5t23.5 -56.5t56.5 -23.5t56.5 23.5t23.5 56.5z" />
<path d="M604 512q0 66 -47 113t-113 47t-113 -47t-47 -113t47 -113t113 -47t113 47t47 113z" />
<path fill="#949487" d="M696 534h140v170h140l-210 280l-210 -280h140v-170z" />|svg}
    | Electro -> {svg|<path d="M618 100l-65 292l305 72l-460 460l65 -292l-305 -72z" />|svg}
    | Fireproof ->
      {svg|<path d="M805 739q-18 40 -46 73q-29 34 -66 58.5t-81 38.5q-44 13 -92 13q-49 0 -102 -11q-53 -10 -99.5 -36.5t-79.5 -71.5q-34 -46 -44 -114q-9 -69 -6 -122q3 -52 15.5 -94t34.5 -75q21 -33 48 -64q55 -60 90 -83.5t53 -47.5t17.5 -56.5t11.5 -40.5t28 -2.5t31 52.5 q14 48 11.5 79.5t-16.5 60.5q-22 45 -59.5 109.5t0.5 130.5q39 66 101.5 60.5t93.5 -39.5q31 -33 20 -67t-40 -90q-29 -57 -15 -92.5t53 -20.5q40 15 75.5 73t62.5 99q28 40 32.5 120t-32.5 160v0z" />|svg}
    | Hasty ->
      {svg|<path d="M90 800l242 -468l158 238l102 -174l-152 -136l480 -170v580l-160 -140l-266 368l-156 -314z" />|svg}
    | Sneaky ->
      {svg|<path d="M190 280q45 0 65 -50t65 -50t65 50t65 50t65 -50t65 -50t65 50t65 50t65 -50t65 -50q22 0 34 25q11 25 11 55t-11 55t-34 25q-45 0 -65 50t-65 50t-65 -50t-65 -50t-65 50t-65 50t-65 -50t-65 -50t-65 50t-65 50q-22 0 -34 -25q-11 -25 -11 -55t11 -55t34 -25zM190 662 q45 0 65 -50t65 -50t65 50t65 50t65 -50t65 -50t65 50t65 50t65 -50t65 -50q22 0 34 25q11 25 11 55t-11 55t-34 25q-45 0 -65 50t-65 50t-65 -50t-65 -50t-65 50t-65 50t-65 -50t-65 -50t-65 50t-65 50q-22 0 -34 -25q-11 -25 -11 -55t11 -55t34 -25z" />|svg}
    | Spicy ->
      {svg|<path d="M364 154q0 -25 17.5 -42.5t42.5 -17.5h14q25 0 42.5 17.5t17.5 42.5v210l181 -105q22 -12 46 -6t36 28l8 12q12 22 5.5 46t-27.5 36l-182 105l182 105q21 12 27.5 36t-5.5 46l-7 12q-13 22 -37 28t-45 -6l-182 -105v210q0 25 -17.5 42.5t-42.5 17.5h-14 q-25 0 -42.5 -17.5t-17.5 -42.5v-210l-182 105q-21 12 -45 6t-37 -28l-7 -12q-12 -22 -5.5 -46t27.5 -36l182 -105l-182 -105q-21 -12 -27.5 -36t5.5 -46l7 -12q13 -22 37 -28t45 6l182 105v-210v0z" />
<path fill="#949487" d="M696 534h140v170h140l-210 280l-210 -280h140v-170z" />|svg}
    | Mighty ->
      {svg|<path d="M196 495l232 232l5 54l-55 -5l-83 -83l-128 127l-68 3l3 -68l128 -127l-84 -84l-5 -54z" />
<path d="M276 494l332 -332l166 -4l-7 162l-333 332l-53 -53l191 -191l-48 -48l-191 191z" />|svg}
    | Tough ->
      {svg|<path d="M272 140q42 45 86 67q44 23 89 23t89 -22q44 -23 86 -68q174 127 203 148.5l29 21.5l-132 160l-68 -60l50 390h-514l50 -390l-68 60l-132 -160l29 -21t203 -149zM447 422q22 0 37.5 -15.5t15.5 -37.5t-15.5 -37.5t-37.5 -15.5t-37.5 15.5t-15.5 37.5t15.5 37.5t37.5 15.5 zM447 570q22 0 37.5 -15.5t15.5 -37.5t-15.5 -37.5t-37.5 -15.5t-37.5 15.5t-15.5 37.5t15.5 37.5t37.5 15.5z" />|svg}
    | Check_all ->
      {svg|<path d="M8.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L2.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093L8.95 4.992a.252.252 0 0 1 .02-.022zm-.92 5.14.92.92a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 1 0-1.091-1.028L9.477 9.417l-.485-.486-.943 1.179z"/>|svg}
    | Reception_0 ->
      {svg|<path d="M0 13.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5z"/>|svg}
    | Reception_1 ->
      {svg|<path d="M0 11.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-2zm4 2a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5z"/>|svg}
    | Reception_2 ->
      {svg|<path d="M0 11.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-2zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v5a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-5zm4 5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5zm4 0a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5z"/>|svg}
    | Reception_3 ->
      {svg|<path d="M0 11.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-2zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v5a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-5zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v8a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-8zm4 8a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 0 1h-2a.5.5 0 0 1-.5-.5z"/>|svg}
    | Reception_4 ->
      {svg|<path d="M0 11.5a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-2zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v5a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-5zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v8a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-8zm4-3a.5.5 0 0 1 .5-.5h2a.5.5 0 0 1 .5.5v11a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-11z"/>|svg}
    | X ->
      {svg|<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/>|svg}
  in
  let html =
    sprintf
      {svg|<svg width="%frem" height="%frem" viewBox="%d %d %d %d" class="%s" %s fill=%s xmlns="http://www.w3.org/2000/svg">
%s
</svg>
|svg}
      width height v1 v2 v3 v4
      (String.concat ~sep:" " raw_extra_classes)
      (if bold then sprintf {|stroke=%s stroke-width="0.75"|} fill else "")
      fill paths
  in
  match container with
  | Div ->
    Node.inner_html ~tag:"div" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html
      ([ attrs; Attr.[ classes [ "v-center"; "align-items-center" ] ] ]
      |> List.concat_no_order
      |> Attrs.merge_classes_and_styles
      )
  | Span -> Node.inner_html ~tag:"span" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html attrs
