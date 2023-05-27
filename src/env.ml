open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open Js_of_ocaml

let is_dev =
  match Url.Current.arguments with
  | [ ("score", "1") ] -> true
  | _ -> false
