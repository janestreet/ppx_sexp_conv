open! Stdppx
open! Ppxlib
open Ast_builder.Default

type ext =
  | Gen
  | Ext

type mut =
  | Mut
  | Imm

type t = ext * mut

let all =
  List.concat_map [ Gen; Ext ] ~f:(fun ext ->
    List.map [ Mut; Imm ] ~f:(fun mut -> ext, mut))
;;

let suffix (ext, mut) =
  let ext =
    match ext with
    | Gen -> ""
    | Ext -> ".Ext"
  in
  let mut =
    match mut with
    | Mut -> ""
    | Imm -> ".Imm"
  in
  "Addr" ^ ext ^ mut
;;

let attr_name ~hum t =
  let sigil = if hum then "" else "@" in
  String.lowercase_ascii ("sexp." ^ sigil ^ suffix t)
;;

let common_type_name t = "Ox." ^ suffix t ^ ".t"

let runtime_lident ~loc t ident =
  Located.mk ~loc (Longident.parse ("Ppx_ox_runtime." ^ suffix t ^ "." ^ ident))
;;

let runtime_type ~loc t ty = ptyp_constr ~loc (runtime_lident ~loc t "t") [ ty ]
let runtime_get ~loc t = pexp_ident ~loc (runtime_lident ~loc t "get")
