open! Stdppx
open! Ppxlib

module To_lift = struct
  type 'a t = { to_lift : 'a } [@@unboxed]
end

open To_lift

let default =
  Attribute.declare
    "sexp.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> { to_lift = x })
;;

let drop_default =
  Attribute.declare_with_attr_loc
    "sexp.sexp_drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (many (pstr_eval __ nil)))
    (fun ~attr_loc -> function
      | [ x ] -> { to_lift = x }
      | _ ->
        Location.raise_errorf
          ~loc:attr_loc
          "Unsupported [@sexp_drop_default] payload; please use one of:\n\
           - [@sexp_drop_default f] and give an explicit equality function [f]\n\
           - [@sexp_drop_default.compare] if the type supports [%%compare]\n\
           - [@sexp_drop_default.equal] if the type supports [%%equal]\n\
           - [@sexp_drop_default.sexp] if you want to compare the sexp representations\n")
;;

let drop_default_equal =
  Attribute.declare
    "sexp.@sexp_drop_default.equal"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_compare =
  Attribute.declare
    "sexp.@sexp_drop_default.compare"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_sexp =
  Attribute.declare
    "sexp.@sexp_drop_default.sexp"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_if =
  Attribute.declare
    "sexp.sexp_drop_if"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> { to_lift = x })
;;

let opaque =
  Attribute.declare "sexp.opaque" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

let non_value_field =
  Attribute.declare
    "sexp.non_value"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let non_value_type =
  Attribute.declare "sexp.non_value" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

let omit_nil =
  Attribute.declare
    "sexp.omit_nil"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let option =
  Attribute.declare
    "sexp.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list =
  Attribute.declare
    "sexp.list"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let array =
  Attribute.declare
    "sexp.array"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let bool =
  Attribute.declare
    "sexp.bool"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list_variant =
  Attribute.declare
    "sexp.list"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list_exception =
  Attribute.declare "sexp.list" Attribute.Context.type_exception Ast_pattern.(pstr nil) ()
;;

let list_poly =
  Attribute.declare "sexp.list" Attribute.Context.rtag Ast_pattern.(pstr nil) ()
;;

let allow_extra_fields_td =
  Attribute.declare
    "sexp.allow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_cd =
  Attribute.declare
    "sexp.allow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let grammar_custom =
  Attribute.declare
    "sexp_grammar.custom"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
;;

let grammar_any =
  Attribute.declare
    "sexp_grammar.any"
    Attribute.Context.core_type
    Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
    (fun x -> x)
;;

let tag_attribute_for_context context =
  let open Ast_pattern in
  let key_equals_value =
    Ast_pattern.(
      pexp_apply (pexp_ident (lident (string "="))) (no_label __ ^:: no_label __ ^:: nil)
      |> pack2)
  in
  let get_captured_values ast_pattern context expression =
    Ast_pattern.to_func ast_pattern context expression.pexp_loc expression (fun x -> x)
  in
  let rec collect_sequence expression =
    match expression.pexp_desc with
    | Pexp_sequence (l, r) -> l :: collect_sequence r
    | _ -> [ expression ]
  in
  let esequence ast_pattern =
    Ast_pattern.of_func (fun context _loc expression k ->
      collect_sequence expression
      |> List.map ~f:(get_captured_values ast_pattern context)
      |> k)
  in
  Attribute.declare
    "sexp_grammar.tag"
    context
    (pstr (pstr_eval (esequence key_equals_value) nil ^:: nil))
    (fun x -> x)
;;

let tag_type = tag_attribute_for_context Core_type
let tag_ld = tag_attribute_for_context Label_declaration
let tag_cd = tag_attribute_for_context Constructor_declaration
let tag_poly = tag_attribute_for_context Rtag

let tags_attribute_for_context context =
  Attribute.declare
    "sexp_grammar.tags"
    context
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
;;

let tags_type = tags_attribute_for_context Core_type
let tags_ld = tags_attribute_for_context Label_declaration
let tags_cd = tags_attribute_for_context Constructor_declaration
let tags_poly = tags_attribute_for_context Rtag

let invalid_attribute ~loc attr description =
  Location.raise_errorf
    ~loc
    "ppx_sexp_conv: [@%s] is only allowed on type [%s]."
    (Attribute.name attr)
    description
;;

let fail_if_allow_extra_field_cd ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_cd x)
  then
    Location.raise_errorf
      ~loc
      "ppx_sexp_conv: [@@allow_extra_fields] is only allowed on inline records."
;;

let fail_if_allow_extra_field_td ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_td x)
  then (
    match x.ptype_kind with
    | Ptype_variant cds
      when List.exists cds ~f:(fun cd ->
             match cd.pcd_args with
             | Pcstr_record _ -> true
             | _ -> false) ->
      Location.raise_errorf
        ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] only works on records. For inline \
         records, do: type t = A of { a : int } [@@allow_extra_fields] | B [@@@@deriving \
         sexp]"
    | _ ->
      Location.raise_errorf
        ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] is only allowed on records.")
;;
