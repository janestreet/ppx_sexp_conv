open Base
open Ppxlib
open Ast_builder.Default
module Attrs = Attrs
module Record_field_attrs = Record_field_attrs
open Expand_sexp_of
open Expand_of_sexp

module Sexp_of = struct
  let type_extension ty =
    Sig_generate_sexp_of.type_of_sexp_of ~loc:{ ty.ptyp_loc with loc_ghost = true } ty
  ;;

  let core_type ty = Str_generate_sexp_of.sexp_of_core_type ty
  let sig_type_decl = Sig_generate_sexp_of.mk_sig
  let sig_exception = Sig_generate_sexp_of.mk_sig_exn
  let str_type_decl = Str_generate_sexp_of.sexp_of_tds
  let str_exception = Str_generate_sexp_of.sexp_of_exn
end

module Sexp_grammar = Ppx_sexp_conv_grammar

module Of_sexp = struct
  let type_extension ty = Sig_generate_of_sexp.type_of_of_sexp ~loc:ty.ptyp_loc ty
  let core_type = Str_generate_of_sexp.core_type_of_sexp

  let sig_type_decl ~poly ~loc ~path tds =
    Sig_generate_of_sexp.mk_sig ~poly ~loc ~path tds
  ;;

  let str_type_decl ~loc ~poly ~path tds =
    Str_generate_of_sexp.tds_of_sexp ~loc ~poly ~path tds
  ;;
end

module Sig_sexp = struct
  let mk_sig ~loc ~path decls =
    List.concat
      [ Sig_generate_sexp_of.mk_sig ~loc ~path decls
      ; Sig_generate_of_sexp.mk_sig ~poly:false ~loc ~path decls
      ]
  ;;

  let sig_type_decl ~loc ~path ((_rf, tds) as decls) =
    let include_infos =
      match tds with
      | [] | _ :: _ :: _ -> None
      | [ td ] ->
        let sg_name =
          let open struct
            type is_value =
              | Value
              | Maybe_non_value
          end in
          let has_jkind_annotation =
            match Ppxlib_jane.Jane_syntax.Layouts.of_type_declaration td with
            | None -> None
            | Some (jkind, _) ->
              (match jkind.txt with
               | Default -> None (* [t : _] *)
               | Abbreviation { txt = "value"; _ } -> Some Value (* [t : value] *)
               | _ -> Some Maybe_non_value)
          in
          let is_value =
            match td.ptype_kind, td.ptype_manifest with
            | (Ptype_variant _ | Ptype_record _ | Ptype_open), _ -> Value
            | Ptype_abstract, Some _ ->
              Option.value has_jkind_annotation ~default:Maybe_non_value
            | Ptype_abstract, None -> Option.value has_jkind_annotation ~default:Value
          in
          match is_value with
          | Value -> "Sexplib0.Sexpable.S"
          | Maybe_non_value -> "Sexplib0.Sexpable.S_any"
        in
        mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant:false [ td ]
    in
    match include_infos with
    | Some include_infos -> [ psig_include ~loc include_infos ]
    | None -> mk_sig ~loc ~path decls
  ;;
end
