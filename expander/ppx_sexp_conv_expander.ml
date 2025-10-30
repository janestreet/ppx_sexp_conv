open Stdppx
open Ppxlib
module Attrs = Attrs
module Record_field_attrs = Record_field_attrs
open Expand_sexp_of
open Expand_of_sexp

module Sexp_of = struct
  let type_extension ty ~stackify =
    Sig_generate_sexp_of.type_of_sexp_of
      ~loc:{ ty.ptyp_loc with loc_ghost = true }
      ty
      ~stackify
  ;;

  let pattern_extension ty ~stackify =
    Str_generate_sexp_of.pat_of_sexp_of
      ~loc:{ ty.ptyp_loc with loc_ghost = true }
      ty
      ~stackify
  ;;

  let core_type = Str_generate_sexp_of.sexp_of_core_type

  let sig_type_decl ~loc ~path ~unboxed tds ~stackify ~portable =
    let stackify =
      match stackify with
      | false -> [ false ]
      | true -> [ false; true ]
    in
    List.concat_map stackify ~f:(fun stackify ->
      Sig_generate_sexp_of.mk_sig ~loc ~path ~unboxed tds ~stackify ~portable)
  ;;

  let sig_exception = Sig_generate_sexp_of.mk_sig_exn
  let str_type_decl = Str_generate_sexp_of.sexp_of_tds
  let str_exception = Str_generate_sexp_of.sexp_of_exn
end

module Sexp_grammar = Ppx_sexp_conv_grammar

module Of_sexp = struct
  let type_extension ty = Sig_generate_of_sexp.type_of_of_sexp ~loc:ty.ptyp_loc ty
  let pattern_extension ty = Str_generate_of_sexp.pat_of_of_sexp ~loc:ty.ptyp_loc ty
  let core_type = Str_generate_of_sexp.core_type_of_sexp

  let sig_type_decl ~poly ~loc ~path ~unboxed tds ~portable =
    Sig_generate_of_sexp.mk_sig ~poly ~loc ~path ~unboxed tds ~portable
  ;;

  let str_type_decl ~loc ~poly ~path ~unboxed tds =
    Str_generate_of_sexp.tds_of_sexp ~loc ~poly ~path ~unboxed tds
  ;;
end

module Sig_sexp = struct
  let mk_sig ~loc ~path ~unboxed decls ~stackify ~portable =
    List.concat
      [ Sig_generate_sexp_of.mk_sig ~loc ~path ~unboxed decls ~stackify:false ~portable
      ; (if stackify
         then
           Sig_generate_sexp_of.mk_sig ~loc ~path ~unboxed decls ~stackify:true ~portable
         else [])
      ; Sig_generate_of_sexp.mk_sig ~poly:false ~loc ~path ~unboxed decls ~portable
      ]
  ;;

  module Is_value = struct
    type t =
      | Value
      | Maybe_non_value

    let rec of_jkind (jkind : Ppxlib_jane.Shim.jkind_annotation) =
      match jkind.pjkind_desc with
      | Pjk_default | Pjk_kind_of _ ->
        (* [t : _] or [t : kind_of u] *)
        None
      | Pjk_abbreviation ("value" | "immediate64" | "immediate") ->
        (* [t : value] or [t : immediate64] or [t : immediate] *)
        Some Value
      | Pjk_abbreviation _ | Pjk_product _ ->
        (* [t : k] or [t : k1 & k2]*)
        Some Maybe_non_value
      | Pjk_mod (jkind, _) | Pjk_with (jkind, _, _) ->
        (* [t : k mod m] or [t : k with t] *)
        of_jkind jkind
    ;;
  end

  let sig_type_decl ~loc ~path ~unboxed (rf, tds) ~stackify ~portable =
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    let include_infos =
      match tds with
      | [] | _ :: _ :: _ -> None
      | [ td ] ->
        let sg_name =
          let has_jkind_annotation =
            match Ppxlib_jane.Shim.Type_declaration.extract_jkind_annotation td with
            | None -> None
            | Some jkind -> Is_value.of_jkind jkind
          in
          let default_is_value : Is_value.t =
            match
              Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind, td.ptype_manifest
            with
            | (Ptype_variant _ | Ptype_record _ | Ptype_open), _ | Ptype_abstract, None ->
              Value (* not necessarily true if the type is [@@unboxed] *)
            | Ptype_record_unboxed_product _, _ | Ptype_abstract, Some _ ->
              Maybe_non_value
          in
          match Option.value has_jkind_annotation ~default:default_is_value with
          | Value -> "Sexplib0.Sexpable.S"
          | Maybe_non_value -> "Sexplib0.Sexpable.S_any"
        in
        mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant:false [ td ]
    in
    match include_infos with
    | Some include_infos ->
      let include_infos =
        match stackify with
        | false -> include_infos
        | true -> Ppxlib_jane.stackify_include_sig include_infos
      in
      [ Ppxlib_jane.Ast_builder.Default.psig_include
          ~loc
          ~modalities:
            (if portable then [ Loc.make ~loc (Ppxlib_jane.Modality "portable") ] else [])
          include_infos
      ]
    | None -> mk_sig ~loc ~path ~unboxed:false (rf, tds) ~stackify ~portable
  ;;
end
