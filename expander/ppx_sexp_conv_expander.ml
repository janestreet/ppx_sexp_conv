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

  let sig_type_decl ~loc ~path ~unboxed (rf, tds) ~stackify ~portable =
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    let include_infos =
      match tds with
      | [] | _ :: _ :: _ -> None
      | [ td ] ->
        mk_named_sig
          ~loc
          ~sg_name:"Sexplib0.Sexpable.S"
          ~handle_polymorphic_variant:false
          [ td ]
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
