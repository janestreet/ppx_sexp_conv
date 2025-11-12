open! Stdppx
open! Ppxlib
open Ppxlib.Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default
open Helpers
open Lifted.Monad_infix

let of_sexp_function_for_type ?(internal = false) ?functor_:modname typename =
  let name, suffix = Ppx_helpers.demangle_template typename in
  let prefix =
    match modname with
    | Some modname -> modname ^ "__"
    | None -> ""
  in
  let internal_mangling = if internal then "__" else "" in
  internal_mangling ^ prefix ^ name ^ "_of_sexp" ^ internal_mangling ^ suffix
;;

(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct
  let type_of_of_sexp ~loc t =
    let loc = { loc with loc_ghost = true } in
    [%type: Sexplib0.Sexp.t -> [%t t]]
  ;;

  let mk_type td =
    Ppx_helpers.combinator_type_of_type_declaration
      td
      ~f:type_of_of_sexp
      ~phantom_attr:Attrs.phantom
  ;;

  let sig_of_td ~poly ~portable td =
    let of_sexp_type =
      mk_type td
      |> Ppx_helpers.Polytype.to_core_type
           ~universally_quantify_only_if_jkind_annotation:true
    in
    let loc = td.ptype_loc in
    let of_sexp_item =
      psig_value
        ~loc
        (Ppxlib_jane.Ast_builder.Default.value_description
           ~loc
           ~name:(Located.map of_sexp_function_for_type td.ptype_name)
           ~type_:of_sexp_type
           ~modalities:
             (if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
           ~prim:[])
    in
    match poly, is_polymorphic_variant td ~sig_:true with
    | true, `Surely_not ->
      Location.raise_errorf
        ~loc
        "Sig_generate_of_sexp.sig_of_td: sexp_poly annotation but type is surely not a \
         polymorphic variant"
    | false, (`Surely_not | `Maybe) -> [ of_sexp_item ]
    | (true | false), `Definitely | true, `Maybe ->
      [ of_sexp_item
      ; psig_value
          ~loc
          (Ppxlib_jane.Ast_builder.Default.value_description
             ~loc
             ~name:(Located.map (of_sexp_function_for_type ~internal:true) td.ptype_name)
             ~type_:of_sexp_type
             ~modalities:
               (if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
             ~prim:[])
      ]
  ;;

  let mk_sig ~poly ~loc:_ ~path:_ ~unboxed (_rf, tds) ~portable =
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    List.concat_map tds ~f:(sig_of_td ~poly ~portable)
  ;;
end

module Str_generate_of_sexp = struct
  module Ptag_error_function = struct
    type t =
      | Ptag_no_args
      | Ptag_takes_args
  end

  module Row_or_constructor = struct
    type t =
      | Row of row_field
      | Constructor of constructor_declaration
  end

  let with_error_source ~loc ~full_type_name make_body =
    let lifted =
      let name = lazy (Fresh_name.create "error_source" ~loc) in
      make_body ~error_source:(fun () -> Fresh_name.expression (Lazy.force name))
      >>| fun body ->
      match Lazy.is_val name with
      | false ->
        (* no references to [name], no need to define it *)
        body
      | true ->
        (* add a definition for [name] *)
        [%expr
          let [%p Fresh_name.pattern (Lazy.force name)] =
            [%e estring ~loc full_type_name]
          in
          [%e body]]
    in
    Lifted.let_bind_user_expressions lifted ~loc
  ;;

  (* [raising_expr] must be an expression that always raises. Such expressions are
     polymorphic in their return, but not yet layout polymorphic. This makes them layout
     polymorphic. *)
  let wrap_error_for_layout_any ~loc ~unboxed raising_expr =
    if unboxed
    then
      [%expr
        match [%e raising_expr] with
        | (_ : Sexplib0.Sexp_conv_error.nothing) -> .]
    else raising_expr
  ;;

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    [ [%pat? Sexplib0.Sexp_conv_error.No_variant_match] --> expr ]
  ;;

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc ~match_last ~fresh_atom matches =
    match match_last, matches with
    | true, [ { pc_lhs = _; pc_guard = None; pc_rhs = expr } ]
    | _, [ { pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = expr } ] -> expr
    | _ -> pexp_match ~loc (Fresh_name.expression fresh_atom) matches
  ;;

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches ~error_source ~fresh__sexp loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      (pstring ~loc cnstr
       -->
       match (call : Ptag_error_function.t) with
       | Ptag_no_args ->
         [%expr
           Sexplib0.Sexp_conv_error.ptag_no_args
             [%e error_source ()]
             [%e Fresh_name.expression fresh__sexp]]
       | Ptag_takes_args ->
         [%expr
           Sexplib0.Sexp_conv_error.ptag_takes_args
             [%e error_source ()]
             [%e Fresh_name.expression fresh__sexp]])
      :: acc
    in
    let exc_no_variant_match =
      [%pat? _] --> [%expr Sexplib0.Sexp_conv_error.no_variant_match ()]
    in
    List.fold_left ~f:coll_structs ~init:[ exc_no_variant_match ] rev_els
  ;;

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let split_row_field ~loc (atoms, structs, ainhs, sinhs) row_field =
    match row_field.prf_desc with
    | Rtag ({ txt = cnstr; _ }, true, []) ->
      let tpl = loc, cnstr in
      tpl :: atoms, structs, `A tpl :: ainhs, sinhs
    | Rtag ({ txt = cnstr; _ }, false, [ tp ]) ->
      let loc = tp.ptyp_loc in
      atoms, (loc, cnstr) :: structs, ainhs, `S (loc, cnstr, tp, row_field) :: sinhs
    | Rinherit inh ->
      let iinh = `I inh in
      atoms, structs, iinh :: ainhs, iinh :: sinhs
    | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
      Location.raise_errorf ~loc "unsupported: polymorphic variant intersection type"
    | Rtag (_, false, []) ->
      Location.raise_errorf ~loc "unsupported: polymorphic variant empty type"
  ;;

  let pat_of_of_sexp ~loc typ =
    let loc = { loc with loc_ghost = true } in
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc with
    | Ptyp_constr (id, _) ->
      Ppx_helpers.type_constr_conv_pat
        ~loc
        id
        ~f:(of_sexp_function_for_type ?internal:None)
    | Ptyp_var _ ->
      Ast_builder.Default.ppat_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Type variables are disallowed here. Instead, consider using a locally \
            abstract type.")
    | _ ->
      Ast_builder.Default.ppat_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Only type constructors are allowed here (e.g. [t], ['a t], or [M(X).t]).")
  ;;

  let type_constr_of_sexp ~loc ?internal id args =
    Ppx_helpers.type_constr_conv_expr
      ~loc
      id
      args
      ~f:(of_sexp_function_for_type ?internal)
  ;;

  (* Conversion of types *)
  let rec type_of_sexp ~error_source ~typevars ?full_type ?(internal = false) typ
    : Conversion.t
    =
    let loc = typ.ptyp_loc in
    match Ppxlib_jane.Shim.Core_type.of_parsetree typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Conversion.of_reference_exn ~thunk:false [%expr Sexplib0.Sexp_conv.opaque_of_sexp]
    | { ptyp_desc = Ptyp_any _; _ } ->
      Conversion.of_reference_exn ~thunk:false [%expr Sexplib0.Sexp_conv.opaque_of_sexp]
    | { ptyp_desc = (Ptyp_tuple labeled_tps | Ptyp_unboxed_tuple labeled_tps) as desc; _ }
      ->
      let unboxed =
        match desc with
        | Ptyp_unboxed_tuple _ -> true
        | Ptyp_tuple _ -> false
        | _ -> assert false
      in
      (match Ppxlib_jane.as_unlabeled_tuple labeled_tps with
       | Some tps ->
         Conversion.of_lambda (tuple_of_sexp ~error_source ~typevars ~unboxed (loc, tps))
       | None ->
         Conversion.of_reference_exn
           ~thunk:unboxed
           (labeled_tuple_of_sexp ~error_source ~typevars ~unboxed ~loc labeled_tps))
    | { ptyp_desc = Ptyp_var (parm, _); _ } ->
      (match String.Map.find_opt parm typevars with
       | Some fresh ->
         Conversion.of_reference_exn ~thunk:false (Fresh_name.expression fresh)
       | None ->
         Location.raise_errorf ~loc "ppx_sexp_conv: unbound type variable '%s" parm)
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      (match typ with
       | [%type: [%t? _] sexp_opaque] ->
         Conversion.of_reference_exn
           ~thunk:false
           [%expr Sexplib0.Sexp_conv.opaque_of_sexp]
       | [%type: [%t? ty1] sexp_list] ->
         let arg1 =
           Conversion.to_expression
             ~loc
             (type_of_sexp ~error_source ~typevars ty1)
             ~stackify:false
         in
         Conversion.of_reference_exn
           ~thunk:false
           [%expr Sexplib0.Sexp_conv.list_of_sexp [%e arg1]]
       | [%type: [%t? ty1] sexp_array] ->
         let arg1 =
           Conversion.to_expression
             ~loc
             (type_of_sexp ~error_source ~typevars ty1)
             ~stackify:false
         in
         Conversion.of_reference_exn
           ~thunk:false
           [%expr Sexplib0.Sexp_conv.array_of_sexp [%e arg1]]
       | _ ->
         let args =
           List.filter args ~f:include_param_in_combinator
           |> List.map ~f:(fun arg ->
             Conversion.to_expression
               ~loc
               (type_of_sexp ~error_source ~typevars arg)
               ~stackify:false)
         in
         Conversion.of_reference_exn
           ~thunk:false
           (type_constr_of_sexp ~loc ~internal id args))
    | { ptyp_desc = Ptyp_arrow (_, _, _, _, _); _ } ->
      Conversion.of_reference_exn ~thunk:false [%expr Sexplib0.Sexp_conv.fun_of_sexp]
    | { ptyp_desc = Ptyp_variant (row_fields, Closed, _); _ } ->
      variant_of_sexp ~error_source ~typevars ?full_type (loc, row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      poly_of_sexp ~error_source ~typevars parms poly_tp
    | core_type ->
      Location.raise_errorf
        ~loc
        "Type unsupported for ppx [of_sexp] conversion (%s)"
        (Ppxlib_jane.Language_feature_name.of_core_type_desc core_type.ptyp_desc)

  (* Conversion of (unlabeled) tuples *)
  and tuple_of_sexp ~error_source ~typevars ~unboxed (loc, tps) =
    let fps = List.map ~f:(type_of_sexp ~error_source ~typevars) tps in
    let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
      Conversion.apply_all ~loc fps
    in
    let converted = List.map ~f:(fun e -> None, e) converted in
    let n = List.length fps in
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let pexp_tuple = if unboxed then pexp_unboxed_tuple else pexp_tuple in
    [ [%pat? Sexplib0.Sexp.List [%p plist ~loc arguments]]
      --> pexp_let ~loc Immutable Nonrecursive bindings (pexp_tuple ~loc converted)
    ; Fresh_name.pattern fresh_sexp
      --> ([%expr
             Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
               [%e error_source ()]
               [%e eint ~loc n]
               [%e Fresh_name.expression fresh_sexp]]
           |> wrap_error_for_layout_any ~loc ~unboxed)
    ]

  (* Conversion of labeled tuples *)
  and labeled_tuple_of_sexp ~error_source ~typevars ~unboxed ~loc alist =
    assert (Labeled_tuple.has_any_label alist);
    let fields_expr =
      List.fold_right
        alist
        ~init:[%expr Empty]
        ~f:(fun (label_option, core_type) rest_expr ->
          let name_expr = estring ~loc (Labeled_tuple.atom_of_label label_option) in
          let conv_expr =
            type_of_sexp ~error_source ~typevars core_type
            |> Conversion.to_expression ~loc:core_type.ptyp_loc ~stackify:false
          in
          let conv_expr =
            let sexp = Fresh_name.create ~loc "sexp" in
            let value = Fresh_name.create ~loc "value" in
            [%expr
              fun [%p Fresh_name.pattern sexp] ->
                let [%p Fresh_name.pattern value] =
                  ([%e conv_expr] [@inlined never]) [%e Fresh_name.expression sexp]
                in
                fun () -> [%e Fresh_name.expression value]]
          in
          [%expr
            Field { name = [%e name_expr]; conv = [%e conv_expr]; rest = [%e rest_expr] }])
    in
    let create_expr =
      let pats, exprs =
        let list =
          List.map alist ~f:(fun (label_option, _) ->
            let name = Fresh_name.create ~loc "field" in
            let name_expr = Fresh_name.expression name in
            let expr = [%expr [%e name_expr] ()] in
            Fresh_name.pattern name, (label_option, expr))
        in
        List.map list ~f:fst, List.map list ~f:snd
      in
      let pat =
        List.fold_right pats ~init:(punit ~loc) ~f:(fun pat1 pat2 ->
          ppat_tuple ~loc [ None, pat1; None, pat2 ] Closed)
      in
      let expr =
        Ppxlib_jane.Ast_builder.Default.(
          if unboxed then pexp_unboxed_tuple else pexp_tuple)
          ~loc
          exprs
      in
      if unboxed
      then [%expr fun [%p pat] () -> [%e expr]]
      else [%expr fun [%p pat] -> [%e expr]]
    in
    pexp_apply
      ~loc
      [%expr Sexplib0.Sexp_conv_labeled_tuple.labeled_tuple_of_sexp]
      [ Labelled "caller", error_source ()
      ; Labelled "fields", fields_expr
      ; Labelled "create", create_expr
      ]

  (* Generate code for matching included variant types *)
  and handle_variant_inh
    ~error_source
    ~typevars
    ~fresh_atom
    ~fresh__sexp
    full_type
    ~match_last
    other_matches
    inh
    =
    let loc = inh.ptyp_loc in
    let func_expr = type_of_sexp ~error_source ~typevars ~internal:true inh in
    let app =
      Conversion.of_reference_exn
        ~thunk:false
        (Conversion.apply ~loc func_expr (Fresh_name.expression fresh__sexp))
    in
    let match_exc =
      handle_no_variant_match
        loc
        (handle_variant_match_last loc ~match_last ~fresh_atom other_matches)
    in
    let new_other_matches =
      [ [%pat? _]
        --> pexp_try
              ~loc
              [%expr
                ([%e Conversion.to_expression ~loc app ~stackify:false]
                  :> [%t replace_variables_by_underscores full_type])]
              match_exc
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom
    ~error_source
    ~typevars
    ~fresh_atom
    ~fresh__sexp
    loc
    full_type
    rev_atoms_inhs
    rev_structs
    =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
        let new_match = pstring ~loc cnstr --> pexp_variant ~loc cnstr None in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~error_source
          ~typevars
          ~fresh_atom
          ~fresh__sexp
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches =
      mk_variant_other_matches ~error_source ~fresh__sexp loc rev_structs Ptag_takes_args
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs
    in
    handle_variant_match_last loc ~match_last ~fresh_atom match_atoms_inhs

  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match
    ~error_source
    ~typevars
    ~loc
    ~is_variant
    ~fresh__sexp
    ~fresh__tag
    ~fresh_sexp_args
    cnstr
    tps
    row
    =
    let cnstr vars_expr =
      if is_variant
      then pexp_variant ~loc cnstr (Some vars_expr)
      else pexp_construct ~loc (Located.lident ~loc cnstr) (Some vars_expr)
    in
    match tps with
    | [ tp ]
      when Option.is_some
             (match (row : Row_or_constructor.t) with
              | Row r -> Attribute.get Attrs.list_poly r
              | Constructor c -> Attribute.get Attrs.list_variant c) ->
      (match tp with
       | [%type: [%t? tp] list] ->
         let cnv =
           Conversion.to_expression
             ~loc
             (type_of_sexp ~error_source ~typevars tp)
             ~stackify:false
         in
         cnstr
           [%expr
             Sexplib0.Sexp_conv.list_map
               [%e cnv]
               [%e Fresh_name.expression fresh_sexp_args]]
       | _ ->
         (match row with
          | Row _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list"
          | Constructor _ -> Attrs.invalid_attribute ~loc Attrs.list_variant "_ list"))
    | [ [%type: [%t? tp] sexp_list] ] ->
      let cnv =
        Conversion.to_expression
          ~loc
          (type_of_sexp ~error_source ~typevars tp)
          ~stackify:false
      in
      cnstr
        [%expr
          Sexplib0.Sexp_conv.list_map [%e cnv] [%e Fresh_name.expression fresh_sexp_args]]
    | _ ->
      let bindings, patts, good_arg_match =
        let fps = List.map ~f:(type_of_sexp ~error_source ~typevars) tps in
        let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
          Conversion.apply_all ~loc fps
        in
        let good_arg_match =
          cnstr (Ppxlib.Ast_builder.Default.pexp_tuple ~loc converted)
        in
        bindings, arguments, good_arg_match
      in
      [%expr
        match [%e Fresh_name.expression fresh_sexp_args] with
        | [%p plist ~loc patts] ->
          [%e pexp_let ~loc Immutable Nonrecursive bindings good_arg_match]
        | _ ->
          [%e
            if is_variant
            then
              [%expr
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh__tag]
                  [%e Fresh_name.expression fresh__sexp]]
            else
              [%expr
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh__tag]
                  [%e Fresh_name.expression fresh__sexp]]]]

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct
    ~error_source
    ~typevars
    ~fresh_atom
    ~fresh__sexp
    ~fresh_sexp_args
    loc
    full_type
    rev_structs_inhs
    rev_atoms
    =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tp, row) ->
        has_structs_ref := true;
        let fresh__tag = Fresh_name.create "_tag" ~loc in
        let expr =
          mk_cnstr_args_match
            ~error_source
            ~typevars
            ~loc:tp.ptyp_loc
            ~is_variant:true
            ~fresh__sexp
            ~fresh__tag
            ~fresh_sexp_args
            cnstr
            [ tp ]
            (Row row)
        in
        let new_match =
          ppat_alias
            ~loc
            [%pat? [%p pstring ~loc cnstr]]
            (Fresh_name.to_string_loc fresh__tag)
          --> expr
        in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~error_source
          ~typevars
          ~fresh_atom
          ~fresh__sexp
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches =
      mk_variant_other_matches ~error_source ~fresh__sexp loc rev_atoms Ptag_no_args
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    ( handle_variant_match_last loc ~match_last ~fresh_atom match_structs_inhs
    , !has_structs_ref )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag ~error_source ~typevars loc full_type row_field_list =
    let fresh_atom = Fresh_name.create "atom" ~loc in
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let fresh__sexp = Fresh_name.create "_sexp" ~loc in
    let fresh_sexp_args = Fresh_name.create "sexp_args" ~loc in
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:(split_row_field ~loc) ~init:([], [], [], []) row_field_list
    in
    let match_struct, has_structs =
      mk_variant_match_struct
        ~error_source
        ~typevars
        ~fresh_atom
        ~fresh__sexp
        ~fresh_sexp_args
        loc
        full_type
        rev_structs_inhs
        rev_atoms
    in
    let maybe_sexp_args_patt =
      if has_structs then Fresh_name.pattern fresh_sexp_args else [%pat? _]
    in
    [ ppat_alias
        ~loc
        [%pat? Sexplib0.Sexp.Atom [%p Fresh_name.pattern fresh_atom]]
        (Fresh_name.to_string_loc fresh__sexp)
      --> mk_variant_match_atom
            ~error_source
            ~typevars
            ~fresh_atom
            ~fresh__sexp
            loc
            full_type
            rev_atoms_inhs
            rev_structs
    ; ppat_alias
        ~loc
        [%pat?
          Sexplib0.Sexp.List
            (Sexplib0.Sexp.Atom [%p Fresh_name.pattern fresh_atom]
            :: [%p maybe_sexp_args_patt])]
        (Fresh_name.to_string_loc fresh__sexp)
      --> match_struct
    ; ppat_alias
        ~loc
        [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _)]
        (Fresh_name.to_string_loc fresh_sexp)
      --> [%expr
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
              [%e error_source ()]
              [%e Fresh_name.expression fresh_sexp]]
    ; ppat_alias ~loc [%pat? Sexplib0.Sexp.List []] (Fresh_name.to_string_loc fresh_sexp)
      --> [%expr
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
              [%e error_source ()]
              [%e Fresh_name.expression fresh_sexp]]
    ]

  (* Generate matching code for variants *)
  and variant_of_sexp ~error_source ~typevars ?full_type (loc, row_fields) =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let top_match =
      let fresh_sexp = Fresh_name.create ~loc "sexp" in
      match row_fields with
      | { prf_desc = Rinherit inh; _ } :: rest ->
        let rec loop inh row_fields =
          let call =
            [%expr
              ([%e
                 Conversion.to_expression
                   ~loc
                   (type_of_sexp ~error_source ~typevars ~internal:true inh)
                   ~stackify:false]
                 [%e Fresh_name.expression fresh_sexp]
                :> [%t replace_variables_by_underscores full_type])]
          in
          match row_fields with
          | [] -> call
          | h :: t ->
            let expr =
              match h.prf_desc with
              | Rinherit inh -> loop inh t
              | _ ->
                let rftag_matches =
                  handle_variant_tag ~error_source ~typevars loc full_type row_fields
                in
                pexp_match ~loc (Fresh_name.expression fresh_sexp) rftag_matches
            in
            pexp_try ~loc call (handle_no_variant_match loc expr)
        in
        [ Fresh_name.pattern fresh_sexp --> loop inh rest ]
      | _ :: _ -> handle_variant_tag ~error_source ~typevars loc full_type row_fields
      | [] ->
        Location.raise_errorf
          ~loc
          "of_sexp is not supported for empty polymorphic variants (impossible?)"
    in
    if is_contained
    then (
      let fresh_sexp = Fresh_name.create "sexp" ~loc in
      Conversion.of_lambda
        [ Fresh_name.pattern fresh_sexp
          --> [%expr
                try [%e pexp_match ~loc (Fresh_name.expression fresh_sexp) top_match] with
                | Sexplib0.Sexp_conv_error.No_variant_match ->
                  Sexplib0.Sexp_conv_error.no_matching_variant_found
                    [%e error_source ()]
                    [%e Fresh_name.expression fresh_sexp]]
        ])
    else Conversion.of_lambda top_match

  and poly_of_sexp ~error_source ~typevars parms tp =
    let loc = tp.ptyp_loc in
    let typevars =
      List.fold_left parms ~init:typevars ~f:(fun map (parm, _jkind) ->
        String.Map.add parm.txt (Fresh_name.create ("_of_" ^ parm.txt) ~loc:parm.loc) map)
    in
    let bindings =
      let mk_binding (parm, _jkind) =
        let fresh = String.Map.find parm.txt typevars in
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        value_binding
          ~loc
          ~pat:(Fresh_name.pattern fresh)
          ~expr:
            [%expr
              fun [%p Fresh_name.pattern fresh_sexp] ->
                Sexplib0.Sexp_conv_error.record_poly_field_value
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh_sexp]]
          ~modes:[]
      in
      List.map ~f:mk_binding parms
    in
    Conversion.bind (type_of_sexp ~error_source ~typevars tp) bindings
  ;;

  type record_poly_type =
    { type_and_field_name : Fresh_name.t
    ; params : string loc list
    ; body : core_type
    }

  let record_poly_type field =
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree field.pld_type.ptyp_desc with
    | Ptyp_poly (params, body) ->
      let type_and_field_name = Fresh_name.of_string_loc field.pld_name in
      let params = List.map params ~f:(fun (name, _jkind) -> name) in
      Some { type_and_field_name; params; body }
    | _ -> None
  ;;

  let close_over_non_value ~loc expr =
    let result = gen_symbol () in
    [%expr
      let [%p pvar ~loc result] = [%e expr] in
      fun () -> [%e evar ~loc result]]
  ;;

  let record_field_conv field ~poly ~loc ~error_source ~typevars =
    match poly with
    | None ->
      let conv =
        type_of_sexp ~error_source ~typevars field.pld_type
        |> Conversion.to_expression ~loc ~stackify:false
      in
      fresh_lambda ~loc (fun ~arg ->
        close_over_non_value ~loc [%expr ([%e conv] [@inlined never]) [%e arg]])
    | Some { type_and_field_name; params; body } ->
      let fresh_sexp = Fresh_name.create "sexp" ~loc in
      let fresh_params =
        List.map params ~f:(fun { loc; txt } -> Fresh_name.create ~loc ("_" ^ txt))
      in
      let pat = Fresh_name.pattern fresh_sexp in
      let body =
        let label = Located.map_lident (Fresh_name.to_string_loc type_and_field_name) in
        let typevars =
          List.fold_left2
            params
            fresh_params
            ~init:typevars
            ~f:(fun typevars param fresh -> String.Map.add param.txt fresh typevars)
        in
        let expr =
          pexp_let
            ~loc
            Immutable
            Nonrecursive
            (List.map fresh_params ~f:(fun fresh ->
               let { loc; txt } = Fresh_name.to_string_loc fresh in
               let expr =
                 [%expr
                   Sexplib0.Sexp_conv_error.record_poly_field_value [%e error_source ()]]
               in
               value_binding ~loc ~pat:(pvar ~loc txt) ~expr ~modes:[]))
            (Conversion.apply
               (type_of_sexp ~error_source ~typevars body)
               ~loc
               (Fresh_name.expression fresh_sexp))
        in
        pexp_record ~loc [ label, expr ] None
      in
      eabstract ~loc [ pat ] (close_over_non_value ~loc body)
  ;;

  let fields_arg_for_record_of_sexp poly_fields ~loc ~error_source ~typevars =
    List.fold_right
      poly_fields
      ~init:(Lifted.return [%expr Empty])
      ~f:
        (fun
          ( poly
          , field
          , (attrs : Record_field_attrs.Of_sexp.t Record_field_attrs.Generic.t) )
          rest_lifted
        ->
        rest_lifted
        >>= fun rest_expr ->
        let label_expr = estring ~loc:field.pld_name.loc field.pld_name.txt in
        match attrs with
        | Specific Required ->
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Required
                ; conv = [%e record_field_conv field ~poly ~loc ~error_source ~typevars]
                ; rest = [%e rest_expr]
                }]
        | Specific (Default lifted) ->
          lifted
          >>| fun default ->
          [%expr
            Field
              { name = [%e label_expr]
              ; kind = Default (fun () -> [%e default])
              ; conv = [%e record_field_conv field ~poly ~loc ~error_source ~typevars]
              ; rest = [%e rest_expr]
              }]
        | Omit_nil ->
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Omit_nil
                ; conv = [%e record_field_conv field ~poly ~loc ~error_source ~typevars]
                ; rest = [%e rest_expr]
                }]
        | Sexp_bool ->
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Sexp_bool
                ; conv = ()
                ; rest = [%e rest_expr]
                }]
        | Sexp_array core_type ->
          let conv_expr =
            type_of_sexp ~error_source ~typevars core_type
            |> Conversion.to_expression ~loc ~stackify:false
          in
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Sexp_array
                ; conv = [%e conv_expr]
                ; rest = [%e rest_expr]
                }]
        | Sexp_list core_type ->
          let conv_expr =
            type_of_sexp ~error_source ~typevars core_type
            |> Conversion.to_expression ~loc ~stackify:false
          in
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Sexp_list
                ; conv = [%e conv_expr]
                ; rest = [%e rest_expr]
                }]
        | Sexp_option core_type ->
          let conv_expr =
            type_of_sexp ~error_source ~typevars core_type
            |> Conversion.to_expression ~loc ~stackify:false
          in
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Sexp_option
                ; conv = [%e conv_expr]
                ; rest = [%e rest_expr]
                }]
        | Sexp_or_null core_type ->
          let conv_expr =
            type_of_sexp ~error_source ~typevars core_type
            |> Conversion.to_expression ~loc ~stackify:false
          in
          Lifted.return
            [%expr
              Field
                { name = [%e label_expr]
                ; kind = Sexp_or_null
                ; conv = [%e conv_expr]
                ; rest = [%e rest_expr]
                }])
  ;;

  let index_of_field_arg_for_record_of_sexp fields ~loc =
    let field_cases =
      List.mapi fields ~f:(fun i (_, field, _) ->
        let lhs = pstring ~loc:field.pld_name.loc field.pld_name.txt in
        let rhs = eint ~loc i in
        case ~lhs ~guard:None ~rhs)
    in
    let default_case = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(eint ~loc (-1)) in
    let cases = List.concat [ field_cases; [ default_case ] ] in
    pexp_function ~loc cases
  ;;

  let create_arg_for_record_of_sexp td fields ~loc ~constructor ~unboxed =
    let vars =
      List.map
        fields
        ~f:
          (fun
            ( poly
            , field
            , (attrs : Record_field_attrs.Of_sexp.t Record_field_attrs.Generic.t) )
          ->
          let pvar = pvar ~loc:field.pld_name.loc field.pld_name.txt in
          let pat =
            match poly with
            | None -> pvar
            | Some { type_and_field_name; _ } ->
              (* Extract a polymorphic value from a polymorphic record defined explicitly
                 for this purpose. *)
              let label =
                Located.map_lident (Fresh_name.to_string_loc type_and_field_name)
              in
              ppat_record ~loc [ label, pvar ] Closed
          in
          let evar = evar ~loc:field.pld_name.loc field.pld_name.txt in
          let pat, rebind, expr =
            match attrs with
            | Specific (Required | Default _) | Omit_nil ->
              pvar, Some (pat, [%expr [%e evar] ()]), evar
            | Sexp_bool | Sexp_array _ | Sexp_list _ | Sexp_option _ | Sexp_or_null _ ->
              pat, None, evar
          in
          pat, rebind, (Located.map_lident field.pld_name, expr))
    in
    let pat =
      List.fold_right
        vars
        ~init:[%pat? ()]
        ~f:(fun (head, _, _) tail -> ppat_tuple ~loc [ None, head; None, tail ] Closed)
    in
    let body =
      let record_expr =
        (if unboxed
         then Ppxlib_jane.Ast_builder.Default.pexp_record_unboxed_product ?attrs:None
         else pexp_record)
          ~loc
          (List.map vars ~f:(fun (_, _, field) -> field))
          None
      in
      let record_or_variant_expr =
        match constructor with
        | None -> record_expr
        | Some label ->
          (* variant constructor with inline record *)
          pexp_construct ~loc label (Some record_expr)
      in
      List.fold_right vars ~init:record_or_variant_expr ~f:(fun (_, rebind, _) body ->
        match rebind with
        | None -> body
        | Some (pat, expr) ->
          [%expr
            let [%p pat] = [%e expr] in
            [%e body]])
    in
    let core_type =
      ptyp_constr
        ~loc
        (Located.map_lident td.ptype_name)
        (List.map td.ptype_params ~f:(fun (core_type, _) ->
           ptyp_any ~loc:core_type.ptyp_loc))
    in
    Ppxlib_jane.Ast_builder.Default.eabstract
      ~loc
      (if unboxed then [ pat; [%pat? ()] ] else [ pat ])
      body
      ~return_constraint:core_type
  ;;

  let polymorphic_record_types_for_record_of_sexp fields ~loc =
    (* Define fresh types to contain polymorphic values parsed from sexps. *)
    List.filter_map fields ~f:(fun (poly, _, _) ->
      match poly with
      | Some { type_and_field_name; params; body } ->
        let fresh_field =
          label_declaration
            ~loc
            ~name:(Fresh_name.to_string_loc type_and_field_name)
            ~modalities:[]
            ~mutable_:Immutable
            ~type_:
              (strip_attributes#core_type
                 (Ppxlib.Ast_builder.Default.ptyp_poly ~loc params body))
        in
        let type_decl =
          type_declaration
            ~loc
            ~name:(Fresh_name.to_string_loc type_and_field_name)
            ~params:[]
            ~cstrs:[]
            ~kind:(Ptype_record [ fresh_field ])
            ~private_:Public
            ~manifest:None
            ()
        in
        Some
          { type_decl with
            ptype_attributes =
              (* define unboxed types to avoid allocation *)
              [ { attr_loc = loc
                ; attr_name = { loc; txt = "unboxed" }
                ; attr_payload = PStr []
                }
              ]
          }
      | None -> None)
  ;;

  let args_for_record_of_sexp
    td
    fields
    ~loc
    ~error_source
    ~typevars
    ~constructor
    ~allow_extra_fields
    ~unboxed
    =
    let caller_expr = error_source () in
    let allow_extra_fields_expr = ebool ~loc allow_extra_fields in
    let fields =
      List.map fields ~f:(fun field ->
        record_poly_type field, field, Record_field_attrs.Of_sexp.create ~loc field)
    in
    let index_of_field_expr = index_of_field_arg_for_record_of_sexp fields ~loc in
    let create_expr =
      create_arg_for_record_of_sexp td fields ~loc ~constructor ~unboxed
    in
    let fields_expr_lifted =
      fields_arg_for_record_of_sexp fields ~loc ~error_source ~typevars
    in
    fields_expr_lifted
    >>| fun fields_expr ->
    let types = polymorphic_record_types_for_record_of_sexp fields ~loc in
    let args =
      [ Labelled "caller", caller_expr
      ; Labelled "fields", fields_expr
      ; Labelled "index_of_field", index_of_field_expr
      ; Labelled "allow_extra_fields", allow_extra_fields_expr
      ; Labelled "create", create_expr
      ]
    in
    types, args
  ;;

  (* Generate matching code for records *)
  let record_of_sexp ~error_source ~typevars ~allow_extra_fields ~unboxed td (loc, flds) =
    args_for_record_of_sexp
      td
      flds
      ~loc
      ~error_source
      ~typevars
      ~constructor:None
      ~allow_extra_fields
      ~unboxed
    >>| fun (types, args) ->
    let conv =
      pexp_apply ~loc [%expr Sexplib0.Sexp_conv_record.record_of_sexp] args
      |> Conversion.of_reference_exn ~thunk:unboxed
    in
    Conversion.bind_types conv types
  ;;

  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let mk_good_sum_matches ~error_source ~typevars td (_, cds) =
    List.map cds ~f:(fun cd ->
      let loc = cd.pcd_loc in
      match cd with
      | { pcd_name = constructor; pcd_args = Pcstr_record fields; _ } ->
        let allow_extra_fields =
          Option.is_some (Attribute.get Attrs.allow_extra_fields_cd cd)
        in
        args_for_record_of_sexp
          td
          fields
          ~loc
          ~error_source
          ~typevars
          ~constructor:(Some (Located.map_lident constructor))
          ~allow_extra_fields
          ~unboxed:false
        >>| fun (types, args) ->
        let string_pat =
          let loc = constructor.loc in
          ppat_or
            ~loc
            (pstring ~loc (String.uncapitalize_ascii constructor.txt))
            (pstring ~loc constructor.txt)
        in
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        let fresh_sexps = Fresh_name.create "sexps" ~loc in
        ppat_alias
          ~loc
          [%pat?
            Sexplib0.Sexp.List
              (Sexplib0.Sexp.Atom [%p string_pat] :: [%p Fresh_name.pattern fresh_sexps])]
          (Fresh_name.to_string_loc fresh_sexp)
        --> (pexp_apply
               ~loc
               [%expr Sexplib0.Sexp_conv_record.record_of_sexps]
               (List.concat
                  [ [ Labelled "context", Fresh_name.expression fresh_sexp ]
                  ; args
                  ; [ Nolabel, Fresh_name.expression fresh_sexps ]
                  ])
             |> with_types ~loc ~types)
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _ } ->
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        let lcstr = pstring ~loc (String.uncapitalize_ascii cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        [%pat? Sexplib0.Sexp.Atom ([%p lcstr] | [%p str])]
        --> pexp_construct ~loc (Located.lident ~loc cnstr.txt) None
        |> Lifted.return
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple (_ :: _ as args); _ } ->
        let tps = List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type in
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        let lcstr = pstring ~loc (String.uncapitalize_ascii cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        let fresh__sexp = Fresh_name.create "_sexp" ~loc in
        let fresh__tag = Fresh_name.create "_tag" ~loc in
        let fresh_sexp_args = Fresh_name.create "sexp_args" ~loc in
        ppat_alias
          ~loc
          [%pat?
            Sexplib0.Sexp.List
              (Sexplib0.Sexp.Atom
                 [%p
                   ppat_alias
                     ~loc
                     [%pat? [%p lcstr] | [%p str]]
                     (Fresh_name.to_string_loc fresh__tag)]
              :: [%p Fresh_name.pattern fresh_sexp_args])]
          (Fresh_name.to_string_loc fresh__sexp)
        --> mk_cnstr_args_match
              ~error_source
              ~typevars
              ~loc
              ~is_variant:false
              ~fresh__sexp
              ~fresh__tag
              ~fresh_sexp_args
              cnstr.txt
              tps
              (Constructor cd)
        |> Lifted.return)
  ;;

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let mk_bad_sum_matches ~error_source (loc, cds) =
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let no_payload, yes_payload =
      List.partition_map
        (function
          | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _ } -> Left cnstr
          | { pcd_name = cnstr; pcd_args = Pcstr_tuple (_ :: _) | Pcstr_record _; _ } ->
            Right cnstr)
        cds
    in
    let or_constructors list =
      (* "constructor1" | "Constructor1" | "constructor2" | "Constructor2" | ... *)
      match
        List.concat_map list ~f:(fun constructor ->
          [ Loc.map constructor ~f:String.uncapitalize_ascii
          ; Loc.map constructor ~f:String.capitalize_ascii
          ])
        |> List.map ~f:(fun { loc; txt } -> pstring ~loc txt)
      with
      | [] -> None
      | head :: tail -> Some (List.fold_left ~init:head tail ~f:(ppat_or ~loc))
    in
    let no_payload =
      Option.map (or_constructors no_payload) ~f:(fun constructor_pattern ->
        ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.Atom [%p constructor_pattern] :: _)]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
              Sexplib0.Sexp_conv_error.stag_no_args
                [%e error_source ()]
                [%e Fresh_name.expression fresh_sexp]])
    in
    let yes_payload =
      Option.map (or_constructors yes_payload) ~f:(fun constructor_pattern ->
        ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.Atom [%p constructor_pattern]]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
              Sexplib0.Sexp_conv_error.stag_takes_args
                [%e error_source ()]
                [%e Fresh_name.expression fresh_sexp]])
    in
    List.filter_opt [ no_payload; yes_payload ]
  ;;

  (* Generate matching code for sum types *)
  let sum_of_sexp ~error_source ~typevars td (loc, alts) =
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let alts_strings =
      elist
        ~loc
        (List.map alts ~f:(fun alt ->
           let { txt; loc } = alt.pcd_name in
           estring ~loc txt))
    in
    [ mk_good_sum_matches ~error_source ~typevars td (loc, alts) |> Lifted.all
    ; mk_bad_sum_matches ~error_source (loc, alts) |> Lifted.return
    ; [ ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _)]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
              Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                [%e error_source ()]
                [%e Fresh_name.expression fresh_sexp]]
      ; ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List []]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
              Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                [%e error_source ()]
                [%e Fresh_name.expression fresh_sexp]]
      ; Fresh_name.pattern fresh_sexp
        --> [%expr
              Sexplib0.Sexp_conv_error.unexpected_stag
                [%e error_source ()]
                [%e alts_strings]
                [%e Fresh_name.expression fresh_sexp]]
      ]
      |> Lifted.return
    ]
    |> Lifted.all
    >>| List.concat
    >>| Conversion.of_lambda
  ;;

  (* Empty type *)
  let nil_of_sexp ~error_source loc : Conversion.t =
    Conversion.of_reference_exn
      ~thunk:false
      [%expr Sexplib0.Sexp_conv_error.empty_type [%e error_source ()]]
  ;;

  (* Generate code from type definitions *)

  let td_of_sexp ~typevars ~loc:_ ~poly ~path ~rec_flag ~values_being_defined ~portable td
    =
    let tps =
      List.filter td.ptype_params ~f:(fun (p, _) -> include_param_in_combinator p)
      |> List.map ~f:Ppxlib_jane.get_type_param_name_and_jkind
    in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let full_type =
      core_type_of_type_declaration td |> replace_variables_by_underscores
    in
    let is_private =
      match td.ptype_private with
      | Private -> true
      | Public -> false
    in
    if is_private
    then Location.raise_errorf ~loc "of_sexp is not supported for private type";
    let create_internal_function =
      match is_polymorphic_variant td ~sig_:false with
      | `Definitely -> true
      | `Maybe -> poly
      | `Surely_not ->
        if poly
        then
          Location.raise_errorf
            ~loc
            "sexp_poly annotation on a type that is surely not a polymorphic variant";
        false
    in
    let body ~error_source =
      let body =
        match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
        | Ptype_variant alts ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          sum_of_sexp ~error_source ~typevars td (td.ptype_loc, alts)
        | Ptype_record lbls ->
          record_of_sexp
            ~error_source
            ~typevars
            ~allow_extra_fields:
              (Option.is_some (Attribute.get Attrs.allow_extra_fields_td td))
            ~unboxed:false
            td
            (loc, lbls)
        | Ptype_record_unboxed_product lbls ->
          record_of_sexp
            ~error_source
            ~typevars
            ~allow_extra_fields:
              (Option.is_some (Attribute.get Attrs.allow_extra_fields_td td))
            ~unboxed:true
            td
            (loc, lbls)
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          (match td.ptype_manifest with
           | None -> nil_of_sexp ~error_source td.ptype_loc |> Lifted.return
           | Some ty ->
             type_of_sexp
               ~error_source
               ~full_type
               ~typevars
               ~internal:create_internal_function
               ty
             |> Lifted.return)
      in
      (* Prevent violation of value restriction, problems with recursive types, and
         toplevel effects by eta-expanding function definitions *)
      body
      >>| Conversion.to_value_expression
            ~loc
            ~rec_flag
            ~values_being_defined
            ~stackify:false
    in
    let name ~internal = of_sexp_function_for_type ~internal type_name in
    let arg_patts, arg_exprs =
      let list =
        List.map
          ~f:(fun (tp, _) ->
            let name = String.Map.find tp.txt typevars in
            Fresh_name.pattern name, Fresh_name.expression name)
          tps
      in
      List.map list ~f:fst, List.map list ~f:snd
    in
    let full_type_name =
      Printf.sprintf "%s.%s" path (Ppx_helpers.mangle_unboxed type_name)
    in
    let internal_fun_body =
      if create_internal_function
      then
        Some
          (with_error_source ~loc ~full_type_name (fun ~error_source ->
             body ~error_source
             >>| fun body ->
             eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc arg_patts body)))
      else None
    in
    let external_fun_body =
      let body_below_lambdas ~error_source =
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        if create_internal_function
        then (
          let no_variant_match_mc =
            [ [%pat? Sexplib0.Sexp_conv_error.No_variant_match]
              --> [%expr
                    Sexplib0.Sexp_conv_error.no_matching_variant_found
                      [%e error_source ()]
                      [%e Fresh_name.expression fresh_sexp]]
            ]
          in
          let internal_call =
            let internal_expr =
              pexp_ident ~loc { loc; txt = Lident (name ~internal:true) }
            in
            eapply ~loc internal_expr (arg_exprs @ [ Fresh_name.expression fresh_sexp ])
          in
          let try_with = pexp_try ~loc internal_call no_variant_match_mc in
          [%expr fun [%p Fresh_name.pattern fresh_sexp] -> [%e try_with]] |> Lifted.return)
        else body ~error_source
      in
      let body_with_lambdas ~error_source =
        body_below_lambdas ~error_source
        >>| fun body ->
        eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc arg_patts body)
      in
      with_error_source ~loc ~full_type_name body_with_lambdas
    in
    let typ = Sig_generate_of_sexp.mk_type td in
    let mk_binding func_name body =
      let ({ vars = tps; body = typ; loc = _ } : Ppx_helpers.Polytype.t) = typ in
      constrained_function_binding loc td typ ~tps ~func_name ~portable body
    in
    let internal_bindings =
      match internal_fun_body with
      | None -> []
      | Some body -> [ mk_binding (name ~internal:true) body ]
    in
    let name = name ~internal:false in
    let external_binding = mk_binding name external_fun_body in
    internal_bindings, [ external_binding ], name
  ;;

  (* Generate code from type definitions *)
  let tds_of_sexp ~loc ~poly ~path ~portable ~unboxed (rec_flag, tds) =
    let tds = List.map ~f:name_type_params_in_td tds in
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    let typevars td =
      List.fold_left td.ptype_params ~init:String.Map.empty ~f:(fun map param ->
        let name = get_type_param_name param in
        String.Map.add name.txt (Fresh_name.create ("_of_" ^ name.txt) ~loc:name.loc) map)
    in
    let singleton =
      match tds with
      | [ _ ] -> true
      | _ -> false
    in
    let values_being_defined =
      List.map tds ~f:(fun td -> of_sexp_function_for_type td.ptype_name.txt)
      |> String.Set.of_list
    in
    let rec_flag = really_recursive_respecting_opaque rec_flag tds in
    let bindings, names =
      if singleton
      then (
        match rec_flag with
        | Recursive ->
          let bindings_and_names =
            List.map tds ~f:(fun td ->
              let typevars = typevars td in
              let internals, externals, name =
                td_of_sexp
                  ~typevars
                  ~loc
                  ~poly
                  ~path
                  ~rec_flag
                  ~values_being_defined
                  ~portable
                  td
              in
              internals @ externals, name)
          in
          let bindings = List.concat_map bindings_and_names ~f:fst in
          let names = List.map bindings_and_names ~f:snd in
          pstr_value_list ~loc Recursive bindings, names
        | Nonrecursive ->
          let bindings_and_names =
            List.map tds ~f:(fun td ->
              let typevars = typevars td in
              let internals, externals, name =
                td_of_sexp
                  ~typevars
                  ~loc
                  ~poly
                  ~path
                  ~rec_flag
                  ~values_being_defined
                  ~portable
                  td
              in
              ( pstr_value_list ~loc Nonrecursive internals
                @ pstr_value_list ~loc Nonrecursive externals
              , name ))
          in
          let bindings = List.concat_map bindings_and_names ~f:fst in
          let names = List.map bindings_and_names ~f:snd in
          bindings, names)
      else (
        (* If there are any polymorphic variants, the binding needs to be recursive (even if
         the types are not) in order for e.g. [t_of_sexp] to be able to reference
         [__t_of_sexp__]. *)
        let rec_flag =
          if List.exists tds ~f:(function
               | { ptype_manifest = Some { ptyp_desc = Ptyp_variant _; _ }; _ } -> true
               | _ -> false)
          then Recursive
          else rec_flag
        in
        let bindings_and_names =
          List.map tds ~f:(fun td ->
            let typevars = typevars td in
            let internals, externals, name =
              td_of_sexp
                ~typevars
                ~poly
                ~loc
                ~path
                ~rec_flag
                ~values_being_defined
                ~portable
                td
            in
            internals @ externals, name)
        in
        let bindings = List.concat_map bindings_and_names ~f:fst in
        let names = List.map bindings_and_names ~f:snd in
        pstr_value_list ~loc rec_flag bindings, names)
    in
    if portable
    then
      [ [%stri include [%m pmod_structure ~loc bindings]]
      ; pstr_value
          ~loc
          Nonrecursive
          (List.map names ~f:(fun name ->
             value_binding
               ~loc
               ~pat:[%pat? _]
               ~expr:(evar ~loc name)
               ~modes:[ { loc; txt = Mode "portable" } ]))
      ]
    else bindings
  ;;

  let core_type_of_sexp ~path core_type =
    let loc = { core_type.ptyp_loc with loc_ghost = true } in
    let full_type_name =
      Printf.sprintf
        "%s line %i: %s"
        path
        loc.loc_start.pos_lnum
        (string_of_core_type core_type)
    in
    with_error_source ~loc ~full_type_name (fun ~error_source ->
      type_of_sexp ~error_source ~typevars:String.Map.empty core_type
      |> Conversion.to_value_expression
           ~loc
           ~rec_flag:Nonrecursive
           ~values_being_defined:String.Set.empty
           ~stackify:false
      |> Merlin_helpers.hide_expression
      |> Lifted.return)
  ;;
end
