open! Stdppx
open! Ppxlib
open Ppxlib.Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default
open Helpers
open Lifted.Monad_infix

let fmt ~stackify : _ format =
  match stackify with
  | false -> "sexp_of_%s%s"
  | true -> "sexp_of_%s%s__stack"
;;

let sexp_of_typename ~stackify ~prefix typename =
  Printf.sprintf (fmt ~stackify) prefix (Ppx_helpers.mangle_unboxed typename)
;;

let list_map ~loc ~stackify =
  match stackify with
  | false -> [%expr Sexplib0.Sexp_conv.list_map]
  | true -> [%expr Sexplib0.Sexp_conv.list_map__stack]
;;

let sexp_of_opaque ~loc ~stackify =
  match stackify with
  | false -> [%expr (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _)]
  | true -> [%expr (Sexplib0.Sexp_conv.sexp_of_opaque : _ @ local -> _ @ local)]
;;

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct
  let type_of_sexp_of ~loc t ~stackify =
    let loc = { loc with loc_ghost = true } in
    match stackify with
    | false -> [%type: [%t t] -> Sexplib0.Sexp.t]
    | true -> [%type: [%t t] @ local -> Sexplib0.Sexp.t @ local]
  ;;

  let mk_type td ~stackify =
    Ppx_helpers.combinator_type_of_type_declaration
      td
      ~f:(type_of_sexp_of ~stackify)
      ~phantom_attr:Attrs.phantom
  ;;

  let mk_val td ~stackify ~portable =
    let loc = td.ptype_loc in
    let name = Located.map (sexp_of_typename ~stackify ~prefix:"") td.ptype_name in
    psig_value
      ~loc
      (Ppxlib_jane.Ast_builder.Default.value_description
         ~loc
         ~name
         ~type_:
           (mk_type td ~stackify
            |> Ppx_helpers.Polytype.to_core_type
                 ~universally_quantify_only_if_jkind_annotation:true)
         ~modalities:(if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
         ~prim:[])
  ;;

  let mk_sig ~loc:_ ~path:_ ~unboxed (_rf, tds) ~stackify ~portable =
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    List.map tds ~f:(mk_val ~stackify ~portable)
  ;;

  let mk_sig_exn ~loc:_ ~path:_ _te = []
end

module Str_generate_sexp_of = struct
  module Types_being_defined = struct
    type t =
      | Nonrec
      | Rec of String.Set.t

    let to_rec_flag = function
      | Nonrec -> Nonrecursive
      | Rec _ -> Recursive
    ;;

    let to_values_being_defined t ~stackify =
      match t with
      | Nonrec -> String.Set.empty
      | Rec types -> String.Set.map (sexp_of_typename ~stackify ~prefix:"") types
    ;;
  end

  let sexp_of_ident_for_constr_conv ~stackify ?functor_:modname typename =
    let prefix =
      match modname with
      | Some modname -> modname ^ "__"
      | None -> ""
    in
    sexp_of_typename ~stackify ~prefix typename
  ;;

  let pat_of_sexp_of ~loc typ ~stackify =
    let loc = { loc with loc_ghost = true } in
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc with
    | Ptyp_constr (id, _) ->
      Ppx_helpers.type_constr_conv_pat
        ~loc
        id
        ~f:(sexp_of_ident_for_constr_conv ~stackify)
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

  let sexp_of_type_constr ~loc id args ~stackify =
    Ppx_helpers.type_constr_conv_expr
      ~loc
      id
      ~f:(sexp_of_ident_for_constr_conv ~stackify)
      args
  ;;

  (* Conversion of types *)
  let rec sexp_of_type ~renaming typ ~stackify : Conversion.t =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Shim.Core_type.of_parsetree typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Conversion.of_reference_exn ~thunk:false (sexp_of_opaque ~loc ~stackify)
    | { ptyp_desc = Ptyp_any _; _ } ->
      Conversion.of_lambda [ ppat_any ~loc --> [%expr Sexplib0.Sexp.Atom "_"] ]
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
         Conversion.of_lambda [ sexp_of_tuple ~renaming ~unboxed (loc, tps) ~stackify ]
       | None ->
         Conversion.of_lambda
           [ sexp_of_labeled_tuple ~renaming ~loc ~unboxed labeled_tps ~stackify ])
    | { ptyp_desc = Ptyp_var (parm, _); _ } ->
      (match Renaming.binding_kind renaming parm ~loc with
       | Universally_bound fresh ->
         Conversion.of_reference_exn ~thunk:false (Fresh_name.expression fresh)
       | Existentially_bound -> sexp_of_type ~renaming [%type: _] ~stackify)
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      (match typ with
       | [%type: [%t? _] sexp_opaque] ->
         Conversion.of_reference_exn ~thunk:false (sexp_of_opaque ~loc ~stackify)
       | _ ->
         Conversion.of_reference_exn
           ~thunk:false
           (sexp_of_type_constr
              ~loc
              id
              (List.filter args ~f:include_param_in_combinator
               |> List.map ~f:(fun tp ->
                 Conversion.to_expression
                   ~loc
                   (sexp_of_type ~renaming tp ~stackify)
                   ~stackify))
              ~stackify))
    | { ptyp_desc = Ptyp_arrow (_, _, _, _, _); _ } ->
      Conversion.of_lambda
        [ ppat_any ~loc
          --> [%expr Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore]
        ]
    | { ptyp_desc = Ptyp_variant (row_fields, Closed, _); _ } ->
      sexp_of_variant ~renaming (loc, row_fields) ~stackify
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      sexp_of_poly ~renaming parms poly_tp ~stackify
    | core_type ->
      Location.raise_errorf
        ~loc
        "Type unsupported for ppx [sexp_of] conversion (%s)"
        (Ppxlib_jane.Language_feature_name.of_core_type_desc core_type.ptyp_desc)

  (* Conversion of (unlabeled) tuples *)
  and sexp_of_tuple ~renaming ~unboxed (loc, tps) ~stackify =
    let fps = List.map ~f:(fun tp -> sexp_of_type ~renaming tp ~stackify) tps in
    let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
      Conversion.apply_all ~loc fps
    in
    let in_expr = [%expr Sexplib0.Sexp.List [%e elist ~loc converted]] in
    let expr = pexp_let ~loc Immutable Nonrecursive bindings in_expr in
    let arguments = List.map arguments ~f:(fun p -> None, p) in
    let ppat_tuple = if unboxed then ppat_unboxed_tuple else ppat_tuple in
    ppat_tuple ~loc arguments Closed --> expr

  (* Conversion of labeled tuples *)
  and sexp_of_labeled_tuple ~renaming ~loc ~unboxed alist ~stackify =
    let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
      List.map alist ~f:(fun (_, core_type) -> sexp_of_type ~renaming core_type ~stackify)
      |> Conversion.apply_all ~loc
    in
    let expr =
      let sexp_exprs =
        (* Constructor inference allows to to leave off [Sexplib0.Sexp.] here. *)
        List.map2 alist converted ~f:(fun (label_option, _) expr ->
          [%expr
            List
              [ Atom [%e estring ~loc (Labeled_tuple.atom_of_label label_option)]
              ; [%e expr]
              ]])
      in
      [%expr Sexplib0.Sexp.List [%e elist ~loc sexp_exprs]]
      |> pexp_let ~loc Immutable Nonrecursive bindings
    in
    let ppat_tuple = if unboxed then ppat_unboxed_tuple else ppat_tuple in
    let pat =
      ppat_tuple
        ~loc
        (List.map2 alist arguments ~f:(fun (label_option, _) arg -> label_option, arg))
        Closed
    in
    pat --> expr

  (* Conversion of variant types *)
  and sexp_of_variant
    ~renaming
    ((loc, row_fields) : Location.t * row_field list)
    ~stackify
    : Conversion.t
    =
    let item row =
      match row.prf_desc with
      | Rtag ({ txt = cnstr; _ }, true, []) ->
        ppat_variant ~loc cnstr None
        --> [%expr Sexplib0.Sexp.Atom [%e estring ~loc cnstr]]
      | Rtag ({ txt = cnstr; _ }, _, [ tp ])
        when Option.is_some (Attribute.get Attrs.list_poly row) ->
        (match tp with
         | [%type: [%t? tp] list] ->
           let cnv_expr =
             Conversion.to_expression ~loc (sexp_of_type ~renaming tp ~stackify) ~stackify
           in
           let name = Fresh_name.create "l" ~loc in
           ppat_variant ~loc cnstr (Some (Fresh_name.pattern name))
           --> [%expr
                 Sexplib0.Sexp.List
                   (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
                    :: [%e list_map ~loc ~stackify]
                         [%e cnv_expr]
                         [%e Fresh_name.expression name])]
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list")
      | Rtag ({ txt = cnstr; _ }, _, [ [%type: [%t? tp] sexp_list] ]) ->
        let cnv_expr =
          Conversion.to_expression ~loc (sexp_of_type ~renaming tp ~stackify) ~stackify
        in
        let name = Fresh_name.create "l" ~loc in
        ppat_variant ~loc cnstr (Some (Fresh_name.pattern name))
        --> [%expr
              Sexplib0.Sexp.List
                (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
                 :: [%e list_map ~loc ~stackify]
                      [%e cnv_expr]
                      [%e Fresh_name.expression name])]
      | Rtag ({ txt = cnstr; _ }, false, [ tp ]) ->
        let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e estring ~loc cnstr]] in
        let fresh = Fresh_name.create "v" ~loc in
        let cnstr_arg =
          Conversion.apply
            ~loc
            (sexp_of_type ~renaming tp ~stackify)
            (Fresh_name.expression fresh)
        in
        let expr = [%expr Sexplib0.Sexp.List [%e elist ~loc [ cnstr_expr; cnstr_arg ]]] in
        ppat_variant ~loc cnstr (Some (Fresh_name.pattern fresh)) --> expr
      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        let name = Fresh_name.create "v" ~loc in
        ppat_alias ~loc (ppat_type ~loc id) (Fresh_name.to_string_loc name)
        --> sexp_of_type_constr ~loc id [ Fresh_name.expression name ] ~stackify
      | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
        Location.raise_errorf ~loc "unsupported: polymorphic variant intersection type"
      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _ :: _); _ } as typ) ->
        let call =
          Conversion.to_expression ~loc (sexp_of_type ~renaming typ ~stackify) ~stackify
        in
        let name = Fresh_name.create "v" ~loc in
        ppat_alias ~loc (ppat_type ~loc id) (Fresh_name.to_string_loc name)
        --> [%expr [%e call] [%e Fresh_name.expression name]]
      | Rinherit _ ->
        Location.raise_errorf
          ~loc
          "unsupported: polymorphic variant with invalid (non-identifier) inherited type"
      | Rtag (_, false, []) ->
        Location.raise_errorf ~loc "unsupported: polymorphic variant empty type"
    in
    Conversion.of_lambda (List.map ~f:item row_fields)

  (* Polymorphic record fields *)
  and sexp_of_poly ~renaming parms tp ~stackify =
    let loc = tp.ptyp_loc in
    let renaming =
      List.fold_left parms ~init:renaming ~f:(fun renaming (name, _jkind) ->
        Renaming.add_universally_bound renaming name ~prefix:"_of_")
    in
    let bindings =
      let mk_binding (parm, _jkind) =
        let name =
          match Renaming.binding_kind renaming parm.txt ~loc:parm.loc with
          | Universally_bound name -> name
          | Existentially_bound -> assert false
        in
        value_binding
          ~loc
          ~pat:(Fresh_name.pattern name)
          ~expr:(sexp_of_opaque ~loc ~stackify)
          ~modes:[]
      in
      List.map ~f:mk_binding parms
    in
    Conversion.bind (sexp_of_type ~renaming tp ~stackify) bindings
  ;;

  (* Conversion of record types *)

  let mk_rec_patt loc patt name fresh =
    let p = Loc.make (Longident.Lident name) ~loc, Fresh_name.pattern fresh in
    patt @ [ p ]
  ;;

  type is_empty_expr =
    | Inspect_value of (location -> expression -> expression)
    | Inspect_sexp of (cnv_expr:expression -> location -> expression -> expression)

  let sexp_of_record_field
    ~renaming
    ~bnds
    patt
    expr
    name
    tp
    ?sexp_of
    is_empty_expr
    ~stackify
    =
    let loc = tp.ptyp_loc in
    let fresh = Fresh_name.create name ~loc in
    let patt = mk_rec_patt loc patt name fresh in
    let cnv_expr =
      Conversion.to_expression ~loc (sexp_of_type ~renaming tp ~stackify) ~stackify
    in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> [%expr [%e sexp_of] [%e cnv_expr]]
    in
    let bnd = Fresh_name.create "bnd" ~loc in
    let arg = Fresh_name.create "arg" ~loc in
    let expr =
      [%expr
        let [%p Fresh_name.pattern bnds] =
          [%e
            match is_empty_expr with
            | Inspect_value is_empty_expr ->
              [%expr
                if [%e is_empty_expr loc (Fresh_name.expression fresh)]
                then [%e Fresh_name.expression bnds]
                else (
                  let [%p Fresh_name.pattern arg] =
                    [%e cnv_expr] [%e Fresh_name.expression fresh]
                  in
                  let [%p Fresh_name.pattern bnd] =
                    Sexplib0.Sexp.List
                      [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                      ; [%e Fresh_name.expression arg]
                      ]
                  in
                  ([%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
                   : _ Stdlib.List.t))]
            | Inspect_sexp is_empty_expr ->
              [%expr
                let [%p Fresh_name.pattern arg] =
                  [%e cnv_expr] [%e Fresh_name.expression fresh]
                in
                if [%e is_empty_expr ~cnv_expr loc (Fresh_name.expression arg)]
                then [%e Fresh_name.expression bnds]
                else (
                  let [%p Fresh_name.pattern bnd] =
                    Sexplib0.Sexp.List
                      [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                      ; [%e Fresh_name.expression arg]
                      ]
                  in
                  ([%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
                   : _ Stdlib.List.t))]]
        in
        [%e expr]]
    in
    patt, expr
  ;;

  let disallow_type_variables_and_recursive_occurrences
    ~types_being_defined
    ~loc
    ~attr_name
    tp
    =
    let disallow_variables =
      let iter =
        object
          inherit Ast_traverse.iter as super

          method! core_type_desc t =
            match Ppxlib_jane.Shim.Core_type_desc.of_parsetree t with
            | Ptyp_var (v, _) ->
              Location.raise_errorf
                ~loc
                "[@%s] was used, but the type of the field contains a type variable: '%s.\n\
                 Comparison is not avaiable for type variables.\n\
                 Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] instead."
                attr_name
                v
            | _ -> super#core_type_desc t
        end
      in
      iter#core_type
    in
    let disallow_recursive_occurrences =
      match (types_being_defined : Types_being_defined.t) with
      | Nonrec -> fun _ -> ()
      | Rec types_being_defined ->
        let iter =
          object
            inherit Ast_traverse.iter as super

            method! core_type_desc =
              function
              | Ptyp_constr ({ loc = _; txt = Lident s }, _) as t ->
                if String.Set.mem s types_being_defined
                then
                  Location.raise_errorf
                    ~loc
                    "[@%s] was used, but the type of the field contains a type defined \
                     in the current recursive block: %s.\n\
                     This is not supported.\n\
                     Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] \
                     instead."
                    attr_name
                    s;
                super#core_type_desc t
              | t -> super#core_type_desc t
          end
        in
        iter#core_type
    in
    disallow_variables tp;
    disallow_recursive_occurrences tp
  ;;

  let sexp_of_default_field
    ~types_being_defined
    how
    ~renaming
    ~bnds
    patt
    expr
    name
    tp
    ?sexp_of
    default
    ~stackify
    =
    let is_empty =
      let inspect_value equality_f =
        Inspect_value (fun loc expr -> [%expr [%e equality_f loc] [%e default] [%e expr]])
      in
      match (how : Record_field_attrs.Sexp_of.Drop.t) with
      | Sexp ->
        Inspect_sexp
          (fun ~cnv_expr loc sexp_expr ->
            [%expr Sexplib0.Sexp_conv.( = ) ([%e cnv_expr] [%e default]) [%e sexp_expr]])
        |> Lifted.return
      | Func lifted -> lifted >>| fun f -> inspect_value (fun _ -> f)
      | Compare ->
        inspect_value (fun loc ->
          disallow_type_variables_and_recursive_occurrences
            ~types_being_defined
            ~attr_name:"sexp_drop_default.compare"
            ~loc
            tp;
          [%expr [%compare.equal: [%t tp]]])
        |> Lifted.return
      | Equal ->
        inspect_value (fun loc ->
          disallow_type_variables_and_recursive_occurrences
            ~types_being_defined
            ~attr_name:"sexp_drop_default.equal"
            ~loc
            tp;
          [%expr [%equal: [%t tp]]])
        |> Lifted.return
    in
    is_empty >>| sexp_of_record_field ~renaming ~bnds patt expr name tp ?sexp_of ~stackify
  ;;

  let sexp_of_label_declaration_list
    ~types_being_defined
    ~renaming
    loc
    flds
    ~wrap_expr
    ~stackify
    ~unboxed
    =
    let bnds = Fresh_name.create "bnds" ~loc in
    let list_empty_expr =
      Inspect_value
        (fun loc lst ->
          [%expr
            match [%e lst] with
            | [] -> true
            | _ -> false])
    in
    let array_empty_expr =
      Inspect_value
        (fun loc arr ->
          [%expr
            match [%e arr] with
            | [||] -> true
            | _ -> false])
    in
    let coll lifted ld =
      lifted
      >>= fun ((patt : (Longident.t loc * pattern) list), expr) ->
      let name = ld.pld_name.txt in
      let loc = ld.pld_name.loc in
      let fresh = Fresh_name.create name ~loc in
      match Record_field_attrs.Sexp_of.create ~loc ld with
      | Sexp_option tp ->
        let v = Fresh_name.create "v" ~loc in
        let bnd = Fresh_name.create "bnd" ~loc in
        let arg = Fresh_name.create "arg" ~loc in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression v in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~renaming tp ~stackify) vname
        in
        let expr =
          [%expr
            let [%p Fresh_name.pattern bnds] =
              match [%e Fresh_name.expression fresh] with
              | Stdlib.Option.None -> [%e Fresh_name.expression bnds]
              | Stdlib.Option.Some [%p Fresh_name.pattern v] ->
                let [%p Fresh_name.pattern arg] = [%e cnv_expr] in
                let [%p Fresh_name.pattern bnd] =
                  Sexplib0.Sexp.List
                    [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                    ; [%e Fresh_name.expression arg]
                    ]
                in
                ([%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
                 : _ Stdlib.List.t)
            in
            [%e expr]]
        in
        Lifted.return (patt, expr)
      | Sexp_or_null tp ->
        let v = Fresh_name.create "v" ~loc in
        let bnd = Fresh_name.create "bnd" ~loc in
        let arg = Fresh_name.create "arg" ~loc in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression v in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~renaming tp ~stackify) vname
        in
        let expr =
          [%expr
            let [%p Fresh_name.pattern bnds] =
              match [%e Fresh_name.expression fresh] with
              | Ppx_sexp_conv_lib.Or_null.Null -> [%e Fresh_name.expression bnds]
              | Ppx_sexp_conv_lib.Or_null.This [%p Fresh_name.pattern v] ->
                let [%p Fresh_name.pattern arg] = [%e cnv_expr] in
                let [%p Fresh_name.pattern bnd] =
                  Sexplib0.Sexp.List
                    [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                    ; [%e Fresh_name.expression arg]
                    ]
                in
                ([%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
                 : _ Stdlib.List.t)
            in
            [%e expr]]
        in
        Lifted.return (patt, expr)
      | Sexp_bool ->
        let patt = mk_rec_patt loc patt name fresh in
        let bnd = Fresh_name.create "bnd" ~loc in
        let expr =
          [%expr
            let [%p Fresh_name.pattern bnds] =
              if [%e Fresh_name.expression fresh]
              then (
                let [%p Fresh_name.pattern bnd] =
                  Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name] ]
                in
                ([%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
                 : _ Stdlib.List.t))
              else [%e Fresh_name.expression bnds]
            in
            [%e expr]]
        in
        Lifted.return (patt, expr)
      | Sexp_list tp ->
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          ~sexp_of:
            (* deliberately using whatever [sexp_of_list] is in scope *)
            (match stackify with
             | false -> [%expr sexp_of_list]
             | true -> [%expr sexp_of_list__stack])
          list_empty_expr
          ~stackify
        |> Lifted.return
      | Sexp_array tp ->
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          ~sexp_of:
            (* deliberately using whatever [sexp_of_array] is in scope *)
            (match stackify with
             | false -> [%expr sexp_of_array]
             | true -> [%expr sexp_of_array__stack])
          array_empty_expr
          ~stackify
        |> Lifted.return
      | Specific (Drop_default how) ->
        let tp = ld.pld_type in
        (match Attribute.get Attrs.default ld with
         | None -> Location.raise_errorf ~loc "no default to drop"
         | Some { to_lift = default } ->
           Record_field_attrs.lift_default ~loc ld default
           >>= sexp_of_default_field
                 ~types_being_defined
                 how
                 ~renaming
                 ~bnds
                 patt
                 expr
                 name
                 tp
                 ~stackify)
      | Specific (Drop_if test) ->
        test
        >>| fun test ->
        let tp = ld.pld_type in
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          (Inspect_value (fun loc expr -> [%expr [%e test] [%e expr]]))
          ~stackify
      | Omit_nil ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression fresh in
        let arg = Fresh_name.create "arg" ~loc in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~renaming tp ~stackify) vname
        in
        let bnds_expr =
          [%expr
            match [%e cnv_expr] with
            | Sexplib0.Sexp.List [] -> [%e Fresh_name.expression bnds]
            | [%p Fresh_name.pattern arg] ->
              (Sexplib0.Sexp.List
                 [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                 ; [%e Fresh_name.expression arg]
                 ]
               :: [%e Fresh_name.expression bnds]
               : _ Stdlib.List.t)]
        in
        ( patt
        , [%expr
            let [%p Fresh_name.pattern bnds] = [%e bnds_expr] in
            [%e expr]] )
        |> Lifted.return
      | Specific Keep ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression fresh in
        let arg = Fresh_name.create "arg" ~loc in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~renaming tp ~stackify) vname
        in
        let bnds_expr =
          [%expr
            let [%p Fresh_name.pattern arg] = [%e cnv_expr] in
            (Sexplib0.Sexp.List
               [ Sexplib0.Sexp.Atom [%e estring ~loc name]
               ; [%e Fresh_name.expression arg]
               ]
             :: [%e Fresh_name.expression bnds]
             : _ Stdlib.List.t)]
        in
        ( patt
        , [%expr
            let [%p Fresh_name.pattern bnds] = [%e bnds_expr] in
            [%e expr]] )
        |> Lifted.return
    in
    let init_expr = wrap_expr (Fresh_name.expression bnds) in
    List.fold_left ~f:coll ~init:(Lifted.return ([], init_expr)) flds
    >>| fun (patt, expr) ->
    ( (if unboxed
       then Ppxlib_jane.Ast_builder.Default.ppat_record_unboxed_product ?attrs:None
       else ppat_record)
        ~loc
        patt
        Closed
    , [%expr
        let [%p Fresh_name.pattern bnds] = ([] : _ Stdlib.List.t) in
        [%e expr]] )
  ;;

  (* Conversion of sum types *)

  let branch_sum
    row
    inline_attr
    ~types_being_defined
    renaming
    ~loc
    constr_lid
    constr_str
    args
    ~stackify
    =
    match args with
    | Pcstr_record lds ->
      let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
      sexp_of_label_declaration_list
        ~types_being_defined
        ~renaming
        loc
        lds
        ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List ([%e cnstr_expr] :: [%e expr])])
        ~stackify
        ~unboxed:false
      >>| fun (patt, expr) -> ppat_construct ~loc constr_lid (Some patt) --> expr
    | Pcstr_tuple pcd_args ->
      (match pcd_args with
       | [] ->
         ppat_construct ~loc constr_lid None
         --> [%expr Sexplib0.Sexp.Atom [%e constr_str]]
         |> Lifted.return
       | args ->
         let args = List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type in
         (match args with
          | [ tp ] when Option.is_some (Attribute.get inline_attr row) ->
            (match tp with
             | [%type: [%t? tp] list] ->
               let cnv_expr =
                 Conversion.to_expression
                   ~loc
                   (sexp_of_type ~renaming tp ~stackify)
                   ~stackify
               in
               let name = Fresh_name.create "l" ~loc in
               ppat_construct ~loc constr_lid (Some (Fresh_name.pattern name))
               --> [%expr
                     Sexplib0.Sexp.List
                       (Sexplib0.Sexp.Atom [%e constr_str]
                        :: [%e list_map ~loc ~stackify]
                             [%e cnv_expr]
                             [%e Fresh_name.expression name])]
             | _ -> Attrs.invalid_attribute ~loc inline_attr "_ list")
          | [ [%type: [%t? tp] sexp_list] ] ->
            let cnv_expr =
              Conversion.to_expression
                ~loc
                (sexp_of_type ~renaming tp ~stackify)
                ~stackify
            in
            let name = Fresh_name.create "l" ~loc in
            ppat_construct ~loc constr_lid (Some (Fresh_name.pattern name))
            --> [%expr
                  Sexplib0.Sexp.List
                    (Sexplib0.Sexp.Atom [%e constr_str]
                     :: [%e list_map ~loc ~stackify]
                          [%e cnv_expr]
                          [%e Fresh_name.expression name])]
          | _ ->
            let sexp_of_args = List.map ~f:(sexp_of_type ~renaming ~stackify) args in
            let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
            let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
              Conversion.apply_all ~loc sexp_of_args
            in
            let patt =
              match arguments with
              | [ arg ] -> arg
              | _ -> Ppxlib.Ast_builder.Default.ppat_tuple ~loc arguments
            in
            ppat_construct ~loc constr_lid (Some patt)
            --> pexp_let
                  ~loc
                  Immutable
                  Nonrecursive
                  bindings
                  [%expr Sexplib0.Sexp.List [%e elist ~loc (cnstr_expr :: converted)]])
         |> Lifted.return)
  ;;

  let sexp_of_sum ~types_being_defined ~renaming tps cds ~stackify =
    List.map cds ~f:(fun cd ->
      let renaming =
        Renaming.with_constructor_declaration renaming ~type_parameters:tps cd
      in
      let constr_lid = Located.map lident cd.pcd_name in
      let constr_str = estring ~loc:cd.pcd_name.loc cd.pcd_name.txt in
      branch_sum
        cd
        Attrs.list_variant
        ~types_being_defined
        renaming
        ~loc:cd.pcd_loc
        constr_lid
        constr_str
        cd.pcd_args
        ~stackify)
    |> Lifted.all
    >>| Conversion.of_lambda
  ;;

  (* Empty type *)
  let sexp_of_nil loc = Conversion.of_lambda [ ppat_any ~loc --> [%expr assert false] ]

  (* Generate code from type definitions *)

  let sexp_of_td ~types_being_defined td ~stackify ~portable =
    let td = name_type_params_in_td td in
    let tps =
      List.filter td.ptype_params ~f:(fun (p, _) -> include_param_in_combinator p)
      |> List.map ~f:Ppxlib_jane.get_type_param_name_and_jkind
    in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let renaming = Renaming.of_type_declaration td ~prefix:"_of_" in
    let body =
      let body =
        match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
        | Ptype_variant cds ->
          sexp_of_sum
            ~renaming
            ~types_being_defined
            (List.map tps ~f:(fun (x, _) -> x.txt))
            cds
            ~stackify
        | Ptype_record lds ->
          sexp_of_label_declaration_list
            ~renaming
            loc
            lds
            ~types_being_defined
            ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List [%e expr]])
            ~stackify
            ~unboxed:false
          >>| fun (patt, expr) -> Conversion.of_lambda [ patt --> expr ]
        | Ptype_record_unboxed_product lds ->
          sexp_of_label_declaration_list
            ~renaming
            loc
            lds
            ~types_being_defined
            ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List [%e expr]])
            ~stackify
            ~unboxed:true
          >>| fun (patt, expr) -> Conversion.of_lambda [ patt --> expr ]
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          (match td.ptype_manifest with
           | None -> sexp_of_nil loc
           | Some ty -> sexp_of_type ~renaming ty ~stackify)
          |> Lifted.return
      in
      body
      >>| fun body ->
      let is_private_alias =
        match td.ptype_kind, td.ptype_manifest, td.ptype_private with
        | Ptype_abstract, Some _, Private -> true
        | _ -> false
      in
      if is_private_alias
      then (
        (* Replace all type variable by _ to avoid generalization problems *)
        let ty_src =
          core_type_of_type_declaration td |> replace_variables_by_underscores
        in
        let manifest =
          match td.ptype_manifest with
          | Some manifest -> manifest
          | None -> Location.raise_errorf ~loc "sexp_of_td/no-manifest"
        in
        let ty_dst = replace_variables_by_underscores manifest in
        let v = Fresh_name.create "v" ~loc in
        let coercion =
          [%expr ([%e Fresh_name.expression v] : [%t ty_src] :> [%t ty_dst])]
        in
        let arg = Fresh_name.pattern v in
        let body = Conversion.apply ~loc body coercion in
        let body =
          match stackify with
          | false -> body
          | true -> [%expr exclave_ [%e body]]
        in
        [%expr fun [%p arg] -> [%e body]])
      else
        (* Prevent violation of value restriction, problems with recursive types, and
           top-level effects by eta-expanding function definitions *)
        Conversion.to_value_expression
          ~loc
          ~rec_flag:(Types_being_defined.to_rec_flag types_being_defined)
          ~values_being_defined:
            (Types_being_defined.to_values_being_defined types_being_defined ~stackify)
          body
          ~stackify
    in
    let typ = Sig_generate_sexp_of.mk_type td ~stackify in
    let func_name = sexp_of_typename ~stackify ~prefix:"" type_name in
    let body =
      body
      >>| fun body ->
      let patts =
        List.map tps ~f:(fun (id, _) ->
          match Renaming.binding_kind renaming id.txt ~loc:id.loc with
          | Universally_bound name -> Fresh_name.pattern name
          | Existentially_bound -> assert false)
      in
      let rec_flag = Types_being_defined.to_rec_flag types_being_defined in
      eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc patts body)
    in
    let body = Lifted.let_bind_user_expressions ~loc body in
    let sexp_of =
      let ({ body = typ; vars = _; loc = _ } : Ppx_helpers.Polytype.t) = typ in
      constrained_function_binding loc td typ ~tps ~func_name ~portable body
    in
    sexp_of, func_name
  ;;

  let sexp_of_tds ~loc ~path:_ ~unboxed (rec_flag, tds) ~stackify ~portable =
    let rec_flag = really_recursive_respecting_opaque rec_flag tds in
    let tds = Ppx_helpers.with_implicit_unboxed_records ~unboxed tds in
    let (types_being_defined : Types_being_defined.t) =
      match rec_flag with
      | Nonrecursive -> Nonrec
      | Recursive ->
        Rec
          (String.Set.of_list
             (List.map tds ~f:(fun td -> Ppx_helpers.mangle_unboxed td.ptype_name.txt)))
    in
    let stackify =
      match stackify with
      | false -> [ false ]
      | true -> [ false; true ]
    in
    let bindings_and_names =
      List.map stackify ~f:(fun stackify ->
        let bindings_and_names =
          List.map tds ~f:(sexp_of_td ~types_being_defined ~stackify ~portable)
        in
        let bindings = List.map bindings_and_names ~f:fst in
        let names = List.map bindings_and_names ~f:snd in
        pstr_value_list ~loc rec_flag bindings, names)
    in
    let bindings = List.concat_map bindings_and_names ~f:fst in
    let names = List.concat_map bindings_and_names ~f:snd in
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

  let sexp_of_exn ~loc:_ ~path ec =
    let renaming = Renaming.without_type () in
    let get_full_cnstr str = path ^ "." ^ str in
    let loc = ec.ptyexn_loc in
    let expr =
      match ec.ptyexn_constructor with
      | { pext_name = cnstr
        ; pext_kind = Pext_decl (_, extension_constructor_kind, None)
        ; _
        } ->
        let constr_lid = Located.map lident cnstr in
        branch_sum
          ec
          Attrs.list_exception
          ~types_being_defined:Nonrec
          renaming
          ~loc
          constr_lid
          (estring ~loc (get_full_cnstr cnstr.txt))
          extension_constructor_kind
          ~stackify:false
        >>| fun converter ->
        let assert_false = ppat_any ~loc --> [%expr assert false] in
        [%expr
          Sexplib0.Sexp_conv.Exn_converter.add
            [%extension_constructor [%e pexp_construct ~loc constr_lid None]]
            [%e
              Conversion.to_expression
                ~loc
                (Conversion.of_lambda [ converter; assert_false ])
                ~stackify:false]]
      | { pext_kind = Pext_decl (_, _, Some _); _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/:"
      | { pext_kind = Pext_rebind _; _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/rebind"
    in
    let expr = Lifted.let_bind_user_expressions ~loc expr in
    [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? ()] ~expr ~modes:[] ]
    ]
  ;;

  let sexp_of_core_type core_type ~stackify =
    let loc = { core_type.ptyp_loc with loc_ghost = true } in
    sexp_of_type ~renaming:(Renaming.without_type ()) core_type ~stackify
    |> Conversion.to_value_expression
         ~loc
         ~rec_flag:Nonrecursive
         ~values_being_defined:String.Set.empty
         ~stackify
    |> Merlin_helpers.hide_expression
  ;;
end
