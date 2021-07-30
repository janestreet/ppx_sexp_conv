open! Base
open! Ppxlib
open Ast_builder.Default
open Helpers

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct
  let type_of_sexp_of ~loc t =
    let loc = { loc with loc_ghost = true } in
    [%type: [%t t] -> Sexplib0.Sexp.t]
  ;;

  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_sexp_of

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (( ^ ) "sexp_of_") td.ptype_name)
           ~type_:(mk_type td)
           ~prim:[]))
  ;;

  let mk_sig_exn ~loc:_ ~path:_ _te = []
end

module Str_generate_sexp_of = struct
  (* Handling of record defaults *)

  let sexp_of_type_constr ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "sexp_of_" ^ s) args
  ;;

  (* Conversion of types *)
  let rec sexp_of_type
            ~(typevar_handling : [ `ok of Renaming.t | `disallowed_in_type_expr ])
            typ
    : Conversion.t
    =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Conversion.of_expression [%expr Sexplib0.Sexp_conv.sexp_of_opaque]
    | [%type: _] -> Conversion.of_expression [%expr fun _ -> Sexplib0.Sexp.Atom "_"]
    | [%type: [%t? _] sexp_opaque] ->
      Conversion.of_expression [%expr Sexplib0.Sexp_conv.sexp_of_opaque]
    | { ptyp_desc = Ptyp_tuple tp; _ } ->
      Conversion.of_cases [ sexp_of_tuple ~typevar_handling (loc, tp) ]
    | { ptyp_desc = Ptyp_var parm; _ } ->
      (match typevar_handling with
       | `disallowed_in_type_expr ->
         Location.raise_errorf
           ~loc
           "Type variables not allowed in [%%sexp_of: ]. Please use locally abstract \
            types instead."
       | `ok renaming ->
         (match Renaming.binding_kind renaming parm with
          | Universally_bound parm -> Conversion.of_expression (evar ~loc ("_of_" ^ parm))
          | Existentially_bound -> sexp_of_type ~typevar_handling [%type: _]))
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      Conversion.of_expression
        (sexp_of_type_constr
           ~loc
           id
           (List.map args ~f:(fun tp ->
              Conversion.to_expression ~loc (sexp_of_type ~typevar_handling tp))))
    | { ptyp_desc = Ptyp_arrow (_, _, _); _ } ->
      Conversion.of_expression [%expr fun _f -> Sexplib0.Sexp_conv.(sexp_of_fun ignore)]
    | { ptyp_desc = Ptyp_variant (row_fields, Closed, _); _ } ->
      sexp_of_variant ~typevar_handling (loc, row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      sexp_of_poly ~typevar_handling parms poly_tp
    | { ptyp_desc = Ptyp_variant (_, Open, _); _ }
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ } ->
      Location.raise_errorf ~loc "Type unsupported for ppx [sexp_of] conversion"

  (* Conversion of tuples *)
  and sexp_of_tuple ~typevar_handling (loc, tps) =
    let fps = List.map ~f:(fun tp -> sexp_of_type ~typevar_handling tp) tps in
    let bindings, pvars, evars = Conversion.map_tmp_vars ~loc fps in
    let in_expr = [%expr Sexplib0.Sexp.List [%e elist ~loc evars]] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc pvars --> expr

  (* Conversion of variant types *)
  and sexp_of_variant ~typevar_handling ((loc, row_fields) : Location.t * row_field list)
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
             Conversion.to_expression ~loc (sexp_of_type ~typevar_handling tp)
           in
           ppat_variant ~loc cnstr (Some [%pat? l])
           --> [%expr
             Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
                :: Sexplib0.Sexp_conv.list_map [%e cnv_expr] l)]
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list")
      | Rtag ({ txt = cnstr; _ }, _, [ [%type: [%t? tp] sexp_list] ]) ->
        let cnv_expr =
          Conversion.to_expression ~loc (sexp_of_type ~typevar_handling tp)
        in
        ppat_variant ~loc cnstr (Some [%pat? l])
        --> [%expr
          Sexplib0.Sexp.List
            (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
             :: Sexplib0.Sexp_conv.list_map [%e cnv_expr] l)]
      | Rtag ({ txt = cnstr; _ }, false, [ tp ]) ->
        let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e estring ~loc cnstr]] in
        let var, patt = evar ~loc "v0", pvar ~loc "v0" in
        let cnstr_arg = Conversion.apply ~loc (sexp_of_type ~typevar_handling tp) var in
        let expr = [%expr Sexplib0.Sexp.List [%e elist ~loc [ cnstr_expr; cnstr_arg ]]] in
        ppat_variant ~loc cnstr (Some patt) --> expr
      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc)
        --> sexp_of_type_constr ~loc id [ [%expr v] ]
      | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rtag/&"
      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _ :: _); _ } as typ) ->
        let call = Conversion.to_expression ~loc (sexp_of_type ~typevar_handling typ) in
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc) --> [%expr [%e call] v]
      | Rinherit _ ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rinherit/non-id"
      (* impossible?*)
      | Rtag (_, false, []) -> assert false
    in
    Conversion.of_cases (List.map ~f:item row_fields)

  (* Polymorphic record fields *)
  and sexp_of_poly ~typevar_handling parms tp =
    let loc = tp.ptyp_loc in
    match typevar_handling with
    | `disallowed_in_type_expr ->
      (* Should be impossible because [sexp_of_poly] is only called on polymorphic record
         fields and record type definitions can't occur in type expressions. *)
      Location.raise_errorf ~loc "polymorphic type in a type expression"
    | `ok renaming ->
      let bindings =
        let mk_binding parm =
          value_binding
            ~loc
            ~pat:(pvar ~loc ("_of_" ^ parm.txt))
            ~expr:[%expr Sexplib0.Sexp_conv.sexp_of_opaque]
        in
        List.map ~f:mk_binding parms
      in
      let renaming =
        List.fold_left parms ~init:renaming ~f:Renaming.add_universally_bound
      in
      Conversion.bind
        (sexp_of_type ~typevar_handling:(`ok renaming) tp)
        ~loc
        ~var:"arg"
        Nonrecursive
        bindings
  ;;

  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p = Loc.make (Longident.Lident name) ~loc, pvar ~loc ("v_" ^ name) in
    patt @ [ p ]
  ;;

  type is_empty_expr =
    | Inspect_value of (location -> expression -> expression)
    | Inspect_sexp of (cnv_expr:expression -> location -> expression -> expression)

  let sexp_of_record_field ~renaming patt expr name tp ?sexp_of is_empty_expr =
    let loc = tp.ptyp_loc in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr =
      Conversion.to_expression ~loc (sexp_of_type ~typevar_handling:(`ok renaming) tp)
    in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> [%expr [%e sexp_of] [%e cnv_expr]]
    in
    let expr =
      let v_name = [%expr [%e "v_" ^ name]] in
      [%expr
        let bnds =
          [%e
            match is_empty_expr with
            | Inspect_value is_empty_expr ->
              [%expr
                if [%e is_empty_expr loc (evar ~loc v_name)]
                then bnds
                else (
                  let arg = [%e cnv_expr] [%e evar ~loc v_name] in
                  let bnd =
                    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name]; arg ]
                  in
                  bnd :: bnds)]
            | Inspect_sexp is_empty_expr ->
              [%expr
                let arg = [%e cnv_expr] [%e evar ~loc v_name] in
                if [%e is_empty_expr ~cnv_expr loc [%expr arg]]
                then bnds
                else (
                  let bnd =
                    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name]; arg ]
                  in
                  bnd :: bnds)]]
        in
        [%e expr]]
    in
    patt, expr
  ;;

  let disallow_type_variables_and_recursive_occurrences ~types_being_defined ~loc ~why tp =
    let disallow_variables =
      let iter =
        object
          inherit Ast_traverse.iter as super

          method! core_type_desc =
            function
            | Ptyp_var v ->
              Location.raise_errorf
                ~loc
                "[@sexp_drop_default.%s] was used, but the type of the field contains a \
                 type variable: '%s.\n\
                 Comparison is not avaiable for type variables.\n\
                 Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] instead."
                (match why with
                 | `compare -> "compare"
                 | `equal -> "equal")
                v
            | t -> super#core_type_desc t
        end
      in
      iter#core_type
    in
    let disallow_recursive_occurrences =
      match types_being_defined with
      | `Nonrecursive -> fun _ -> ()
      | `Recursive types_being_defined ->
        let iter =
          object
            inherit Ast_traverse.iter as super

            method! core_type_desc =
              function
              | Ptyp_constr ({ loc = _; txt = Lident s }, _) as t ->
                if Set.mem types_being_defined s
                then
                  Location.raise_errorf
                    ~loc
                    "[@sexp_drop_default.%s] was used, but the type of the field \
                     contains a type defined in the current recursive block: %s.\n\
                     This is not supported.\n\
                     Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] \
                     instead."
                    (match why with
                     | `compare -> "compare"
                     | `equal -> "equal")
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
        patt
        expr
        name
        tp
        ?sexp_of
        default
    =
    let is_empty =
      match how with
      | `sexp ->
        Inspect_sexp
          (fun ~cnv_expr loc sexp_expr ->
             [%expr Sexplib0.Sexp_conv.( = ) ([%e cnv_expr] [%e default]) [%e sexp_expr]])
      | (`no_arg | `func _ | `compare | `equal) as how ->
        let equality_f loc =
          match how with
          | `no_arg ->
            [%expr
              Sexplib0.Sexp_conv.( = ) [@ocaml.ppwarning
                "[@sexp_drop_default] is deprecated: please use \
                 one of:\n\
                 - [@sexp_drop_default f] and give an explicit \
                 equality function ([f = Poly.(=)] corresponds \
                 to the old behavior)\n\
                 - [@sexp_drop_default.compare] if the type \
                 supports [%compare]\n\
                 - [@sexp_drop_default.equal] if the type \
                 supports [%equal]\n\
                 - [@sexp_drop_default.sexp] if you want to \
                 compare the sexp representations\n"]]
          | `func f ->
            f
          | `compare ->
            disallow_type_variables_and_recursive_occurrences
              ~types_being_defined
              ~why:`compare
              ~loc
              tp;
            [%expr [%compare.equal: [%t tp]]]
          | `equal ->
            disallow_type_variables_and_recursive_occurrences
              ~types_being_defined
              ~why:`equal
              ~loc
              tp;
            [%expr [%equal: [%t tp]]]
        in
        Inspect_value (fun loc expr -> [%expr [%e equality_f loc] [%e default] [%e expr]])
    in
    sexp_of_record_field ~renaming patt expr name tp ?sexp_of is_empty
  ;;

  let sexp_of_label_declaration_list ~types_being_defined ~renaming loc flds ~wrap_expr =
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
    let coll ((patt : (Longident.t loc * pattern) list), expr) ld =
      let name = ld.pld_name.txt in
      let loc = ld.pld_name.loc in
      match Attrs.Record_field_handler.Sexp_of.create ~loc ld with
      | `sexp_option tp ->
        let patt = mk_rec_patt loc patt name in
        let vname = [%expr v] in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~typevar_handling:(`ok renaming) tp) vname
        in
        let expr =
          [%expr
            let bnds =
              match [%e evar ~loc ("v_" ^ name)] with
              | Stdlib.Option.None -> bnds
              | Stdlib.Option.Some v ->
                let arg = [%e cnv_expr] in
                let bnd =
                  Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name]; arg ]
                in
                bnd :: bnds
            in
            [%e expr]]
        in
        patt, expr
      | `sexp_bool ->
        let patt = mk_rec_patt loc patt name in
        let expr =
          [%expr
            let bnds =
              if [%e evar ~loc ("v_" ^ name)]
              then (
                let bnd =
                  Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name] ]
                in
                bnd :: bnds)
              else bnds
            in
            [%e expr]]
        in
        patt, expr
      | `sexp_list tp ->
        sexp_of_record_field
          ~renaming
          patt
          expr
          name
          tp
          ~sexp_of:[%expr sexp_of_list]
          list_empty_expr
      | `sexp_array tp ->
        sexp_of_record_field
          ~renaming
          patt
          expr
          name
          tp
          ~sexp_of:[%expr sexp_of_array]
          array_empty_expr
      | `drop_default how ->
        let tp = ld.pld_type in
        (match Attribute.get Attrs.default ld with
         | None -> Location.raise_errorf ~loc "no default to drop"
         | Some default ->
           sexp_of_default_field
             ~types_being_defined
             how
             ~renaming
             patt
             expr
             name
             tp
             default)
      | `drop_if test ->
        let tp = ld.pld_type in
        sexp_of_record_field
          ~renaming
          patt
          expr
          name
          tp
          (Inspect_value (fun loc expr -> [%expr [%e test] [%e expr]]))
      | (`omit_nil | `keep) as test ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name in
        let vname = evar ~loc ("v_" ^ name) in
        let cnv_expr =
          Conversion.apply ~loc (sexp_of_type ~typevar_handling:(`ok renaming) tp) vname
        in
        let bnds =
          match test with
          | `keep ->
            [%expr
              let arg = [%e cnv_expr] in
              Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name]; arg ]
              :: bnds]
          | `omit_nil ->
            [%expr
              match [%e cnv_expr] with
              | Sexplib0.Sexp.List [] -> bnds
              | arg ->
                Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name]; arg ]
                :: bnds]
        in
        ( patt
        , [%expr
          let bnds = [%e bnds] in
          [%e expr]] )
    in
    let init_expr = wrap_expr [%expr bnds] in
    let patt, expr = List.fold_left ~f:coll ~init:([], init_expr) flds in
    ( ppat_record ~loc patt Closed
    , [%expr
      let bnds = [] in
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
    =
    match args with
    | Pcstr_record lds ->
      let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
      let patt, expr =
        (* Uncomment to wrap record *)
        (* sexp_of_label_declaration_list loc lds
         *   ~wrap_expr:(fun expr ->
         *     [%expr Sexplib0.Sexp.List [ [%e cnstr_expr];
         *                                Sexplib0.Sexp.List [%e expr]
         *                              ]
         *     ]) *)
        sexp_of_label_declaration_list
          ~types_being_defined
          ~renaming
          loc
          lds
          ~wrap_expr:(fun expr ->
            [%expr Sexplib0.Sexp.List ([%e cnstr_expr] :: [%e expr])])
      in
      ppat_construct ~loc constr_lid (Some patt) --> expr
    | Pcstr_tuple pcd_args ->
      (match pcd_args with
       | [] ->
         ppat_construct ~loc constr_lid None --> [%expr Sexplib0.Sexp.Atom [%e constr_str]]
       | args ->
         (match args with
          | [ tp ] when Option.is_some (Attribute.get inline_attr row) ->
            (match tp with
             | [%type: [%t? tp] list] ->
               let cnv_expr =
                 Conversion.to_expression
                   ~loc
                   (sexp_of_type ~typevar_handling:(`ok renaming) tp)
               in
               ppat_construct ~loc constr_lid (Some [%pat? l])
               --> [%expr
                 Sexplib0.Sexp.List
                   (Sexplib0.Sexp.Atom [%e constr_str]
                    :: Sexplib0.Sexp_conv.list_map [%e cnv_expr] l)]
             | _ -> Attrs.invalid_attribute ~loc inline_attr "_ list")
          | [ [%type: [%t? tp] sexp_list] ] ->
            let cnv_expr =
              Conversion.to_expression
                ~loc
                (sexp_of_type ~typevar_handling:(`ok renaming) tp)
            in
            ppat_construct ~loc constr_lid (Some [%pat? l])
            --> [%expr
              Sexplib0.Sexp.List
                (Sexplib0.Sexp.Atom [%e constr_str]
                 :: Sexplib0.Sexp_conv.list_map [%e cnv_expr] l)]
          | _ ->
            let sexp_of_args =
              List.map ~f:(sexp_of_type ~typevar_handling:(`ok renaming)) args
            in
            let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
            let bindings, patts, vars = Conversion.map_tmp_vars ~loc sexp_of_args in
            let patt =
              match patts with
              | [ patt ] -> patt
              | _ -> ppat_tuple ~loc patts
            in
            ppat_construct ~loc constr_lid (Some patt)
            --> pexp_let
                  ~loc
                  Nonrecursive
                  bindings
                  [%expr Sexplib0.Sexp.List [%e elist ~loc (cnstr_expr :: vars)]]))
  ;;

  let sexp_of_sum ~types_being_defined tps cds =
    Conversion.of_cases
      (List.map cds ~f:(fun cd ->
         let renaming = Renaming.of_gadt tps cd in
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
           cd.pcd_args))
  ;;

  (* Empty type *)
  let sexp_of_nil loc = Conversion.of_expression [%expr fun _v -> assert false]

  (* Generate code from type definitions *)

  let sexp_of_td ~types_being_defined td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant cds ->
          sexp_of_sum ~types_being_defined (List.map tps ~f:(fun x -> x.txt)) cds
        | Ptype_record lds ->
          let renaming = Renaming.identity in
          let patt, expr =
            sexp_of_label_declaration_list
              ~renaming
              loc
              lds
              ~types_being_defined
              ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List [%e expr]])
          in
          Conversion.of_cases [ patt --> expr ]
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          (match td.ptype_manifest with
           | None -> sexp_of_nil loc
           | Some ty -> sexp_of_type ~typevar_handling:(`ok Renaming.identity) ty)
      in
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
        let coercion = [%expr (v : [%t ty_src] :> [%t ty_dst])] in
        [%expr fun v -> [%e Conversion.apply ~loc body coercion]])
      else
        (* Prevent violation of value restriction and problems with recursive types by
           eta-expanding function definitions *)
        Conversion.to_lambda_expression ~loc ~var:"v" body
    in
    let typ = Sig_generate_sexp_of.mk_type td in
    let func_name = "sexp_of_" ^ type_name in
    let body =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id.txt)) in
      let rec_flag =
        match types_being_defined with
        | `Recursive _ -> Recursive
        | `Nonrecursive -> Nonrecursive
      in
      eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc patts body)
    in
    [ constrained_function_binding loc td typ ~tps ~func_name body ]
  ;;

  let sexp_of_tds ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let types_being_defined =
      match rec_flag with
      | Nonrecursive -> `Nonrecursive
      | Recursive ->
        `Recursive
          (Set.of_list (module String) (List.map tds ~f:(fun td -> td.ptype_name.txt)))
    in
    let bindings = List.concat_map tds ~f:(sexp_of_td ~types_being_defined) in
    pstr_value_list ~loc rec_flag bindings
  ;;

  let sexp_of_exn ~types_being_defined ~loc:_ ~path ec =
    let renaming = Renaming.identity in
    let get_full_cnstr str = path ^ "." ^ str in
    let loc = ec.ptyexn_loc in
    let expr =
      match ec.ptyexn_constructor with
      | { pext_name = cnstr; pext_kind = Pext_decl (extension_constructor_kind, None); _ }
        ->
        let constr_lid = Located.map lident cnstr in
        let converter =
          branch_sum
            ec
            Attrs.list_exception
            ~types_being_defined
            renaming
            ~loc
            constr_lid
            (estring ~loc (get_full_cnstr cnstr.txt))
            extension_constructor_kind
        in
        let assert_false = ppat_any ~loc --> [%expr assert false] in
        [%expr
          Sexplib0.Sexp_conv.Exn_converter.add
            [%extension_constructor [%e pexp_construct ~loc constr_lid None]]
            [%e
              Conversion.to_expression
                ~loc
                (Conversion.of_cases [ converter; assert_false ])]]
      | { pext_kind = Pext_decl (_, Some _); _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/:"
      | { pext_kind = Pext_rebind _; _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/rebind"
    in
    [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? ()] ~expr ] ]
  ;;
end
