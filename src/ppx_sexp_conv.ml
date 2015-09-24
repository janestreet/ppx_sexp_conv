(* sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default
open Ppx_sexp_conv_expander.Internal

[@@@metaloc loc]

module Type_conv = Ppx_type_conv.Std.Type_conv

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

let record_default =
  Attribute.declare "sexp.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)

let drop_default =
  Attribute.declare "sexp.sexp_drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let drop_if =
  Attribute.declare "sexp.sexp_drop_if"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)

(* Utility functions *)

let name_type_params_in_td (td : type_declaration) : type_declaration =
  let name_param (tp, variance) =
    let ptyp_desc =
      match tp.ptyp_desc with
      | Ptyp_any -> Ptyp_var ("v" ^ gen_symbol ())
      | Ptyp_var _ as v -> v
      | _ -> Location.raise_errorf ~loc:tp.ptyp_loc
               "ppx_sexp_conv: not a type parameter"
    in
    ({ tp with ptyp_desc }, variance)
  in
  { td with ptype_params = List.map td.ptype_params ~f:name_param }

let make_type_rigid =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type ty =
      let ptyp_desc =
        match ty.ptyp_desc with
        | Ptyp_var s -> Ptyp_constr (Located.lident ~loc:ty.ptyp_loc s, [])
        | desc -> super#core_type_desc desc
      in
      { ty with ptyp_desc }
  end in
  map#core_type

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ] or
   [type a .. z. make_mono_type t (a .. z)] when [use_rigid_variables] is true.
   Annotation are needed for non regular recursive datatypes and gadt when the return type
   of constructors are constrained. Unfortunately, putting rigid variables everywhere does
   not work because of certains types with constraints. We thus only use rigid variables
   for sum types, which includes all GADTs. *)

let tvars_of_core_type : (core_type -> string list) =
  let tvars = object
    inherit [string list] Ast_traverse.fold as super
    method! core_type x acc =
      match x.ptyp_desc with
      | Ptyp_var x -> if List.mem x ~set:acc then acc else x :: acc
      | _ ->
        super#core_type x acc
  end
  in fun typ ->
    List.rev (tvars#core_type typ [])

let constrained_function_binding = fun
  (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
  (loc:Location.t) (td:type_declaration) (typ:core_type) ~(tps:string list)
  ~(func_name:string) (body:expression)
->
  let vars = tvars_of_core_type typ in
  let has_vars = match vars with [] -> false | _::_ -> true in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars then pat else
      ppat_constraint ~loc pat (ptyp_poly ~loc vars typ)
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with | Ptype_variant _ -> true | _ -> false
    in
    if use_rigid_variables
    then
      List.fold_right tps
        ~f:(pexp_newtype ~loc)
        ~init:(pexp_constraint ~loc body (make_type_rigid typ))
    else
      if has_vars
      then body
      else pexp_constraint ~loc body typ
  in
  value_binding ~loc ~pat ~expr:body

let sexp_type_is_recursive =
  types_are_recursive ~short_circuit:(fun typ ->
    match typ with
    | [%type: [%t? _] sexp_opaque ] -> Some false
    | _ -> None)

let really_recursive rec_flag tds =
  match rec_flag with
  | Recursive    -> if sexp_type_is_recursive tds then Recursive else Nonrecursive
  | Nonrecursive -> Nonrecursive

(* Generators for S-expressions *)

let combinator_type_of_td ~f td =
  let td = name_type_params_in_td td in
  let result_type = f ~loc:td.ptype_name.loc (core_type_of_type_declaration td) in
  List.fold_right
    td.ptype_params
    ~init:result_type
    ~f:(fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      [%type:  [%t f ~loc tp] -> [%t acc] ])

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct

  let mk_type td =
    combinator_type_of_td td ~f:(fun ~loc t -> [%type: [%t t] -> Sexplib.Sexp.t])

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value ~loc
        (value_description ~loc
           ~name:(Located.map ((^) "sexp_of_") td.ptype_name)
           ~type_:(mk_type td)
           ~prim:[]))
    |> Type_conv.Generator_result.make_at_the_end

  let gen = Type_conv.Generator.make Type_conv.Args.empty mk_sig

  let mk_sig_exn ~loc:_ ~path:_ _te = Type_conv.Generator_result.nil

  let gen_exn = Type_conv.Generator.make Type_conv.Args.empty mk_sig_exn
end

(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct

  let mk_type td =
    combinator_type_of_td td ~f:(fun ~loc t -> [%type: Sexplib.Sexp.t -> [%t t] ])

  let is_polymorphic_variant =
    let rec check = function
      | { ptyp_desc = Ptyp_variant _; _ } -> `Definitely
      | { ptyp_desc = Ptyp_alias (typ,_); _ } -> check typ
      | { ptyp_desc = (Ptyp_any | Ptyp_var _ | Ptyp_constr _); _ } -> `Maybe
      | _ -> `Surely_not
    in
    fun td ->
      match td.ptype_kind with
      | Ptype_variant _ | Ptype_record _ | Ptype_open -> `Surely_not
      | Ptype_abstract ->
        match td.ptype_manifest with
        | None -> `Maybe
        | Some typ -> check typ


  let sig_of_td with_poly td =
    let of_sexp_type = mk_type td in
    let loc = td.ptype_loc in
    let of_sexp_item =
      psig_value ~loc
        (value_description ~loc
           ~name:(Located.map (fun s -> s ^ "_of_sexp") td.ptype_name)
           ~type_:of_sexp_type
           ~prim:[])
    in
    match with_poly, is_polymorphic_variant td with
      | true, `Surely_not ->
        Location.raise_errorf ~loc
          "Sig_generate_of_sexp.sig_of_td: sexp_poly annotation \
           but type is surely not a polymorphic variant"
      | false, (`Surely_not | `Maybe) -> [of_sexp_item]
      | (true | false), `Definitely
      | true, `Maybe ->
        [ of_sexp_item
        ; psig_value ~loc
            (value_description ~loc
               ~name:(Located.map (fun s -> "__" ^ s ^ "_of_sexp__") td.ptype_name)
               ~type_:of_sexp_type
               ~prim:[])
        ]

  let mk_sig with_poly ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(sig_of_td with_poly)
    |> List.flatten
    |> Type_conv.Generator_result.make_at_the_end

  let gen      = Type_conv.Generator.make Type_conv.Args.empty (mk_sig false)
  let gen_poly = Type_conv.Generator.make Type_conv.Args.empty (mk_sig true )
end

(* Generator for converters of OCaml-values to S-expressions *)
module Generate_sexp_of = struct

  (* Handling of record defaults *)

  type record_field_handler = [ `keep | `drop_default | `drop_if of expression ]

  let get_record_field_handler ~loc ld : record_field_handler =
    match
      Attribute.get drop_default ld,
      Attribute.get drop_if ld
    with
    | None, None -> `keep
    | Some (), None -> `drop_default
    | None, Some e -> `drop_if e
    | Some (), Some _ ->
      Location.raise_errorf ~loc "sexp record field handler defined twice"

  (* Conversion of sum types *)

  let branch_sum tvars cds =
    List.map cds ~f:(fun cd ->
      let renaming = Renaming.of_gadt tvars cd in
      let loc = cd.pcd_loc in
      let cnstr = cd.pcd_name in
      let lid = Located.map lident cnstr in
      let str = estring ~loc cnstr.txt in
      match cd.pcd_args with
      | [] ->
        ppat_construct ~loc lid None --> [%expr Sexplib.Sexp.Atom [%e str]]
      | args ->
        match args with
        | [ [%type: [%t? tp] sexp_list ] ] ->
          let cnv_expr = Fun_or_match.expr ~loc (sexp_of_type renaming tp) in
          ppat_construct ~loc lid (Some [%pat? l]) -->
          [%expr
            Sexplib.Sexp.List
              (Sexplib.Sexp.Atom [%e str] ::
               Sexplib.Conv.list_map [%e cnv_expr] l)]
        | _ ->
          let sexp_of_args = List.map ~f:(sexp_of_type renaming) args in
          let cnstr_expr = [%expr Sexplib.Sexp.Atom [%e str] ] in
          let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc sexp_of_args in
          let patt =
            match patts with
            | [patt] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          ppat_construct ~loc lid (Some patt) -->
          pexp_let ~loc Nonrecursive bindings
            [%expr Sexplib.Sexp.List [%e elist ~loc (cnstr_expr :: vars)]]
    )

  let sexp_of_sum tps cds = Fun_or_match.Match (branch_sum tps cds)

  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p =
      Location.mkloc (Longident.Lident name) loc ,
      pvar ~loc ("v_" ^ name)
    in
    patt @ [p]

  let sexp_of_record_field patt expr name tp ?sexp_of is_empty_expr =
    let renaming = Renaming.identity in
    let loc = tp.ptyp_loc in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr = match (sexp_of_type renaming tp) with
      | Fun exp -> exp
      | Match matchings -> [%expr fun el -> [%e pexp_match ~loc [%expr el] matchings]]
    in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> [%expr  [%e sexp_of] [%e cnv_expr] ]
    in
    let expr =
      let v_name = [%expr  [%e  "v_" ^ name] ] in
      [%expr
        let bnds =
          if [%e is_empty_expr loc (evar ~loc v_name)] then bnds
          else
            let arg = [%e cnv_expr] [%e evar ~loc v_name] in
            let bnd =
              Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
            in
            bnd :: bnds
        in
        [%e expr]
        ]
    in
    patt, expr

  let sexp_of_default_field patt expr name tp ?sexp_of default =
    sexp_of_record_field patt expr name tp ?sexp_of
      (fun loc expr -> [%expr  Pervasives.(=) [%e default] [%e expr] ])

  let sexp_of_record (loc,flds) : Fun_or_match.t =
    let renaming = Renaming.identity in
    let list_empty_expr loc lst =
      [%expr
          match [%e lst] with
          | [] -> true
          | _ -> false ]
    in
    let array_empty_expr loc arr =
      [%expr
          match [%e arr] with
          | [||] -> true
          | _ -> false ]
    in
    let coll ((patt : (Longident.t loc * pattern) list), expr) = function
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_option]; _ } ->
        let patt = mk_rec_patt loc patt name in
        let vname = [%expr  v ] in
        let cnv_expr = Fun_or_match.unroll ~loc vname (sexp_of_type renaming tp) in
        let expr =
          [%expr
           let bnds =
             match [%e evar ~loc ("v_" ^ name)] with
             | None -> bnds
             | Some v ->
               let arg = [%e cnv_expr] in
               let bnd =
                 Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
               in
               bnd :: bnds
           in
           [%e expr]
          ]
        in
        patt, expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: sexp_bool]; _ } ->
        let patt = mk_rec_patt loc patt name in
        let expr =
          [%expr
           let bnds =
             if [%e evar ~loc ("v_" ^ name)] then
               let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]] in
               bnd :: bnds
             else bnds
           in
           [%e expr]
          ]
        in
        patt, expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_list]; _ } ->
        sexp_of_record_field patt expr name tp
          ~sexp_of:[%expr  sexp_of_list ] list_empty_expr
      | {pld_name = {txt=name; loc};
         pld_type = [%type: [%t? tp] sexp_array]; _ } ->
        sexp_of_record_field patt expr name tp
          ~sexp_of:[%expr  sexp_of_array ] array_empty_expr
      | {pld_name = {txt=name; loc}; pld_type = tp; _ } as ld ->
        begin match get_record_field_handler ~loc ld with
        | `drop_default -> begin
            match Attribute.get record_default ld with
            | None ->
              Location.raise_errorf ~loc "no default to drop"
            | Some default ->
              sexp_of_default_field patt expr name tp default
          end
        | `drop_if test ->
          sexp_of_record_field patt expr name tp
            (fun loc expr -> [%expr [%e test] [%e expr]])
        | `keep ->
          let patt = mk_rec_patt loc patt name in
          let vname = evar ~loc ("v_" ^ name) in
          let cnv_expr = Fun_or_match.unroll ~loc vname (sexp_of_type renaming tp) in
          let expr =
            [%expr
             let arg = [%e cnv_expr] in
             let bnd =
               Sexplib.Sexp.List [Sexplib.Sexp.Atom [%e estring ~loc name]; arg]
             in
             let bnds = bnd :: bnds in
             [%e expr]
            ]
          in
          patt, expr
        end
    in
    let init_expr = [%expr  Sexplib.Sexp.List bnds ] in
    let patt, expr =
      List.fold_left ~f:coll ~init:([], init_expr) flds
    in
    Match
      [ ppat_record ~loc patt Closed --> [%expr let bnds = [] in [%e expr]]
      ]

  (* Empty type *)
  let sexp_of_nil loc = Fun_or_match.Fun [%expr  fun _v -> assert false ]

  (* Generate code from type definitions *)

  let sexp_of_td td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    let {ptype_name = {txt = type_name; loc = _}; ptype_loc = loc; _} = td in
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant cds -> sexp_of_sum tps cds
        | Ptype_record  lds -> sexp_of_record (loc,lds)
        | Ptype_open -> Location.raise_errorf ~loc
                          "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          match td.ptype_manifest with
          | None    -> sexp_of_nil loc
          | Some { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
            sexp_of_variant Renaming.identity (loc,row_fields)
          | Some ty -> sexp_of_type Renaming.identity ty
      in
      let is_private_alias =
        match td.ptype_kind, td.ptype_manifest, td.ptype_private with
        | Ptype_abstract, Some _, Private -> true
        | _ -> false
      in
      if is_private_alias then
        (* Replace all type variable by _ to avoid generalization problems *)
        let ty_src =
          core_type_of_type_declaration td
          |> replace_variables_by_underscores
        in
        let manifest =
          match td.ptype_manifest with
          | Some manifest -> manifest
          | None -> Location.raise_errorf ~loc "sexp_of_td/no-manifest"
        in
        let ty_dst = replace_variables_by_underscores manifest in
        let coercion = [%expr  (v : [%t ty_src] :> [%t ty_dst]) ] in
        match body with
        | Fun fun_expr ->
          [%expr  fun v -> [%e fun_expr] [%e coercion] ]
        | Match matchings ->
          [%expr  fun v -> [%e pexp_match ~loc coercion matchings]]
      else
        match body with
          (* Prevent violation of value restriction and problems with recursive types by
             eta-expanding function definitions *)
        | Fun fun_expr -> [%expr fun v -> [%e fun_expr] v ]
        | Match matchings -> pexp_function ~loc matchings
    in
    let typ = Sig_generate_sexp_of.mk_type td in
    let func_name = "sexp_of_" ^ type_name in
    let body =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id)) in
      eabstract ~loc patts body
    in
    [constrained_function_binding loc td typ ~tps ~func_name body]

  let sexp_of ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive rec_flag tds in
    let bindings = List.map tds ~f:sexp_of_td |> List.concat in
    let structure = [pstr_value ~loc rec_flag bindings] in
    (*Format.eprintf "%a@." Pprintast.default #structure structure;*)
    Type_conv.Generator_result.make_just_after structure

  (* Add code generator to the set of known generators *)
  let gen =
    Type_conv.Generator.make Type_conv.Args.empty sexp_of
      ~attributes:[ Attribute.T record_default
                  ; Attribute.T drop_default
                  ; Attribute.T drop_if
                  ]
  ;;

  let sexp_of_exn ~loc:_ ~path ec =
    let renaming = Renaming.identity in
    let get_full_cnstr str = path ^ "." ^ str in
    let loc = ec.pext_name.loc in
    let expr =
      match ec with
      | {pext_name = {loc; txt = cnstr};
         pext_kind = Pext_decl ([], None); _;} ->
        [%expr
            Sexplib.Exn_magic.register [%e pexp_construct ~loc
                                             (Located.lident ~loc cnstr) None]
            [%e estring ~loc (get_full_cnstr cnstr)]
        ]
      | {pext_name = {loc; txt = cnstr};
         pext_kind = Pext_decl (_::_ as tps, None); _;} ->
        let fps = List.map ~f:(fun tp -> sexp_of_type renaming tp) tps in
        let sexp_converters = List.map fps ~f:Fun_or_match.(expr ~loc) in
        let _, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
        let qualified_reg_name =
          let open Longident in
          pexp_ident ~loc (Location.mkloc (
            Ldot (Ldot (Lident "Sexplib", "Exn_magic"),
                  Printf.sprintf "register%d" (List.length fps))) loc)
        in
        let make_exc =
          eabstract ~loc patts (pexp_construct ~loc (Located.lident ~loc cnstr)
                                  (Some (pexp_tuple ~loc vars)))
        in
        let call =
          let partial =
            [%expr
                [%e qualified_reg_name]
                [%e make_exc] [%e estring ~loc (get_full_cnstr cnstr)]
            ]
          in
          eapply ~loc partial sexp_converters
        in
        [%expr  [%e call] ]
      | { pext_kind = Pext_decl (_, Some _); _} ->
        Location.raise_errorf ~loc "sexp_of_exn/:"
      | { pext_kind = Pext_rebind _; _} ->
        Location.raise_errorf ~loc "sexp_of_exn/rebind"
    in
    Type_conv.Generator_result.make_just_after [
      pstr_value ~loc Nonrecursive [value_binding ~loc ~pat:[%pat? ()] ~expr]
    ]

  let gen_exn = Type_conv.Generator.make Type_conv.Args.empty sexp_of_exn

end

(* Generator for converters of S-expressions to OCaml-values *)
module Generate_of_sexp = struct

  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let mk_good_sum_matches (loc,cds) =
    List.map cds ~f:(function
    | { pcd_name = cnstr; pcd_args = []; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.Atom ([%p lcstr] | [%p str])] -->
      pexp_construct ~loc (Located.lident ~loc cnstr.txt) None

    | { pcd_name = cnstr; pcd_args = (_::_ as tps); _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? (Sexplib.Sexp.List
                (Sexplib.Sexp.Atom ([%p lcstr] | [%p str] as _tag) ::
                 sexp_args) as _sexp)
      ] -->
      mk_cnstr_args_match ~loc ~is_variant:false cnstr.txt tps
    )

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let mk_bad_sum_matches (loc,cds) =
    List.map cds ~f:(function
    | { pcd_name = cnstr; pcd_args = []; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.List
             (Sexplib.Sexp.Atom ([%p lcstr] | [%p str]) :: _) as sexp
      ] -->
      [%expr Sexplib.Conv_error.stag_no_args _tp_loc sexp]
    | { pcd_name = cnstr; pcd_args = _::_; _} ->
      let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
      let str = pstring ~loc cnstr.txt in
      [%pat? Sexplib.Sexp.Atom ([%p lcstr] | [%p str]) as sexp] -->
      [%expr Sexplib.Conv_error.stag_takes_args _tp_loc sexp]
    )

  (* Generate matching code for sum types *)
  let sum_of_sexp (loc,alts) : Fun_or_match.t =
    Match (List.concat [
      mk_good_sum_matches (loc,alts);
      mk_bad_sum_matches (loc,alts);
      [ [%pat? Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp] -->
        [%expr Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp]
      ; [%pat? Sexplib.Sexp.List [] as sexp] -->
        [%expr Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp]
      ; [%pat? sexp] -->
        [%expr Sexplib.Conv_error.unexpected_stag _tp_loc sexp]
      ]
    ])

  (* Record conversions *)

  (* Generate code for extracting record fields *)
  let mk_extract_fields (loc,flds) =
    let rec loop no_args args = function
      | {pld_name = {txt=nm; loc};
         pld_type = [%type: sexp_bool ] ; _ } :: more_flds ->
        let no_args =
          (pstring ~loc nm -->
           [%expr
             if ! [%e evar ~loc (nm ^ "_field")] then
               duplicates := ( field_name :: !duplicates )
             else [%e evar ~loc (nm ^ "_field")] := true
           ]
          ) :: no_args
        in
        loop no_args args more_flds
      | {pld_name = {txt=nm; loc};
         pld_type = [%type: [%t? tp] sexp_option ] ; _ } :: more_flds
      | {pld_name = {txt=nm; loc}; pld_type = tp; _ } :: more_flds ->
        let unrolled =
          Fun_or_match.unroll ~loc [%expr  _field_sexp ] (type_of_sexp tp)
        in
        let args =
          (pstring ~loc nm -->
           [%expr
             match ! [%e evar ~loc (nm ^ "_field")] with
             | None ->
               let fvalue = [%e unrolled] in
               [%e evar ~loc (nm ^ "_field")] := Some fvalue
             | Some _ ->
               duplicates := (field_name :: ! duplicates) ]
          ) :: args
        in
        loop no_args args more_flds
      | [] ->
        no_args,args
    in
    let handle_extra =
      [ [%pat? _] -->
        [%expr if !Sexplib.Conv.record_check_extra_fields then
                 extra := (field_name :: !extra)
               else ()]
      ]

    in
    loop handle_extra handle_extra (List.rev flds)

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result has_poly (loc,flds) =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop ((res_tpls, bi_lst, good_patts) as acc) = function
        | {pld_name = {txt=nm; loc}; pld_type = tp; _ } as ld :: more_flds ->
          let fld = [%expr ! [%e evar ~loc (nm ^ "_field")]] in
          let mk_default loc =
            bi_lst, [%pat? [%p pvar ~loc (nm ^ "_value")] ] :: good_patts
          in
          let new_bi_lst, new_good_patts =
            match tp with
            | [%type: sexp_bool ]
            | [%type: [%t? _] sexp_option ]
            | [%type: [%t? _] sexp_list ]
            | [%type: [%t? _] sexp_array ]
              -> mk_default loc
            | _ ->
              match Attribute.get record_default ld with
              | Some _ -> mk_default loc
              | None ->
                has_nonopt_fields := true;
                (
                  [%expr
                      (Pervasives.(=) [%e fld] None, [%e estring ~loc nm]) ] :: bi_lst,
                  [%pat?  Some [%p pvar ~loc (nm ^ "_value")] ] :: good_patts
                )
          in
          let acc =(
            [%expr  [%e fld] ] :: res_tpls,
            new_bi_lst,
            new_good_patts
          )
          in loop acc more_flds
        | [] -> acc
      in
      loop ([], [], []) (List.rev flds)
    in
    let match_good_expr =
      if has_poly then
        let cnvt = function
          | {pld_name = {txt=nm; _}; _ } ->
            evar ~loc (nm ^ "_value")
        in
        match List.map ~f:cnvt flds with
        | [match_good_expr] -> match_good_expr
        | match_good_exprs -> pexp_tuple ~loc match_good_exprs
      else
        let cnvt = function
          | {pld_name = {txt=nm; _};
             pld_type = [%type: [%t? _] sexp_list ]; _ } ->
            (Located.lident ~loc nm),
            [%expr
                match [%e evar ~loc (nm ^ "_value")] with
                | None -> [] | Some v -> v
            ]

          | {pld_name = {txt=nm; _};
             pld_type = [%type: [%t? _] sexp_array ]; _ } ->
            (Located.lident ~loc nm),
            [%expr
                match [%e evar ~loc (nm ^ "_value")] with
                | None -> [||] | Some v -> v
            ]
          | {pld_name = {txt=nm; _}; _ } as ld ->
            begin match Attribute.get record_default ld with
            | None ->
              Located.lident ~loc nm,
              evar ~loc (nm ^ "_value")
            | Some default ->
              Located.lident ~loc nm,
              [%expr
                  match [%e evar ~loc (nm ^ "_value")] with
                    None -> [%e default] | Some v -> v
              ]
            end
        in
        pexp_record ~loc (List.map ~f:cnvt flds) None
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [res_expr], [res_patt] -> res_expr, res_patt
      | _ ->
          pexp_tuple ~loc res_tpls,
          ppat_tuple ~loc good_patts
    in
    if !has_nonopt_fields then
      pexp_match ~loc expr
        [ patt --> match_good_expr
        ; [%pat? _] -->
          [%expr
            Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
              [%e elist ~loc bi_lst]
          ]
        ]
    else pexp_match ~loc expr [ patt --> match_good_expr ]

  (* Generate code for converting record fields *)
  let mk_cnv_fields has_poly (loc,flds) =
    let field_refs =
      List.map flds ~f:(function
      | {pld_name = {txt=name; loc};
         pld_type = [%type: sexp_bool]; _ } ->
        value_binding ~loc ~pat:(pvar ~loc (name ^ "_field")) ~expr:[%expr ref false]
      | {pld_name = {txt=name; loc}; _ } ->
        value_binding ~loc ~pat:(pvar ~loc (name ^ "_field")) ~expr:[%expr ref None]
      )
    in
    let mc_no_args_fields, mc_fields_with_args = mk_extract_fields (loc,flds) in
    pexp_let ~loc Nonrecursive (field_refs @ [
      value_binding ~loc ~pat:[%pat? duplicates] ~expr:[%expr ref []];
      value_binding ~loc ~pat:[%pat? extra] ~expr:[%expr ref []];
    ]) [%expr
      let rec iter =
        [%e pexp_function ~loc
              [ [%pat?
                       Sexplib.Sexp.List
                       [(Sexplib.Sexp.Atom field_name); _field_sexp] ::
                     tail] -->
                [%expr [%e pexp_match ~loc [%expr field_name] mc_fields_with_args];
                       iter tail]
              ; [%pat? Sexplib.Sexp.List [(Sexplib.Sexp.Atom field_name)] :: tail] -->
                [%expr [%e pexp_match ~loc [%expr field_name] mc_no_args_fields];
                       iter tail]
              ; [%pat? ((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _) as sexp) :: _] -->
                [%expr Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp]
              ; [%pat? []] --> [%expr ()]
              ]
        ]
      in
      iter field_sexps;
      if Pervasives.(<>) (!duplicates) [] then
        Sexplib.Conv_error.record_duplicate_fields
          _tp_loc (!duplicates) sexp
      else if Pervasives.(<>) (!extra) [] then
        Sexplib.Conv_error.record_extra_fields _tp_loc (!extra) sexp
      else [%e mk_handle_record_match_result has_poly (loc,flds)]
    ]

  let is_poly (_,flds) =
    List.exists flds ~f:(function
    | { pld_type = {ptyp_desc = Ptyp_poly _; _ }; _} -> true
    | _ -> false)

  (* Generate matching code for records *)
  let record_of_sexp (loc,flds) : Fun_or_match.t =
    let handle_fields =
      let has_poly = is_poly (loc,flds) in
      let cnv_fields = mk_cnv_fields has_poly (loc,flds) in
      if has_poly then
        let patt =
          let pats =
            List.map flds ~f:(fun {pld_name = {txt=name; loc}; _ } ->
              pvar ~loc name
            )
          in
          match pats with
          | [pat] -> pat
          | pats -> ppat_tuple ~loc pats
        in
        let record_def =
          pexp_record ~loc (
            List.map flds ~f:(fun {pld_name = {txt=name; loc}; _ } ->
              (Located.lident ~loc name, evar ~loc name)
            )) None
        in
        pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:patt ~expr:cnv_fields]
          record_def
      else cnv_fields
    in
    Match
      [ [%pat? Sexplib.Sexp.List field_sexps as sexp] --> handle_fields
      ; [%pat? Sexplib.Sexp.Atom _ as sexp] -->
        [%expr Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp]
      ]

  (* Empty type *)
  let nil_of_sexp loc : Fun_or_match.t =
    Fun [%expr  fun sexp -> Sexplib.Conv_error.empty_type _tp_loc sexp ]

  (* Generate code from type definitions *)

  let td_of_sexp ~loc:_ ~path td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:(fun tp -> (get_type_param_name tp).txt) in
    let {ptype_name = {txt = type_name; loc = _}; ptype_loc = loc; _} = td in
    let alias_ref = ref `Not_an_alias in
    let handle_alias tp =
      alias_ref :=
        (match tp with
          { ptyp_desc = Ptyp_var _; _ } -> `Alias `Type_var
        | _ -> `Alias `Type_constructor);
      type_of_sexp tp
    in
    let full_type =
      core_type_of_type_declaration td
      |> replace_variables_by_underscores
    in
    let is_variant_ref = ref false in
    let handle_variant (loc,row_fields) =
      is_variant_ref := true;
      variant_of_sexp ~full_type (loc,row_fields)
    in
    let is_private = (match td.ptype_private with Private -> true | Public -> false) in
    if is_private
    then Location.raise_errorf ~loc "of_sexp is not supported for private type";
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant alts -> sum_of_sexp (td.ptype_loc, alts)
        | Ptype_record lbls -> record_of_sexp (loc, lbls)
        | Ptype_open -> Location.raise_errorf ~loc
                          "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          match td.ptype_manifest with
          | None -> nil_of_sexp td.ptype_loc
          | Some { ptyp_desc = Ptyp_variant (rows, _, _); _ } -> handle_variant (loc, rows)
          | Some ty -> handle_alias ty
      in
      match body with
      (* Prevent violation of value restriction and problems with
         recursive types by eta-expanding function definitions *)
      | Fun fun_expr -> [%expr fun t -> [%e fun_expr] t ]
      | Match matchings -> pexp_function ~loc matchings
    in
    let external_name = type_name ^ "_of_sexp" in
    let internal_name = "__" ^ type_name ^ "_of_sexp__" in
    let arg_patts, arg_exprs =
      List.split (
        List.map ~f:(fun tp ->
            let name = "_of_" ^ tp in
            pvar ~loc name, evar ~loc name)
          tps)
    in
    let with_poly_call =
      match !alias_ref with
      | `Not_an_alias
      | `Alias `Type_constructor -> false
      | `Alias `Type_var -> true in
    let internal_fun_body =
      let full_type_name = Printf.sprintf "%s.%s" path type_name in
      if with_poly_call then
        (* special case for type definitions whose bodies are type variables, like:
             type ('a, 'b) t = 'a
           because
           - they can used in polymorphic variants: [ ([`A], int) t | `B ]
           - the way sexplib works, it cannot handle backtracking in these cases,
             (because we only receive as parameter sexp_of_'a but not
             __sexp_of_'a__ presumably)
             so it is better to emit an error rather than do something weird
        *)
        eabstract ~loc arg_patts
          [%expr
            fun sexp ->
              Sexplib.Conv_error.silly_type [%e estring ~loc full_type_name] sexp
          ]
      else
        [%expr
          let _tp_loc = [%e estring ~loc full_type_name] in
          [%e eabstract ~loc arg_patts body]
        ]
    in
    let pre_external_fun_body =
      let internal_call =
        let internal_expr = evar ~loc internal_name in
        eapply ~loc internal_expr (arg_exprs @ [ [%expr sexp] ])
      in
      let no_variant_match_mc =
        [ [%pat? Sexplib.Conv_error.No_variant_match (_tp_loc, sexp)] -->
          [%expr Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp]
        ]
      in
      if with_poly_call then
        pexp_try ~loc [%expr [%e body] sexp] no_variant_match_mc
      (* Type alias may refer to variant, therefore same handling here! *)
      else if !is_variant_ref || !alias_ref = `Alias `Type_constructor then
        pexp_try ~loc internal_call no_variant_match_mc
      else internal_call
    in
    let external_fun_body =
      eabstract ~loc arg_patts
        [%expr  fun sexp -> ([%e pre_external_fun_body]) ]
    in
    let typ = Sig_generate_of_sexp.mk_type td in
    let mk_binding func_name body =
      constrained_function_binding loc td typ ~tps ~func_name body
    in
    let internal_binding = mk_binding internal_name internal_fun_body in
    let external_binding = mk_binding external_name external_fun_body in
    [internal_binding], [external_binding]

  (* Generate code from type definitions *)
  let of_sexp ~loc ~path (rec_flag, tds) =
    begin
      (* special case for singleton type defs to match camlp4 *)
      let singleton = (match tds with [_] -> true | _ -> false) in
      if singleton
      then
        match really_recursive rec_flag tds with
        | Recursive ->
          let bindings =
            List.map tds ~f:(fun td ->
              let internals,externals = td_of_sexp ~loc ~path td in
              internals @ externals)
            |> List.concat
          in
          [pstr_value ~loc Recursive bindings]
        | Nonrecursive ->
          let bindings =
            List.map tds ~f:(fun td ->
              let internals,externals = td_of_sexp ~loc ~path td in
              [pstr_value ~loc Nonrecursive internals;
               pstr_value ~loc Nonrecursive externals])
            |> List.concat
          in
          bindings
      else
        let bindings =
          List.map tds ~f:(fun td ->
            let internals,externals = td_of_sexp ~loc ~path td in
            internals @ externals)
          |> List.concat
        in
        [pstr_value ~loc rec_flag bindings]
    end
    |> Type_conv.Generator_result.make_just_after

  let gen = Type_conv.Generator.make Type_conv.Args.empty of_sexp
              ~attributes:[Attribute.T record_default]
end

module Quotations = struct
  let of_sexp_quote ~loc ~path ctyp =
    let fp = type_of_sexp ctyp in
    let body =
      match fp with
      | Fun fun_expr -> [%expr  [%e fun_expr] sexp ]
      | Match matchings -> pexp_match ~loc [%expr sexp] matchings
    in
    let full_type_name =
      let _, line_num, _ = Location.get_pos_info loc.Location.loc_start in
      Printf.sprintf "%s line %i: %s"
        path line_num
        (string_of_core_type ctyp)
    in
    [%expr
      fun sexp ->
        let _tp_loc = [%e estring ~loc full_type_name] in
        [%e body]
      ]
  ;;

  let sexp_of_quote = Ppx_sexp_conv_expander.sexp_of
end

let () =
  let of_sexp =
    Type_conv.add "of_sexp"
      ~str_type_decl:Generate_of_sexp.gen
      ~sig_type_decl:Sig_generate_of_sexp.gen
      ~extension:Quotations.of_sexp_quote
  in
  let sexp_of =
    Type_conv.add "sexp_of"
      ~str_type_decl:Generate_sexp_of.gen
      ~str_exception:Generate_sexp_of.gen_exn
      ~sig_type_decl:Sig_generate_sexp_of.gen
      ~sig_exception:Sig_generate_sexp_of.gen_exn
      ~extension:(fun ~loc:_ ~path:_ ty -> Quotations.sexp_of_quote ty)
  in
  let of_sexp_poly =
    Type_conv.add "of_sexp_poly"
      ~sig_type_decl:Sig_generate_of_sexp.gen_poly
  in
  Type_conv.add_alias "sexp" [sexp_of; of_sexp]
    ~str_exception:[sexp_of]
    ~sig_exception:[sexp_of]
  |> Type_conv.ignore;
  Type_conv.add_alias "sexp_poly" [sexp_of; of_sexp_poly]
  |> Type_conv.ignore;
;;
