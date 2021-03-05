open! Base
open! Ppxlib
open Ast_builder.Default

let unsupported ~loc string =
  Location.raise_errorf ~loc "sexp_grammar: %s are unsupported" string
;;

let grammar_name name = name ^ "_sexp_grammar"
let tyvar_grammar_name name = grammar_name ("_'" ^ name)
let estr { loc; txt } = estring ~loc txt
let grammar_type ~loc core_type = [%type: [%t core_type] Ppx_sexp_conv_lib.Sexp_grammar.t]

let abstract_grammar ~ctxt ~loc id =
  let module_name =
    ctxt |> Expansion_context.Deriver.code_path |> Code_path.fully_qualified_path
  in
  [%expr Any [%e estr { id with txt = String.concat ~sep:"." [ module_name; id.txt ] }]]
;;

let arrow_grammar ~loc = [%expr Ppx_sexp_conv_lib.Conv.fun_sexp_grammar.untyped]
let opaque_grammar ~loc = [%expr Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar.untyped]
let wildcard_grammar ~loc = [%expr Any "_"]
let list_grammar ~loc expr = [%expr List [%e expr]]
let many_grammar ~loc expr = [%expr Many [%e expr]]
let fields_grammar ~loc expr = [%expr Fields [%e expr]]
let tyvar_grammar ~loc expr = [%expr Tyvar [%e expr]]
let tycon_grammar ~loc name args = [%expr Tycon ([%e name], [%e args])]
let recursive_grammar ~loc grammar defns = [%expr Recursive ([%e grammar], [%e defns])]

let defns_type ~loc =
  [%type: Ppx_sexp_conv_lib.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t]
;;

let untyped_grammar ~loc expr =
  match expr with
  | [%expr { untyped = [%e? untyped] }] -> untyped
  | _ -> [%expr [%e expr].untyped]
;;

let typed_grammar ~loc expr =
  match expr with
  | [%expr [%e? typed].untyped] -> typed
  | _ -> [%expr { untyped = [%e expr] }]
;;

let defn_expr ~loc ~tycon ~tyvars ~grammar =
  [%expr { tycon = [%e tycon]; tyvars = [%e tyvars]; grammar = [%e grammar] }]
;;

let union_grammar ~loc exprs =
  match exprs with
  | [] -> [%expr Union []]
  | [ expr ] -> expr
  | _ -> [%expr Union [%e elist ~loc exprs]]
;;

let tuple_grammar ~loc exprs =
  List.fold_right exprs ~init:[%expr Empty] ~f:(fun expr rest ->
    [%expr Cons ([%e expr], [%e rest])])
;;

let variant_and_or_enum_grammars ~loc ~name_kind ~variants ~enums =
  let unless_empty ~f = function
    | [] -> None
    | _ :: _ as xs -> Some (f xs)
  in
  List.filter_opt
    [ unless_empty enums ~f:(fun enums ->
        [%expr
          Enum
            { name_kind = [%e name_kind]
            ; names = [%e enums |> List.map ~f:estr |> elist ~loc]
            }])
    ; unless_empty variants ~f:(fun variants ->
        let clauses =
          List.map variants ~f:(fun (name, args) ->
            [%expr { name = [%e estr name]; args = [%e args] }])
        in
        [%expr
          Variant { name_kind = [%e name_kind]; clauses = [%e elist ~loc clauses] }])
    ]
;;

(* Wrap [expr] in [fun a b ... ->] for type parameters. *)
let td_params_fun td expr =
  let loc = td.ptype_loc in
  let params =
    List.map td.ptype_params ~f:(fun param ->
      let { loc; txt } = get_type_param_name param in
      pvar ~loc (tyvar_grammar_name txt))
  in
  eabstract ~loc params expr
;;

module Row_field_type = struct
  type t =
    | Inherit of core_type
    | Tag_no_arg of string loc
    | Tag_with_arg of string loc * core_type

  let of_row_field ~loc row_field =
    match row_field with
    | Rinherit core_type -> Inherit core_type
    | Rtag (name, possibly_no_arg, possible_type_args) ->
      (match possibly_no_arg, possible_type_args with
       | true, [] -> Tag_no_arg name
       | false, [ core_type ] -> Tag_with_arg (name, core_type)
       | false, [] -> unsupported ~loc "empty polymorphic variant types"
       | true, _ :: _ | false, _ :: _ :: _ -> unsupported ~loc "intersection types")
  ;;
end

let rec grammar_of_type core_type ~rec_flag =
  let loc = core_type.ptyp_loc in
  match Attribute.get Attrs.opaque core_type with
  | Some () -> opaque_grammar ~loc
  | None ->
    (match core_type.ptyp_desc with
     | Ptyp_any -> wildcard_grammar ~loc
     | Ptyp_var name ->
       (match rec_flag with
        | Recursive ->
          (* For recursive grammars, [grammar_of_type] for any type variables is called
             inside a [defn]. The variables should therefore be resolved as [Tyvar]
             grammars. *)
          tyvar_grammar ~loc (estring ~loc name)
        | Nonrecursive ->
          (* Outside recursive [defn]s, type variables are passed in as function
             arguments. *)
          unapplied_type_constr_conv ~loc ~f:tyvar_grammar_name (Located.lident ~loc name)
          |> untyped_grammar ~loc)
     | Ptyp_arrow _ -> arrow_grammar ~loc
     | Ptyp_tuple list ->
       List.map ~f:(grammar_of_type ~rec_flag) list
       |> tuple_grammar ~loc
       |> list_grammar ~loc
     | Ptyp_constr (id, args) ->
       List.map args ~f:(fun core_type ->
         let loc = core_type.ptyp_loc in
         grammar_of_type ~rec_flag core_type |> typed_grammar ~loc)
       |> type_constr_conv ~loc ~f:grammar_name id
       |> untyped_grammar ~loc
     | Ptyp_object _ -> unsupported ~loc "object types"
     | Ptyp_class _ -> unsupported ~loc "class types"
     | Ptyp_alias _ -> unsupported ~loc "type aliases"
     | Ptyp_variant (rows, closed_flag, (_ : string list option)) ->
       (match closed_flag with
        | Open -> unsupported ~loc "open polymorphic variant types"
        | Closed -> grammar_of_polymorphic_variant ~loc ~rec_flag rows)
     | Ptyp_poly _ -> unsupported ~loc "explicitly polymorphic types"
     | Ptyp_package _ -> unsupported ~loc "first-class module types"
     | Ptyp_extension _ -> unsupported ~loc "unexpanded ppx extensions")

and grammar_of_polymorphic_variant ~loc ~rec_flag rows =
  let inherits, enums, variants =
    List.partition3_map rows ~f:(fun row ->
      match Attribute.get Attrs.list_poly row with
      | Some () ->
        (match Row_field_type.of_row_field ~loc row.prf_desc with
         | Tag_with_arg (name, [%type: [%t? ty] list]) ->
           `Trd (name, many_grammar ~loc (grammar_of_type ~rec_flag ty))
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list")
      | None ->
        (match Row_field_type.of_row_field ~loc row.prf_desc with
         | Inherit core_type -> `Fst (grammar_of_type ~rec_flag core_type)
         | Tag_no_arg name -> `Snd name
         | Tag_with_arg (name, core_type) ->
           `Trd (name, tuple_grammar ~loc [ grammar_of_type ~rec_flag core_type ])))
  in
  variant_and_or_enum_grammars ~loc ~name_kind:[%expr Any_case] ~enums ~variants
  |> List.append inherits
  |> union_grammar ~loc
;;

let record_expr ~loc ~rec_flag ~extra_attr syntax fields =
  let fields =
    List.map fields ~f:(fun field ->
      let loc = field.pld_loc in
      let field_kind = Attrs.Record_field_handler.Of_sexp.create ~loc field in
      let required =
        match field_kind with
        | None -> true
        | Some
            ( `default _
            | `sexp_bool
            | `sexp_option _
            | `sexp_array _
            | `sexp_list _
            | `omit_nil ) -> false
      in
      let args =
        match field_kind with
        | None | Some (`default _ | `omit_nil) ->
          [%expr Cons ([%e grammar_of_type ~rec_flag field.pld_type], Empty)]
        | Some `sexp_bool -> [%expr Empty]
        | Some (`sexp_option ty) ->
          [%expr Cons ([%e grammar_of_type ~rec_flag ty], Empty)]
        | Some (`sexp_list ty | `sexp_array ty) ->
          [%expr Cons (List (Many [%e grammar_of_type ~rec_flag ty]), Empty)]
      in
      [%expr
        { name = [%e estr field.pld_name]
        ; required = [%e ebool ~loc required]
        ; args = [%e args]
        }])
  in
  let allow_extra_fields =
    match Attribute.get extra_attr syntax with
    | Some () -> true
    | None -> false
  in
  [%expr
    { allow_extra_fields = [%e ebool ~loc allow_extra_fields]
    ; fields = [%e elist ~loc fields]
    }]
;;

let grammar_of_variant ~loc ~rec_flag clauses =
  let enums, variants =
    List.partition_map clauses ~f:(fun clause ->
      let loc = clause.pcd_loc in
      match Attribute.get Attrs.list_variant clause with
      | Some () ->
        (match clause.pcd_args with
         | Pcstr_tuple [ [%type: [%t? ty] list] ] ->
           let args = many_grammar ~loc (grammar_of_type ty ~rec_flag) in
           Second (clause.pcd_name, args)
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_variant "_ list")
      | None ->
        (match clause.pcd_args with
         | Pcstr_tuple [] -> First clause.pcd_name
         | Pcstr_tuple (_ :: _ as args) ->
           let args =
             tuple_grammar ~loc (List.map args ~f:(grammar_of_type ~rec_flag))
           in
           Second (clause.pcd_name, args)
         | Pcstr_record fields ->
           let args =
             record_expr
               ~loc
               ~rec_flag
               ~extra_attr:Attrs.allow_extra_fields_cd
               clause
               fields
             |> fields_grammar ~loc
           in
           Second (clause.pcd_name, args)))
  in
  variant_and_or_enum_grammars ~loc ~name_kind:[%expr Capitalized] ~enums ~variants
  |> union_grammar ~loc
;;

let grammar_of_td ~ctxt ~rec_flag td =
  let loc = td.ptype_loc in
  match td.ptype_kind with
  | Ptype_open -> unsupported ~loc "open types"
  | Ptype_record fields ->
    record_expr ~loc ~rec_flag ~extra_attr:Attrs.allow_extra_fields_td td fields
    |> fields_grammar ~loc
    |> list_grammar ~loc
  | Ptype_variant clauses -> grammar_of_variant ~loc ~rec_flag clauses
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | None -> abstract_grammar ~ctxt ~loc td.ptype_name
     | Some core_type -> grammar_of_type ~rec_flag core_type)
;;

let pattern_of_td td =
  let { loc; txt } = td.ptype_name in
  ppat_constraint
    ~loc
    (pvar ~loc (grammar_name txt))
    (combinator_type_of_type_declaration td ~f:grammar_type)
;;

(* Any grammar expression that is purely a constant does no work, and does not need to be
   wrapped in [Lazy]. *)
let rec is_preallocated_constant expr =
  match expr.pexp_desc with
  | Pexp_constraint (expr, _) | Pexp_coerce (expr, _, _) | Pexp_open (_, expr) ->
    is_preallocated_constant expr
  | Pexp_constant _ -> true
  | Pexp_tuple args -> List.for_all ~f:is_preallocated_constant args
  | Pexp_variant (_, maybe_arg) | Pexp_construct (_, maybe_arg) ->
    Option.for_all ~f:is_preallocated_constant maybe_arg
  | Pexp_record (fields, maybe_template) ->
    List.for_all fields ~f:(fun (_, expr) -> is_preallocated_constant expr)
    && Option.for_all ~f:is_preallocated_constant maybe_template
  | _ -> false
;;

(* Any grammar expression that just refers to a previously defined grammar also does not
   need to be wrapped in [Lazy]. Accessing the previous grammar is work, but building the
   closure for a lazy value is at least as much work anyway. *)
let rec is_variable_access expr =
  match expr.pexp_desc with
  | Pexp_constraint (expr, _) | Pexp_coerce (expr, _, _) | Pexp_open (_, expr) ->
    is_variable_access expr
  | Pexp_ident _ -> true
  | Pexp_field (expr, _) -> is_variable_access expr
  | _ -> false
;;

let grammar_needs_lazy_wrapper expr =
  not (is_preallocated_constant expr || is_variable_access expr)
;;

let lazy_grammar ~loc td expr =
  if
    List.is_empty td.ptype_params
    (* polymorphic types generate functions, so the body does not need a [lazy] wrapper *)
    && grammar_needs_lazy_wrapper expr
  then [%expr Lazy (lazy [%e expr])]
  else expr
;;

let force_expr ~loc expr = [%expr Stdlib.Lazy.force [%e expr]]

(* Definitions of grammars that do not refer to each other. *)
let nonrecursive_grammars ~ctxt ~loc td_lists =
  List.concat_map td_lists ~f:(fun tds ->
    List.map tds ~f:(fun td ->
      let td = name_type_params_in_td td in
      let loc = td.ptype_loc in
      let pat = pattern_of_td td in
      let expr =
        grammar_of_td ~ctxt ~rec_flag:Nonrecursive td
        |> lazy_grammar td ~loc
        |> typed_grammar ~loc
        |> td_params_fun td
      in
      value_binding ~loc ~pat ~expr)
    |> pstr_value_list ~loc Nonrecursive)
;;

(* Type constructor grammars used to "tie the knot" for (mutally) recursive grammars. *)
let recursive_grammar_tycons tds =
  List.map tds ~f:(fun td ->
    let td = name_type_params_in_td td in
    let loc = td.ptype_loc in
    let pat = pattern_of_td td in
    let expr =
      tycon_grammar
        ~loc
        (estr td.ptype_name)
        (List.map td.ptype_params ~f:(fun param ->
           let { loc; txt } = get_type_param_name param in
           tyvar_grammar_name txt |> evar ~loc |> untyped_grammar ~loc)
         |> elist ~loc)
      |> typed_grammar ~loc
      |> td_params_fun td
    in
    value_binding ~loc ~pat ~expr)
;;

(* Recursive grammar definitions, based on the type constructors from above. *)
let recursive_grammar_defns ~ctxt ~loc tds =
  List.map tds ~f:(fun td ->
    let td = name_type_params_in_td td in
    let loc = td.ptype_loc in
    let tycon = estr td.ptype_name in
    let tyvars =
      List.map td.ptype_params ~f:(fun param -> estr (get_type_param_name param))
      |> elist ~loc
    in
    let grammar = grammar_of_td ~ctxt ~rec_flag:Recursive td in
    defn_expr ~loc ~tycon ~tyvars ~grammar)
  |> elist ~loc
;;

(* Grammar expression using [Recursive] and a shared definition of grammar definitions.
   The shared definitions are wrapped in [lazy] to avoid toplevel side effects. *)
let recursive_grammar_expr ~defns_name td =
  let td = name_type_params_in_td td in
  let loc = td.ptype_loc in
  let pat = pattern_of_td td in
  let expr =
    let tyvars =
      List.map td.ptype_params ~f:(fun param ->
        let { loc; txt } = get_type_param_name param in
        tyvar_grammar_name txt |> evar ~loc |> untyped_grammar ~loc)
      |> elist ~loc
    in
    recursive_grammar
      ~loc
      (tycon_grammar ~loc (estr td.ptype_name) tyvars)
      (evar ~loc defns_name |> force_expr ~loc)
    |> lazy_grammar td ~loc
    |> typed_grammar ~loc
    |> td_params_fun td
  in
  value_binding ~loc ~pat ~expr
;;

(* Puts together recursive grammar definitions from the parts implemented above. *)
let recursive_grammars ~ctxt ~loc tds =
  match List.is_empty tds with
  | true -> []
  | false ->
    let defns_name = gen_symbol ~prefix:"grammars" () in
    let defns_item =
      let expr =
        recursive_grammar_defns ~ctxt ~loc tds
        |> pexp_let ~loc Nonrecursive (recursive_grammar_tycons tds)
        |> pexp_lazy ~loc
      in
      let pat = ppat_constraint ~loc (pvar ~loc defns_name) (defns_type ~loc) in
      pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ]
    in
    let grammars_item =
      List.map tds ~f:(recursive_grammar_expr ~defns_name) |> pstr_value ~loc Nonrecursive
    in
    [%str
      include struct
        open struct
          [%%i defns_item]
        end

        [%%i grammars_item]
      end]
;;

let partition_recursive_and_nonrecursive ~rec_flag tds =
  match (rec_flag : rec_flag) with
  | Nonrecursive -> [], [ tds ]
  | Recursive ->
    (* Pulling out non-recursive references repeatedly means we only "tie the knot" for
       variables that actually need it, and we don't have to manually [ignore] the added
       bindings in case they are unused. *)
    let rec loop tds ~acc =
      let obj =
        object
          inherit type_is_recursive Recursive tds

          method recursion td = {<type_names = [ td.ptype_name.txt ]>}#go ()
        end
      in
      let recursive, nonrecursive =
        List.partition_tf tds ~f:(fun td ->
          match obj#recursion td with
          | Recursive -> true
          | Nonrecursive -> false)
      in
      if List.is_empty recursive || List.is_empty nonrecursive
      then recursive, nonrecursive :: acc
      else loop recursive ~acc:(nonrecursive :: acc)
    in
    loop tds ~acc:[]
;;

let str_type_decl ~ctxt (rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let recursive, nonrecursive = partition_recursive_and_nonrecursive ~rec_flag tds in
  [ recursive_grammars ~ctxt ~loc recursive
  ; nonrecursive_grammars ~ctxt ~loc nonrecursive
  ]
  |> List.concat
;;

let sig_type_decl ~ctxt:_ (_rec_flag, tds) =
  List.map tds ~f:(fun td ->
    let loc = td.ptype_loc in
    value_description
      ~loc
      ~name:(Loc.map td.ptype_name ~f:grammar_name)
      ~type_:(combinator_type_of_type_declaration td ~f:grammar_type)
      ~prim:[]
    |> psig_value ~loc)
;;

let extension_loc ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  { loc with loc_ghost = true }
;;

let core_type ~ctxt core_type =
  let loc = extension_loc ~ctxt in
  pexp_constraint
    ~loc
    (core_type |> grammar_of_type ~rec_flag:Nonrecursive |> typed_grammar ~loc)
    (core_type |> grammar_type ~loc)
  |> Merlin_helpers.hide_expression
;;

let type_extension ~ctxt core_type =
  assert_no_attributes_in#core_type core_type;
  let loc = extension_loc ~ctxt in
  core_type |> grammar_type ~loc
;;
