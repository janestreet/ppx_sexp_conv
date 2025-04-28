open! Stdppx
open! Ppxlib
open Ast_builder.Default

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

(* Utility functions *)

let replace_variables_by_underscores =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc t =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree t with
        | Ptyp_var (_, jkind) ->
          Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_any jkind)
        | _ -> super#core_type_desc t
    end
  in
  map#core_type
;;

let make_rigid_types tps =
  List.fold_left tps ~init:String.Map.empty ~f:(fun map (tp, jkind) ->
    String.Map.update
      tp.txt
      (function
        | None -> Some (Fresh_name.of_string_loc tp, jkind)
        | Some (fresh, jkind) ->
          (* Ignore duplicate names, the typechecker will raise after expansion. *)
          Some (fresh, jkind))
      map)
;;

let find_rigid_type ~loc ~rigid_types name =
  match String.Map.find_opt name rigid_types with
  | Some (tp, jkind) -> Fresh_name.to_string_loc tp, jkind
  | None ->
    (* Ignore unbound type names, the typechecker will raise after expansion. *)
    { txt = name; loc }, None
;;

let find_rigid_type_constr ~loc ~rigid_types name =
  let name, _jkind = find_rigid_type ~loc ~rigid_types name in
  Ptyp_constr (Located.map_lident name, [])
;;

let make_type_rigid ~rigid_types =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type ty =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
        | Ptyp_var (name, _) ->
          let ptyp_desc = find_rigid_type_constr ~loc:ty.ptyp_loc ~rigid_types name in
          { ty with ptyp_desc }
        | _ -> super#core_type ty
    end
  in
  map#core_type
;;

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ] or
   [type a .. z. make_mono_type t (a .. z)] when [use_rigid_variables] is true.
   Annotation are needed for non regular recursive datatypes and gadt when the return type
   of constructors are constrained. Unfortunately, putting rigid variables everywhere does
   not work because of certains types with constraints. We thus only use rigid variables
   for sum types, which includes all GADTs. *)

type bound_var = string loc * Ppxlib_jane.jkind_annotation option

let tvars_of_core_type : core_type -> bound_var list =
  let add_binding_to_list (bindings : bound_var list) (bound : bound_var) =
    let { txt = bound_name; loc = _ }, _annot = bound in
    match
      List.exists bindings ~f:(fun b' ->
        let { txt = bound_name'; loc = _ }, _annot' = b' in
        String.equal bound_name bound_name')
    with
    | true -> bindings
    | false -> bound :: bindings
  in
  let tvars =
    object
      inherit [bound_var list] Ast_traverse.fold as super

      method! core_type x acc =
        let loc = x.ptyp_loc in
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree x.ptyp_desc with
        | Ptyp_var (bound_name, jkind) ->
          add_binding_to_list acc ({ txt = bound_name; loc }, jkind)
        | _ -> super#core_type x acc
    end
  in
  fun typ -> List.rev (tvars#core_type typ [])
;;

let constrained_function_binding
  (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
    (loc : Location.t)
  (td : type_declaration)
  (typ : core_type)
  ~(tps : (string loc * Ppxlib_jane.jkind_annotation option) list)
  ~(func_name : string)
  ~portable
  (body : expression)
  =
  let bound_vars = tvars_of_core_type typ in
  let has_vars =
    match bound_vars with
    | [] -> false
    | _ :: _ -> true
  in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars
    then pat
    else (
      let annot = Ppxlib_jane.Ast_builder.Default.ptyp_poly ~loc bound_vars typ in
      ppat_constraint ~loc pat annot)
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with
      | Ptype_variant _ -> true
      | _ -> false
    in
    if use_rigid_variables
    then (
      let rigid_types = make_rigid_types tps in
      List.fold_right
        tps
        ~f:(fun (tp, _) body ->
          let name, jkind = find_rigid_type ~loc:tp.loc ~rigid_types tp.txt in
          match jkind with
          | None -> pexp_newtype ~loc name body
          | Some jkind ->
            Ppxlib_jane.Ast_builder.Default.pexp_newtype ~loc name (Some jkind) body)
        ~init:(pexp_constraint ~loc body (make_type_rigid ~rigid_types typ)))
    else if has_vars
    then body
    else pexp_constraint ~loc body typ
  in
  Ppxlib_jane.Ast_builder.Default.value_binding
    ~loc
    ~pat
    ~expr:body
    ~modes:(if portable then [ { txt = Mode "portable"; loc } ] else [])
;;

let with_let ~loc ~binds body =
  List.fold_right binds ~init:body ~f:(fun bind body ->
    if List.is_empty bind then body else pexp_let ~loc Nonrecursive bind body)
;;

let with_types ~loc ~types body =
  if List.is_empty types
  then body
  else
    pexp_open
      ~loc
      (open_infos
         ~loc
         ~override:Fresh
         ~expr:
           (pmod_structure
              ~loc
              (List.map types ~f:(fun type_decl -> pstr_type ~loc Recursive [ type_decl ]))))
      body
;;

let fresh_lambda ~loc apply =
  let var = gen_symbol ~prefix:"x" () in
  let pat = pvar ~loc var in
  let arg = evar ~loc var in
  let body = apply ~arg in
  pexp_fun ~loc Nolabel None pat body
;;

let rec is_value_expression expr =
  match
    Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc:expr.pexp_loc
  with
  (* Syntactic values. *)
  | Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_lazy _ -> true
  (* Type-only wrappers; we check their contents. *)
  | Pexp_constraint (expr, (_ : core_type option), _)
  | Pexp_coerce (expr, (_ : core_type option), (_ : core_type))
  | Pexp_newtype ((_ : string loc), (_ : Ppxlib_jane.jkind_annotation option), expr)
  | Pexp_stack expr -> is_value_expression expr
  (* Allocating constructors; they are only values if all of their contents are. *)
  | Pexp_tuple lexprs -> List.for_all lexprs ~f:(fun (_, e) -> is_value_expression e)
  | Pexp_unboxed_tuple lexprs ->
    List.for_all lexprs ~f:(fun (_, e) -> is_value_expression e)
  | Pexp_construct (_, None) -> true
  | Pexp_construct (_, Some expr) -> is_value_expression expr
  | Pexp_variant (_, None) -> true
  | Pexp_variant (_, Some expr) -> is_value_expression expr
  | Pexp_record (fields, maybe_expr) | Pexp_record_unboxed_product (fields, maybe_expr) ->
    List.for_all fields ~f:(fun (_, expr) -> is_value_expression expr)
    &&
      (match maybe_expr with
      | None -> true
      | Some expr -> is_value_expression expr)
  (* Not values, or not always values. We make a conservative approximation. *)
  | Pexp_unreachable
  | Pexp_let _
  | Pexp_apply _
  | Pexp_match _
  | Pexp_try _
  | Pexp_field _
  | Pexp_unboxed_field _
  | Pexp_setfield _
  | Pexp_array _
  | Pexp_ifthenelse _
  | Pexp_sequence _
  | Pexp_while _
  | Pexp_for _
  | Pexp_send _
  | Pexp_new _
  | Pexp_setinstvar _
  | Pexp_override _
  | Pexp_letmodule _
  | Pexp_letexception _
  | Pexp_assert _
  | Pexp_poly _
  | Pexp_object _
  | Pexp_pack _
  | Pexp_open _
  | Pexp_letop _
  | Pexp_extension _
  | Pexp_comprehension _
  | Pexp_overwrite _
  | Pexp_hole -> false
;;

let really_recursive_respecting_opaque rec_flag tds =
  (object
     inherit type_is_recursive rec_flag tds as super

     method! core_type ctype =
       match ctype with
       | _ when Option.is_some (Attribute.get ~mark_as_seen:false Attrs.opaque ctype) ->
         ()
       | [%type: [%t? _] sexp_opaque] -> ()
       | _ -> super#core_type ctype
  end)
    #go
    ()
;;

let strip_attributes =
  object
    inherit Ppxlib_jane.Ast_traverse.map

    method! attribute attr =
      Location.raise_errorf ~loc:attr.attr_loc "failed to strip attribute from syntax"

    method! attributes _ = []

    method! signature_items items =
      List.filter items ~f:(fun item ->
        match item.psig_desc with
        | Psig_attribute _ -> false
        | _ -> true)

    method! structure items =
      List.filter items ~f:(fun item ->
        match item.pstr_desc with
        | Pstr_attribute _ -> false
        | _ -> true)

    method! class_signature csig =
      { csig with
        pcsig_fields =
          List.filter csig.pcsig_fields ~f:(fun field ->
            match field.pctf_desc with
            | Pctf_attribute _ -> false
            | _ -> true)
      }

    method! class_structure cstr =
      { cstr with
        pcstr_fields =
          List.filter cstr.pcstr_fields ~f:(fun field ->
            match field.pcf_desc with
            | Pcf_attribute _ -> false
            | _ -> true)
      }
  end
;;
