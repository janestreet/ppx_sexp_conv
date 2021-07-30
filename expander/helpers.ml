open! Base
open! Ppxlib
open Ast_builder.Default

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

(* Simplifies match cases, for readability of the generated code. It's not obvious we can
   stick this in ppx_core, as (match e1 with p -> e2) and (let p = e1 in e2) are not typed
   exactly the same (type inference goes in different order, meaning type disambiguation
   differs). *)
let pexp_match ~loc expr cases =
  match cases with
  | [ { pc_lhs; pc_guard = None; pc_rhs } ] ->
    (match pc_lhs, expr with
     | ( { ppat_attributes = []; ppat_desc = Ppat_var { txt = ident; _ }; _ }
       , { pexp_attributes = []; pexp_desc = Pexp_ident { txt = Lident ident'; _ }; _ } )
       when String.equal ident ident' -> pc_rhs
     | _ -> pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat:pc_lhs ~expr ] pc_rhs)
  | _ -> pexp_match ~loc expr cases
;;

(* Utility functions *)

let replace_variables_by_underscores =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc =
        function
        | Ptyp_var _ -> Ptyp_any
        | t -> super#core_type_desc t
    end
  in
  map#core_type
;;

let rigid_type_var ~type_name x =
  let prefix = "rigid_" in
  if String.equal x type_name || String.is_prefix x ~prefix
  then prefix ^ x ^ "_of_type_" ^ type_name
  else x
;;

let make_type_rigid ~type_name =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type ty =
        let ptyp_desc =
          match ty.ptyp_desc with
          | Ptyp_var s ->
            Ptyp_constr (Located.lident ~loc:ty.ptyp_loc (rigid_type_var ~type_name s), [])
          | desc -> super#core_type_desc desc
        in
        { ty with ptyp_desc }
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

let tvars_of_core_type : core_type -> string list =
  let tvars =
    object
      inherit [string list] Ast_traverse.fold as super

      method! core_type x acc =
        match x.ptyp_desc with
        | Ptyp_var x -> if List.mem acc x ~equal:String.equal then acc else x :: acc
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
      ~(tps : string loc list)
      ~(func_name : string)
      (body : expression)
  =
  let vars = tvars_of_core_type typ in
  let has_vars =
    match vars with
    | [] -> false
    | _ :: _ -> true
  in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars
    then pat
    else (
      let vars = List.map ~f:(fun txt -> { txt; loc }) vars in
      ppat_constraint ~loc pat (ptyp_poly ~loc vars typ))
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with
      | Ptype_variant _ -> true
      | _ -> false
    in
    if use_rigid_variables
    then (
      let type_name = td.ptype_name.txt in
      List.fold_right
        tps
        ~f:(fun tp body ->
          pexp_newtype ~loc { txt = rigid_type_var ~type_name tp.txt; loc = tp.loc } body)
        ~init:(pexp_constraint ~loc body (make_type_rigid ~type_name typ)))
    else if has_vars
    then body
    else pexp_constraint ~loc body typ
  in
  value_binding ~loc ~pat ~expr:body
;;

let really_recursive rec_flag tds =
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
