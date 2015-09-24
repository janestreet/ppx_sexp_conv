open StdLabels
open MoreLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]

module StringMap = Map.Make(String)

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

let replace_variables_by_underscores =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_var _ -> Ptyp_any
      | t -> super#core_type_desc t
  end in
  map#core_type

(* A renaming is a mapping from type variable name to type variable name.
   In definitions such as:

   type 'a t =
   | A : <type> -> 'b t
   | B of 'a

   we generate a function that takes an sexp_of parameter named after 'a, but 'a is not in
   scope in <type> when handling the constructor A (because A is a gadt constructor).
   Instead the type variables in scope are the ones defined in the return type of A,
   namely 'b. There could be less or more type variable in cases such as:

   type _ less = Less : int less
   type _ more = More : ('a * 'a) more

   If for instance, <type> is ['b * 'c], when we find 'b, we will look for ['b] in the
   renaming and find ['a] (only in that gadt branch, it could be something else in other
   branches), at which point we can call the previously bound sexp_of parameter named
   after 'a.
   If we can't find a resulting name, like when looking up ['c] in the renaming, then we
   assume the variable is existentially quantified and treat it as [_] (which is ok,
   assuming there are no constraints). *)
module Renaming : sig
  type t
  val identity : t

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  val binding_kind : t -> string -> binding_kind

  val of_gadt : string list -> constructor_declaration -> t
end = struct
  type ('a, 'b) result = Ok of 'a | Error of 'b
  type t = (string, Location.error) result StringMap.t option

  let identity = None

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  let binding_kind t var =
    match t with
    | None -> Universally_bound var
    | Some map ->
      match StringMap.find var map with
      | exception Not_found -> Existentially_bound
      | Ok value    -> Universally_bound value
      | Error error -> raise (Location.Error error)

  (* Return a map translating type variables appearing in the return type of a GADT
     constructor to their name in the type parameter list.

     For instance:

     {[
       type ('a, 'b) t = X : 'x * 'y -> ('x, 'y) t
     ]}

     will produce:

     {[
       "x" -> Ok "a"
       "y" -> Ok "b"
     ]}

     If a variable appears twice in the return type it will map to [Error _]. If a
     variable cannot be mapped to a parameter of the type declaration, it will map to
     [Error] (for instance [A : 'a -> 'a list t]).

     It returns None on user error, to let the typer give the error message *)
  let of_gadt =
    (* Add all type variables of a type to a map. *)
    let add_typevars = object
      inherit [ (string, Location.error) result StringMap.t ] Ast_traverse.fold
        as super
      method! core_type ty map =
        match ty.ptyp_desc with
        | Ptyp_var var ->
          let error =
            Location.error ~loc:ty.ptyp_loc
              "ppx_sexp_conv: variable is not a parameter of the type constructor"
          in
          StringMap.add map ~key:var ~data:(Error error)
        | _ -> super#core_type ty map
    end in

    let aux map tp_name tp_in_return_type =
      match tp_in_return_type.ptyp_desc with
      | Ptyp_var var ->
        let data =
          if StringMap.mem var map then
            let loc = tp_in_return_type.ptyp_loc in
            Error (Location.error ~loc "ppx_sexp_conv: duplicate variable")
          else
            Ok tp_name
        in
        StringMap.add map ~key:var ~data
      | _ ->
        add_typevars#core_type tp_in_return_type map
    in

    fun tps cd ->
      match cd.pcd_res with
      | None -> None
      | Some ty ->
        match ty.ptyp_desc with
        | Ptyp_constr (_, params) ->
          if List.length params <> List.length tps then
            None
          else
            Some (List.fold_left2 tps params ~init:StringMap.empty ~f:aux)
        | _ ->
          None
end

module Fun_or_match = struct
  type t =
    | Fun   of expression
    | Match of case list

  let expr ~loc t =
    match t with
    | Fun f       -> f
    | Match cases -> pexp_function ~loc cases

  let unroll ~loc e t =
    match t with
    | Fun f       -> eapply ~loc f [e]
    | Match cases -> pexp_match ~loc e cases

  let map_tmp_vars ~loc ts =
    let vars = List.mapi ts ~f:(fun i _ -> "v" ^ string_of_int i) in
    let bindings =
      List.map2 vars ts ~f:(fun var fp ->
        let expr =
          match fp with
          | Fun f       -> eapply ~loc f [evar ~loc var]
          | Match cases -> pexp_match ~loc (evar ~loc var) cases
        in
        value_binding ~loc ~pat:(pvar ~loc var) ~expr)
    in
    (bindings,
     List.map vars ~f:(pvar ~loc),
     List.map vars ~f:(evar ~loc))
end

module Sexp_of = struct
  (* Conversion of type paths *)
  let sexp_of_type_id (id : Longident.t Located.t) =
    let txt : Longident.t =
      match id.txt with
      | Lident   s  -> Lident  ("sexp_of_" ^ s)
      | Ldot (p, s) -> Ldot (p, "sexp_of_" ^ s)
      | Lapply _    -> failwith "Ppx_sexp_conv_expander.sexp_of_type_id"
    in
    pexp_ident ~loc:id.loc { id with txt }

  (* Conversion of types *)
  let rec sexp_of_type (renaming : Renaming.t) typ : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type:  _ ] ->
      Fun [%expr  fun _ -> Sexp.Atom "_" ]
    | [%type: [%t? _] sexp_opaque ] ->
      Fun [%expr  Sexplib.Conv.sexp_of_opaque ]
    | { ptyp_desc = Ptyp_tuple tp; _ } -> Match [sexp_of_tuple renaming (loc,tp)]
    | { ptyp_desc = Ptyp_var parm; _ } ->
      begin match Renaming.binding_kind renaming parm with
      | Universally_bound parm -> Fun (evar ~loc ("_of_" ^ parm))
      | Existentially_bound -> sexp_of_type renaming [%type:  _ ]
      end
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      let init = sexp_of_type_id id in
      Fun (List.fold_left args ~init ~f:(fun exp1 tp2 ->
        let exp2 = Fun_or_match.expr ~loc (sexp_of_type renaming tp2) in
        [%expr [%e exp1] [%e exp2]]))
    | { ptyp_desc = Ptyp_arrow (_,_,_); _ } ->
      Fun [%expr  fun _f -> Sexplib.Conv.sexp_of_fun Pervasives.ignore ]
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      sexp_of_variant renaming (loc,row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      sexp_of_poly renaming parms poly_tp
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ }
      ->
      Location.raise_errorf ~loc "unknown type"

  (* Conversion of tuples *)
  and sexp_of_tuple renaming (loc,tps) =
    let fps = List.map ~f:(fun tp -> sexp_of_type renaming tp) tps in
    let bindings, pvars, evars = Fun_or_match.map_tmp_vars ~loc fps in
    let in_expr = [%expr  Sexplib.Sexp.List [%e elist ~loc evars] ] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc pvars --> expr

  (* Conversion of variant types *)

  and sexp_of_variant renaming ((loc,row_fields):(Location.t * row_field list))
    : Fun_or_match.t =
    let item = function
      | Rtag (cnstr,_,true,[]) ->
        ppat_variant ~loc cnstr None -->
        [%expr Sexplib.Sexp.Atom [%e estring ~loc cnstr]]
      | Rtag (cnstr,_,_,[ [%type: [%t? tp] sexp_list] ]) ->
        let cnv_expr = Fun_or_match.expr ~loc (sexp_of_type renaming tp) in
        ppat_variant ~loc cnstr (Some [%pat? l]) -->
        [%expr
          Sexplib.Sexp.List
            ( Sexplib.Sexp.Atom [%e estring ~loc cnstr] ::
              Sexplib.Conv.list_map [%e cnv_expr] l
            )
        ]
      | Rtag (cnstr,_,false,[tp]) ->
        (* work hard to avoid diffs with camlp4... *)
        let cnstr_expr = [%expr Sexplib.Sexp.Atom [%e estring ~loc cnstr] ] in
        let tps =
          match tp.ptyp_desc with
          | Ptyp_tuple tps -> tps
          | _ -> [tp]
        in
        let single_type =
          match tps with | [_] -> true | _::_::_ -> false | [] -> assert false
        in
        if not single_type
        then
          let v1_pat,v1_exp = [%pat? v1],[%expr v1] in
          ppat_variant ~loc cnstr (Some v1_pat) -->
          pexp_let ~loc Nonrecursive [
            value_binding ~loc ~pat:v1_pat ~expr:(
              pexp_match ~loc v1_exp [
                sexp_of_tuple renaming (loc, tps)
              ]
            )] [%expr Sexplib.Sexp.List [%e elist ~loc [cnstr_expr; v1_exp]]]
        else
          let fps = List.map ~f:(fun tp -> sexp_of_type renaming tp) tps in
          let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
          let expr =
            pexp_let ~loc Nonrecursive bindings
              [%expr
                Sexplib.Sexp.List [%e elist ~loc (cnstr_expr :: vars)]
              ]
          in
          let patt =
            match patts with
            | [patt] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          ppat_variant ~loc cnstr (Some patt) --> expr

      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        let call = sexp_of_type_id id in
        ppat_alias ~loc (ppat_type ~loc id) (Location.mkloc "v" loc) -->
        [%expr [%e call] v]
      | Rtag (_,_,true,[_])
      | Rtag (_,_,_,_::_::_) ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rtag/&"

      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _::_); _ } as typ) ->
        let call = Fun_or_match.expr ~loc (sexp_of_type renaming typ) in
        ppat_alias ~loc (ppat_type ~loc id) (Location.mkloc "v" loc) -->
        [%expr [%e call] v]

      | Rinherit _ ->
        Location.raise_errorf ~loc
          "unsupported: sexp_of_variant/Rinherit/non-id" (* impossible?*)

      | Rtag (_,_,false,[]) ->
        assert false
    in
    Match (List.map ~f:item row_fields)

  (* Polymorphic record fields *)

  and sexp_of_poly renaming parms tp =
    assert (renaming = Renaming.identity); (* because this is only for record fields.
                                              Otherwise, we'd need to update the renaming
                                              with the newly bound vars. *)
    let loc = tp.ptyp_loc in
    let bindings =
      let mk_binding parm =
        value_binding ~loc ~pat:(pvar ~loc ("_of_" ^ parm))
          ~expr:[%expr Sexplib.Conv.sexp_of_opaque]
      in
      List.map ~f:mk_binding parms
    in
    match sexp_of_type renaming tp with
    | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | Match matchings ->
      Match
        [ [%pat? arg] -->
          pexp_let ~loc Nonrecursive bindings
            (pexp_match ~loc [%expr arg] matchings)
        ]
end

module Of_sexp = struct

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    [[%pat? Sexplib.Conv_error.No_variant_match _] --> expr]

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc ~match_last matches =
    match match_last, matches with
    | true, [{pc_lhs = _; pc_guard = None; pc_rhs = expr}]
    | _, [{pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = expr}]
      -> expr
    | _ ->
      pexp_match ~loc [%expr atom] matches

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      pstring ~loc cnstr -->
      (match call with
       | `ptag_no_args -> [%expr Sexplib.Conv_error.ptag_no_args _tp_loc _sexp]
       | `ptag_takes_args -> [%expr Sexplib.Conv_error.ptag_takes_args _tp_loc _sexp])
      :: acc
    in
    let exc_no_variant_match =
      [%pat? _] --> [%expr Sexplib.Conv_error.no_variant_match _tp_loc _sexp]
    in
    List.fold_left ~f:coll_structs ~init:[exc_no_variant_match] rev_els

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let split_row_field ~loc (atoms, structs, ainhs, sinhs) row_field =
    match row_field with
    | Rtag (cnstr,_,true,[]) ->
      let tpl = loc, cnstr in
      (
        tpl :: atoms,
        structs,
        `A tpl :: ainhs,
        sinhs
      )
    | Rtag (cnstr,_,false,[tp]) ->
      let loc = tp.ptyp_loc in
      (
        atoms,
        (loc, cnstr) :: structs,
        ainhs,
        `S (loc, cnstr, tp) :: sinhs
      )
      | Rinherit inh ->
        let iinh = `I inh in
        (
          atoms,
          structs,
          iinh :: ainhs,
          iinh :: sinhs
        )
      | Rtag (_,_,true,[_])
      | Rtag (_,_,_,_::_::_) ->
        Location.raise_errorf ~loc "split_row_field/&"
      | Rtag (_,_,false,[]) ->
        assert false

  (* Conversion of type paths *)
  let path_of_sexp_fun ?(internal=false) (id : Longident.t Located.t) =
    let map_name s =
      let s = s ^ "_of_sexp" in
      if internal then "__" ^ s ^ "__" else s
    in
    let txt : Longident.t =
      match id.txt with
      | Lident   s  -> Lident  (map_name s)
      | Ldot (p, s) -> Ldot (p, map_name s)
      | Lapply _    -> failwith "Ppx_sexp_conv.Generate_of_sexp.path_of_sexp_fun"
    in
    pexp_ident ~loc:id.loc { id with txt }

  (* Conversion of types *)
  let rec type_of_sexp ?(internal=false) typ : Fun_or_match.t =
    let loc = typ.ptyp_loc in
    match typ with
    | [%type: [%t? _] sexp_opaque ]
    | [%type: _ ] ->
      Fun [%expr  Sexplib.Conv.opaque_of_sexp ]
    (*| [%type: sexp_option ] -> (* will never match surely! *)
      Fun [%expr  fun a_of_sexp v -> Some (a_of_sexp v) ]*)
    | [%type: [%t? ty1] sexp_list ] ->
      let arg1 = Fun_or_match.expr ~loc (type_of_sexp ty1) in
      Fun [%expr (fun a_of_sexp v -> Sexplib.Conv.list_of_sexp  a_of_sexp v) [%e arg1]]
    | [%type: [%t? ty1] sexp_array ] ->
      let arg1 = Fun_or_match.expr ~loc (type_of_sexp ty1) in
      Fun [%expr (fun a_of_sexp v -> Sexplib.Conv.array_of_sexp a_of_sexp v) [%e arg1] ]
    | { ptyp_desc = Ptyp_tuple tp; _ } -> Match (tuple_of_sexp (loc,tp))
    | { ptyp_desc = Ptyp_var parm; _ } -> Fun (evar ~loc ("_of_" ^ parm))
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      let init = path_of_sexp_fun ~internal id in
      let args = List.map args ~f:(fun arg -> Fun_or_match.expr ~loc (type_of_sexp arg)) in
      Fun (eapply ~loc init args)

    | { ptyp_desc = Ptyp_arrow (_,_,_); _ } -> Fun [%expr  Sexplib.Conv.fun_of_sexp ]
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
        variant_of_sexp ?full_type:None (loc,row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } -> poly_of_sexp parms poly_tp
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ }
      -> Location.raise_errorf ~loc "unknown type"

  (* Conversion of tuples *)
  and tuple_of_sexp (loc,tps) =
    let fps = List.map ~f:type_of_sexp tps in
    let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
    let n = List.length fps in
    [ [%pat? Sexplib.Sexp.List [%p plist ~loc patts]] -->
      pexp_let ~loc Nonrecursive bindings
        (pexp_tuple ~loc vars)
    ; [%pat? sexp] -->
      [%expr Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
               [%e eint ~loc n]
               sexp]
    ]

  (* Generate internal call *)
  and mk_internal_call ~loc row_field : Fun_or_match.t =
    match row_field with
    | Rtag (cnstr,_,true,[]) ->
      Fun (path_of_sexp_fun ~internal:true (Located.lident ~loc cnstr))
    | Rtag (cnstr,_,false,[tp]) ->
      let loc = tp.ptyp_loc in
      let init = path_of_sexp_fun ~internal:true (Located.lident ~loc cnstr) in
      let args =
        (match tp.ptyp_desc with
         | Ptyp_tuple tps -> tps
         | _ -> [tp])
        |> List.map ~f:(fun arg -> Fun_or_match.expr ~loc (type_of_sexp arg)
      ) in
      Fun (eapply ~loc init args)

    | Rtag (_,_,true,[_])
    | Rtag (_,_,_,_::_::_) ->
      Location.raise_errorf ~loc "mk_internal_call/&"
    | Rtag (_,_,false,[]) ->
      assert false
    | Rinherit typ -> type_of_sexp ~internal:true typ

  (* Generate code for matching included variant types *)
  and handle_variant_inh full_type ~match_last other_matches inh =
    let loc = inh.ptyp_loc in
    let func_expr = mk_internal_call ~loc (Rinherit inh) in
    let app : Fun_or_match.t =
      let fun_expr = Fun_or_match.expr ~loc func_expr in
      Fun [%expr [%e fun_expr] _sexp]
    in
    let match_exc =
      handle_no_variant_match loc (
        handle_variant_match_last loc ~match_last other_matches) in
    let new_other_matches =
      [ [%pat? _] -->
        pexp_try ~loc
          [%expr ([%e Fun_or_match.expr ~loc app]
                  :> [%t replace_variables_by_underscores full_type])]
          match_exc
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
          let new_match = pstring ~loc cnstr --> pexp_variant ~loc cnstr None in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type ~match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_structs `ptag_takes_args
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs in
    handle_variant_match_last loc ~match_last match_atoms_inhs

  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match ~loc ~is_variant cnstr tps =
    let cnstr vars_expr =
      if is_variant
      then pexp_variant ~loc cnstr (Some vars_expr)
      else pexp_construct ~loc (Located.lident ~loc cnstr) (Some vars_expr)
    in
    match tps with
    | [ [%type: [%t? tp] sexp_list ] ] ->
      let cnv = Fun_or_match.expr ~loc (type_of_sexp tp) in
      cnstr [%expr  Sexplib.Conv.list_map ([%e cnv]) sexp_args ]
    | _ ->
      let bindings,patts,good_arg_match =
        let fps = List.map ~f:type_of_sexp tps in
        let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
        let good_arg_match =
          let vars_expr =
            match vars with
            | [var_expr] -> var_expr
            | _ -> pexp_tuple ~loc vars
          in
          cnstr vars_expr
        in
        bindings,patts,good_arg_match
      in
      [%expr
        match sexp_args with
        | [%p plist ~loc patts] ->
          [%e pexp_let ~loc Nonrecursive bindings good_arg_match]
        | _ ->
          [%e
            if is_variant
            then [%expr Sexplib.Conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp]
            else [%expr Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp]]
      ]

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tp) ->
        has_structs_ref := true;
        let expr =
          mk_cnstr_args_match ~loc:tp.ptyp_loc ~is_variant:true cnstr [tp]
        in
        let new_match = [%pat? ([%p pstring ~loc cnstr] as _tag)] --> expr in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh full_type ~match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_atoms `ptag_no_args
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    (
      handle_variant_match_last loc ~match_last match_structs_inhs,
      !has_structs_ref
    )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag loc full_type row_field_list =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:(split_row_field ~loc) ~init:([], [], [], []) row_field_list
    in
    let match_struct, has_structs =
      mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms in
    let maybe_sexp_args_patt =
      if has_structs then [%pat?  sexp_args ]
      else [%pat?  _ ]
    in
    [ [%pat? Sexplib.Sexp.Atom atom as _sexp] -->
      mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs
    ; [%pat? Sexplib.Sexp.List
             (Sexplib.Sexp.Atom atom :: [%p maybe_sexp_args_patt]) as _sexp] -->
      match_struct
    ; [%pat? Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp] -->
      [%expr Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp]
    ; [%pat? Sexplib.Sexp.List [] as sexp] -->
      [%expr Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp]
    ]

  (* Generate matching code for variants *)
  and variant_of_sexp ?full_type (loc,row_fields) =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
        Rinherit _ as inh :: rest ->
        let rec loop inh row_fields =
          let call =
            [%expr  ( [%e Fun_or_match.expr ~loc (mk_internal_call ~loc inh)] sexp :>
                        [%t replace_variables_by_underscores full_type] ) ]
          in
          match row_fields with
          | [] -> call
          | h :: t ->
            let expr =
              match h with
              | Rinherit _ -> loop h t
              | _ ->
                let rftag_matches =
                  handle_variant_tag loc full_type row_fields
                in
                pexp_match ~loc [%expr sexp] rftag_matches
            in
            pexp_try ~loc call
              (handle_no_variant_match loc expr)
        in
        [ [%pat? sexp] --> loop inh rest ]
      | _ :: _ -> handle_variant_tag loc full_type row_fields
      | [] -> assert false  (* impossible *)
    in
    if is_contained then
      Fun
        [%expr
          fun sexp ->
            try [%e pexp_match ~loc [%expr sexp] top_match]
            with
              Sexplib.Conv_error.No_variant_match (_tp_loc, sexp) ->
              Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
        ]
    else Match top_match

  and poly_of_sexp parms tp =
    let loc = tp.ptyp_loc in
    let bindings =
      let mk_binding parm =
        value_binding ~loc ~pat:(pvar ~loc ("_of_" ^ parm))
          ~expr:[%expr fun sexp -> Sexplib.Conv_error.record_poly_field_value _tp_loc sexp]
      in
      List.map ~f:mk_binding parms
    in
    match type_of_sexp tp with
    | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | Match matchings ->
      Match
        [ [%pat? arg] -->
          pexp_let ~loc Nonrecursive bindings
            (pexp_match ~loc [%expr arg] matchings)
        ]
end

let sexp_of ty =
  Sexp_of.sexp_of_type Renaming.identity ty
  |> Fun_or_match.expr ~loc:ty.ptyp_loc
;;

module Internal = struct
  module Renaming     = Renaming
  module Fun_or_match = Fun_or_match

  let sexp_of_type                     = Sexp_of.sexp_of_type
  let sexp_of_variant                  = Sexp_of.sexp_of_variant
  let type_of_sexp                     = fun ty -> Of_sexp.type_of_sexp ty
  let variant_of_sexp                  = Of_sexp.variant_of_sexp
  let mk_cnstr_args_match              = Of_sexp.mk_cnstr_args_match
  let replace_variables_by_underscores = replace_variables_by_underscores
end
