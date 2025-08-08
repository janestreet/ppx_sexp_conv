open! Stdppx
open! Ppxlib
open Ast_builder.Default
open Helpers

let maybe_exclave ~loc expr ~stackify =
  match stackify with
  | false -> expr
  | true -> [%expr [%e expr]]
;;

let maybe_constrain ~loc expr = function
  | None -> expr
  | Some cstr -> [%expr ([%e expr] : [%t cstr])]
;;

module Reference = struct
  type t =
    { types : type_declaration list
    ; binds : value_binding list list
    ; ident : longident_loc
    ; cstr : core_type option
    ; args : (arg_label * expression) list
    ; after_args : (arg_label * expression) list
    }

  let bind t binds = { t with binds = binds :: t.binds }
  let bind_types t types = { t with types = types @ t.types }

  let maybe_apply { types; binds; ident; cstr; args; after_args } ~loc maybe_arg =
    let ident = pexp_ident ~loc ident in
    let args =
      match maybe_arg with
      | None -> args @ after_args
      | Some arg -> args @ [ Nolabel, arg ] @ after_args
    in
    let expr =
      match args with
      | [] -> maybe_constrain ~loc ident cstr
      | _ -> pexp_apply ~loc ident args
    in
    with_types ~loc ~types (with_let ~loc ~binds expr)
  ;;

  let apply t ~loc arg = maybe_apply t ~loc (Some arg)
  let to_expression t ~loc = maybe_apply t ~loc None

  let to_value_expression t ~loc ~rec_flag ~values_being_defined ~stackify =
    let may_refer_directly_to ident =
      match rec_flag with
      | Nonrecursive -> true
      | Recursive -> not (String.Set.mem (Longident.name ident.txt) values_being_defined)
    in
    match t with
    | { types = []; binds = []; ident; cstr; args = []; after_args = [] }
      when may_refer_directly_to ident ->
      maybe_constrain ~loc (pexp_ident ~loc ident) cstr
    | _ -> fresh_lambda ~loc (fun ~arg -> maybe_exclave ~loc (apply t ~loc arg) ~stackify)
  ;;
end

module Lambda = struct
  type t =
    { types : type_declaration list
    ; binds : value_binding list list
    ; cases : cases
    }

  let bind t binds = { t with binds = binds :: t.binds }
  let bind_types t types = { t with types = types @ t.types }

  (* generic case: use [function] or [match] *)
  let maybe_apply_generic ~loc ~types ~binds maybe_arg cases ~stackify =
    let expr =
      match maybe_arg with
      | None ->
        let cases =
          List.map cases ~f:(fun case ->
            { case with pc_rhs = maybe_exclave ~loc case.pc_rhs ~stackify })
        in
        pexp_function ~loc cases
      | Some arg -> pexp_match ~loc arg cases
    in
    with_types ~loc ~types (with_let ~loc ~binds expr)
  ;;

  (* zero cases: synthesize an "impossible" case, i.e. [| _ -> .] *)
  let maybe_apply_impossible ~loc ~types ~binds maybe_arg ~stackify =
    [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(pexp_unreachable ~loc) ]
    |> maybe_apply_generic ~loc ~binds ~types maybe_arg ~stackify
  ;;

  (* one case without guard: use [fun] or [let] *)
  let maybe_apply_simple ~loc ~types ~binds maybe_arg pat body ~stackify =
    let expr =
      match maybe_arg with
      | None -> pexp_fun ~loc Nolabel None pat (maybe_exclave ~loc body ~stackify)
      | Some arg -> pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:arg ] body
    in
    with_types ~loc ~types (with_let ~loc ~binds expr)
  ;;

  (* shared special-casing logic for [apply] and [to_expression] *)
  let maybe_apply t ~loc maybe_arg ~stackify =
    match t with
    | { types; binds; cases = [] } ->
      maybe_apply_impossible ~loc ~types ~binds maybe_arg ~stackify
    | { types; binds; cases = [ { pc_lhs; pc_guard = None; pc_rhs } ] } ->
      maybe_apply_simple ~loc ~types ~binds maybe_arg pc_lhs pc_rhs ~stackify
    | { types; binds; cases } ->
      maybe_apply_generic ~loc ~types ~binds maybe_arg cases ~stackify
  ;;

  let apply t ~loc arg = maybe_apply t ~loc (Some arg) ~stackify:false
  let to_expression t ~loc ~stackify = maybe_apply t ~loc None ~stackify

  let to_value_expression t ~loc ~stackify =
    match t with
    | { types = []; binds = []; cases = _ } ->
      (* lambdas without [let] are already values *)
      let expr = to_expression t ~loc ~stackify in
      assert (is_value_expression expr);
      expr
    | _ -> fresh_lambda ~loc (fun ~arg -> maybe_exclave ~loc (apply t ~loc arg) ~stackify)
  ;;
end

type t =
  | Reference of Reference.t
  | Lambda of Lambda.t

let of_lambda cases = Lambda { types = []; binds = []; cases }

let of_reference_exn ~thunk expr =
  let loc = expr.pexp_loc in
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc with
  | Pexp_ident ident ->
    Reference { types = []; binds = []; ident; cstr = None; args = []; after_args = [] }
  | Pexp_constraint ({ pexp_desc = Pexp_ident ident; _ }, cstr, _) ->
    Reference { types = []; binds = []; ident; cstr; args = []; after_args = [] }
  | Pexp_apply ({ pexp_desc = Pexp_ident ident; _ }, args) ->
    Reference
      { types = []
      ; binds = []
      ; ident
      ; cstr = None
      ; args
      ; after_args = (if thunk then [ Nolabel, [%expr ()] ] else [])
      }
  | _ ->
    Location.raise_errorf
      ~loc:expr.pexp_loc
      "ppx_sexp_conv: internal error.\n\
       [Conversion.of_reference_exn] expected an identifier possibly applied to arguments.\n\
       Instead, got:\n\
       %s"
      (Pprintast.string_of_expression expr)
;;

let to_expression t ~loc ~stackify =
  match t with
  | Reference reference -> Reference.to_expression ~loc reference
  | Lambda lambda -> Lambda.to_expression ~loc lambda ~stackify
;;

let to_value_expression t ~loc ~rec_flag ~values_being_defined ~stackify =
  match t with
  | Reference reference ->
    Reference.to_value_expression ~loc ~rec_flag ~values_being_defined reference ~stackify
  | Lambda lambda -> Lambda.to_value_expression ~loc lambda ~stackify
;;

let apply t ~loc e =
  match t with
  | Reference reference -> Reference.apply ~loc reference e
  | Lambda lambda -> Lambda.apply ~loc lambda e
;;

let bind t binds =
  match t with
  | Reference reference -> Reference (Reference.bind reference binds)
  | Lambda lambda -> Lambda (Lambda.bind lambda binds)
;;

let bind_types t types =
  match t with
  | Reference reference -> Reference (Reference.bind_types reference types)
  | Lambda lambda -> Lambda (Lambda.bind_types lambda types)
;;

module Apply_all = struct
  type t =
    { bindings : value_binding list
    ; arguments : pattern list
    ; converted : expression list
    }
end

let gen_symbols list ~prefix =
  List.mapi list ~f:(fun i _ -> gen_symbol ~prefix:(prefix ^ Int.to_string i) ())
;;

let zip list1 list2 =
  List.fold_right2 list1 list2 ~init:[] ~f:(fun x y acc -> (x, y) :: acc)
;;

let apply_all ts ~loc =
  let arguments_names = gen_symbols ts ~prefix:"arg" in
  let converted_names = gen_symbols ts ~prefix:"res" in
  let bindings =
    List.map
      (zip ts (zip arguments_names converted_names))
      ~f:(fun (t, (arg, conv)) ->
        let expr = apply ~loc t (evar ~loc arg) in
        value_binding ~loc ~pat:(pvar ~loc conv) ~expr)
  in
  ({ bindings
   ; arguments = List.map arguments_names ~f:(pvar ~loc)
   ; converted = List.map converted_names ~f:(evar ~loc)
   }
   : Apply_all.t)
;;
