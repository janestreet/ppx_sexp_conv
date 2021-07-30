open! Base
open! Ppxlib
open Ast_builder.Default
open Helpers

type t =
  | Fun of expression
  | Match of cases

let of_cases cases = Match cases
let of_expression expr = Fun expr

let to_function ~loc cases =
  match cases with
  | [] ->
    pexp_function
      ~loc
      [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(pexp_unreachable ~loc) ]
  | [ { pc_lhs; pc_guard = None; pc_rhs } ] -> eabstract ~loc [ pc_lhs ] pc_rhs
  | _ -> pexp_function ~loc cases
;;

let to_expression t ~loc =
  match t with
  | Fun f -> f
  | Match cases -> to_function ~loc cases
;;

let to_lambda_expression ?(var = "x") t ~loc =
  match t with
  | Fun exp -> eabstract ~loc [ pvar ~loc var ] (eapply ~loc exp [ evar ~loc var ])
  | Match cases -> to_function ~loc cases
;;

let apply t ~loc e =
  match t with
  | Fun f -> eapply ~loc f [ e ]
  | Match cases -> pexp_match ~loc e cases
;;

let bind ?(var = "x") t ~loc rec_flag bindings =
  match t with
  | Fun fun_expr -> Fun (pexp_let ~loc rec_flag bindings fun_expr)
  | Match matchings ->
    Match
      [ pvar ~loc var
        --> pexp_let ~loc rec_flag bindings (pexp_match ~loc (evar ~loc var) matchings)
      ]
;;

let map_tmp_vars ~loc ts =
  let vars = List.mapi ts ~f:(fun i _ -> "v" ^ Int.to_string i) in
  let bindings =
    List.map2_exn vars ts ~f:(fun var t ->
      let expr = apply ~loc t (evar ~loc var) in
      value_binding ~loc ~pat:(pvar ~loc var) ~expr)
  in
  bindings, List.map vars ~f:(pvar ~loc), List.map vars ~f:(evar ~loc)
;;
