open! Stdppx
open Ppxlib
open Ast_builder.Default

type 'a t =
  { value_bindings : value_binding list
  ; body : 'a
  }

let return body = { value_bindings = []; body }

let bind a ~f =
  let b = f a.body in
  { value_bindings = a.value_bindings @ b.value_bindings; body = b.body }
;;

let map a ~f = { a with body = f a.body }

module Monad_infix = struct
  let ( >>| ) a f = map a ~f
  let ( >>= ) a f = bind a ~f
end

open Monad_infix

let all list =
  List.fold_right list ~init:(return []) ~f:(fun head tail ->
    head >>= fun head -> tail >>| fun tail -> head :: tail)
;;

let create ~loc ~prefix ~ty rhs =
  let name = gen_symbol ~prefix () in
  let lhs = pvar ~loc name in
  let body = evar ~loc name in
  let ty, rhs, body =
    if Helpers.is_value_expression rhs
    then ty, rhs, body
    else (
      (* Thunkify the value to evaluate when referred to. *)
      let ty = [%type: Stdlib.Unit.t -> [%t ty]] in
      let rhs = [%expr fun () -> [%e rhs]] in
      let body = [%expr [%e body] ()] in
      ty, rhs, body)
  in
  { value_bindings = [ value_binding ~loc ~pat:(ppat_constraint ~loc lhs ty) ~expr:rhs ]
  ; body
  }
;;

let let_bind_user_expressions { value_bindings; body } ~loc =
  if List.is_empty value_bindings
  then body
  else pexp_let ~loc Nonrecursive value_bindings body
;;
