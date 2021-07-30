open! Base
open! Ppxlib

type error = string Loc.t
type t = (string, error) Result.t Map.M(String).t option

let identity = None

type binding_kind =
  | Universally_bound of string
  | Existentially_bound

let add_universally_bound (t : t) name : t =
  let name = name.txt in
  match t with
  | None -> None
  | Some map -> Some (Map.set ~key:name ~data:(Ok name) map)
;;

let binding_kind t var =
  match t with
  | None -> Universally_bound var
  | Some map ->
    (match Map.find map var with
     | None -> Existentially_bound
     | Some (Ok value) -> Universally_bound value
     | Some (Error { loc; txt }) -> Location.raise_errorf ~loc "%s" txt)
;;

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
  let add_typevars =
    object
      inherit [(string, error) Result.t Map.M(String).t] Ast_traverse.fold as super

      method! core_type ty map =
        match ty.ptyp_desc with
        | Ptyp_var var ->
          let error =
            { loc = ty.ptyp_loc
            ; txt = "ppx_sexp_conv: variable is not a parameter of the type constructor"
            }
          in
          Map.set map ~key:var ~data:(Error error)
        | _ -> super#core_type ty map
    end
  in
  let aux map tp_name tp_in_return_type =
    match tp_in_return_type.ptyp_desc with
    | Ptyp_var var ->
      let data =
        if Map.mem map var
        then (
          let loc = tp_in_return_type.ptyp_loc in
          Error { loc; txt = "ppx_sexp_conv: duplicate variable" })
        else Ok tp_name
      in
      Map.set map ~key:var ~data
    | _ -> add_typevars#core_type tp_in_return_type map
  in
  fun tps cd ->
    match cd.pcd_res with
    | None -> None
    | Some ty ->
      (match ty.ptyp_desc with
       | Ptyp_constr (_, params) ->
         if List.length params <> List.length tps
         then None
         else
           Some
             (Stdlib.ListLabels.fold_left2
                tps
                params
                ~init:(Map.empty (module String))
                ~f:aux)
       | _ -> None)
;;
