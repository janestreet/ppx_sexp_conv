open! Base
open! Ppxlib

type t =
  | Gadt of (string, string loc) Result.t Map.M(String).t
  | Non_gadt

let non_gadt = Non_gadt

module Binding_kind = struct
  type t =
    | Universally_bound of string
    | Existentially_bound
end

let add_universally_bound (t : t) name : t =
  let name = name.txt in
  match t with
  | Non_gadt -> Non_gadt
  | Gadt map -> Gadt (Map.set ~key:name ~data:(Ok name) map)
;;

let binding_kind t var : Binding_kind.t =
  match t with
  | Non_gadt -> Universally_bound var
  | Gadt map ->
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

   {v
     "x" -> Ok "a"
     "y" -> Ok "b"
   v}

   If a variable appears twice in the return type it will map to [Error _]. If a
   variable cannot be mapped to a parameter of the type declaration, it will map to
   [Error] (for instance [A : 'a -> 'a list t]).

   It returns [Non_gadt] on user error, to let the typer give the error message *)
let of_constructor_declaration =
  (* Add all type variables of a type to a map. *)
  let add_typevars =
    object
      inherit [(string, string loc) Result.t Map.M(String).t] Ast_traverse.fold as super

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
  fun cd ~type_parameters:tps ->
    match cd.pcd_res with
    | None -> Non_gadt
    | Some ty ->
      (match ty.ptyp_desc with
       | Ptyp_constr (_, params) ->
         if List.length params <> List.length tps
         then Non_gadt
         else
           Gadt
             (Stdlib.ListLabels.fold_left2
                tps
                params
                ~init:(Map.empty (module String))
                ~f:aux)
       | _ -> Non_gadt)
;;
