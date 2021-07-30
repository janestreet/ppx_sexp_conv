open! Base
open! Ppxlib

(** Sexp conversion function, expressed as either a single expression or as a collection
    of [match] cases. Expressing as cases rather than wrapping directly in [pexp_function]
    allows us to simplify some expressions built on this. *)
type t

(** Construct [t] from a list of pattern/expression cases. *)
val of_cases : cases -> t

(** Construct [t] from an expression representing a function. *)
val of_expression : expression -> t

(** Convert [t] to an expression. *)
val to_expression : t -> loc:location -> expression

(** Convert [t] to a syntactic [fun] or [function] expression *)
val to_lambda_expression : ?var:string -> t -> loc:location -> expression

(** Apply [t] to an argument. *)
val apply
  :  t
  -> loc:location
  -> expression (** argument [t] is applied to *)
  -> expression

(** Wrap [t] in [let]-bindings. *)
val bind : ?var:string -> t -> loc:location -> rec_flag -> value_binding list -> t

(** Bind [t]s to temporary variable names. Variables are distinct from each other but do
    not attempt to be globally unique. Produces bindings for each [t], and corresponding
    lists of patterns and expressions representing each of the temporary variables. *)
val map_tmp_vars
  :  loc:location
  -> t list
  -> value_binding list * pattern list * expression list

