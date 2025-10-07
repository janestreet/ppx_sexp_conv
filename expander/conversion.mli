open! Stdppx
open! Ppxlib

(** Sexp conversion function, expressed as either a single expression or as a collection
    of [match] cases. Expressing as cases rather than wrapping directly in [pexp_function]
    allows us to simplify some expressions built on this. *)
type t

(** Construct [t] from a list of pattern/expression cases. *)
val of_lambda : cases -> t

(** Construct [t] from an identifier, possibly applied to arguments. Raise on any other
    form of expression.

    If [thunk], then [expression] should evaluate to something with type [() -> 'a], and
    [of_reference_exn ~thunk:true |> to_value_expression] will evaluate to something of
    type ['a]. [thunk] should only ever be set when working with unboxed types, as this is
    a trick for circumventing the lack of layout polymorphism ([() -> 'a] has layout value
    even if ['a] is unboxed). *)
val of_reference_exn : thunk:bool -> expression -> t

(** Convert [t] to an expression. *)
val to_expression : t -> loc:location -> stackify:bool -> expression

(** Convert [t] to an expression that is a syntactic value, i.e. a constant, identifier,
    or lambda expression that does no "work", can can be preallocated, and works in the
    context of a [let rec]. *)
val to_value_expression
  :  t
  -> loc:location
  -> rec_flag:rec_flag
  -> values_being_defined:String.Set.t
  -> stackify:bool
  -> expression

(** Apply [t] to an argument. *)
val apply
  :  t
  -> loc:location
  -> expression (** argument [t] is applied to *)
  -> expression

(** Wrap [t] in [let]-bindings. *)
val bind : t -> value_binding list -> t

(** Wrap [t] in [let open .. in] with type declarations. *)
val bind_types : t -> type_declaration list -> t

module Apply_all : sig
  type t =
    { bindings : value_binding list
    ; arguments : pattern list
    ; converted : expression list
    }
end

(** Applies each [t] to a fresh variable, and binds the results to fresh variables.
    Returns the corresponding [value_binding]s, patterns for the argument variables, and
    expressions for the result variables. *)
val apply_all : t list -> loc:location -> Apply_all.t
