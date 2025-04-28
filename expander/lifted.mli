open! Stdppx
open Ppxlib

(** Represents an ['a], along with some user expressions that should lifted out of the
    scope of internal bindings. For example, if a user writes [[@@default x]], they mean
    [x] in the surface code, not some temporary variable [x] added by ppx machinery. *)
type 'a t

(** As a monad, combines all client expressions so they can be lifted to the outermost
    level of generated code. *)

val return : 'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val all : 'a t list -> 'a list t

module Monad_infix : sig
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(** Lifts the given expression and binds it to a fresh variable starting with [prefix].
    The expression is evaluated each time it is referred to. The binding is annotated with
    [ty]. Uses [loc] for generated code. *)
val create : loc:location -> prefix:string -> ty:core_type -> expression -> expression t

(** Uses [let] to bind all lifted user expressions, with the contained expression as the
    body. Should be called in whatever scope the user should be able to refer to. *)
val let_bind_user_expressions : expression t -> loc:location -> expression
