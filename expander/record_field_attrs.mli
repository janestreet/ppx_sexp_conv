open! Stdppx
open! Ppxlib

module Generic : sig
  type nullary = { must_type_check : unit Lifted.t }

  type unary =
    { param : core_type
    ; must_type_check : unit Lifted.t
    }

  type 'specific t =
    | Omit_nil
    | Sexp_array of unary
    | Sexp_bool of nullary
    | Sexp_list of unary
    | Sexp_option of unary
    | Sexp_or_null of unary
    | Specific of 'specific
end

module Of_sexp : sig
  type t =
    | Default of expression Lifted.t
    | Required

  val create : loc:Location.t -> label_declaration -> t Generic.t
end

module Sexp_of : sig
  module Drop : sig
    type t =
      | Compare of { local : bool }
      | Equal of { local : bool }
      | Sexp
      | Func of expression Lifted.t
  end

  type t =
    | Drop_default of Drop.t
    | Drop_if of expression Lifted.t
    | Keep

  val create : loc:Location.t -> stackify:bool -> label_declaration -> t Generic.t
end

(** Lift the contents of [Attrs.default]. *)
val lift_default : loc:location -> label_declaration -> expression -> expression Lifted.t
