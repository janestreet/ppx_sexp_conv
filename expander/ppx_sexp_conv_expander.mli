open Parsetree

(** [sexp_of ty] is an expression of type [ty -> Sexp.t] *)
val sexp_of : core_type -> expression

(**/**)

module Internal : sig
  module Renaming : sig
    type t
    val identity : t
    type binding_kind =
      | Universally_bound of string
      | Existentially_bound
    val binding_kind : t -> string -> binding_kind
    val of_gadt : string list -> constructor_declaration -> t
  end

  module Fun_or_match : sig
    type t =
      | Fun   of expression
      | Match of case list

    val expr : loc:Location.t -> t -> expression
    val unroll : loc:Location.t -> expression -> t -> expression
    val map_tmp_vars
      :  loc:Location.t
      -> t list
      -> value_binding list * pattern list * expression list
  end

  val sexp_of_type    : Renaming.t -> core_type                     -> Fun_or_match.t
  val sexp_of_variant : Renaming.t -> (Location.t * row_field list) -> Fun_or_match.t

  val type_of_sexp : core_type -> Fun_or_match.t
  val variant_of_sexp
    :  ?full_type:core_type
    -> (Location.t * row_field list)
    -> Fun_or_match.t

  val mk_cnstr_args_match
    :  loc:Location.t
    -> is_variant:bool
    -> string
    -> core_type list
    -> expression

  val replace_variables_by_underscores : core_type -> core_type
end
