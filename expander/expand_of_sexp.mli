open! Stdppx
open! Ppxlib

module Sig_generate_of_sexp : sig
  (** Given a type, produce the type of its [of_sexp] conversion. *)
  val type_of_of_sexp : loc:location -> core_type -> core_type

  (** Derive an [of_sexp] interface for a list of type declarations. *)
  val mk_sig
    :  poly:bool
    -> loc:location
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> portable:bool
    -> signature_item list
end

module Str_generate_of_sexp : sig
  (** Given a type, produce a pattern for that type's [of_sexp] conversion. *)
  val pat_of_of_sexp : loc:location -> core_type -> pattern

  (** Given a type, produce its [of_sexp] conversion. *)
  val core_type_of_sexp : path:string -> core_type -> expression

  (** Derive an [of_sexp] implementation for a list of type declarations. *)
  val tds_of_sexp
    :  loc:location
    -> poly:bool
    -> path:string
    -> portable:bool
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> structure_item list
end
