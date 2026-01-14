open! Stdppx
open! Ppxlib

module Sig_generate_sexp_of : sig
  (** Given a type, produce the type of its [sexp_of] conversion. *)
  val type_of_sexp_of : loc:location -> core_type -> stackify:bool -> core_type

  (** Derive a [sexp_of] interface for a list of type declarations. *)
  val mk_sig
    :  loc:location
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> stackify:bool
    -> portable:bool
    -> signature_item list

  (** Derive a [sexp_of] interface for an exception declaration. *)
  val mk_sig_exn : loc:location -> path:string -> type_exception -> signature_item list
end

module Str_generate_sexp_of : sig
  (** Given a type, produce a pattern for that type's [sexp_of] conversion. *)
  val pat_of_sexp_of : loc:location -> core_type -> stackify:bool -> pattern

  (** Given a type, produce its [sexp_of] conversion. *)
  val sexp_of_core_type : core_type -> stackify:bool -> expression

  (** Derive a [sexp_of] implementation for a list of type declarations. *)
  val sexp_of_tds
    :  loc:location
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> stackify:bool
    -> portable:bool
    -> structure_item list

  (** Derive a [sexp_of] implementation for an exception declaration.

      If passed [~nonportable_magic:true], adds [Obj.magic_portable] around the generated
      function before registering it with [Sexplib0]. This happens when the user derives
      [[@@deriving sexp ~nonportable__magic_unsafe_in_parallel_programs]]. *)
  val sexp_of_exn
    :  loc:location
    -> path:string
    -> nonportable_magic:bool
    -> type_exception
    -> structure_item list
end
