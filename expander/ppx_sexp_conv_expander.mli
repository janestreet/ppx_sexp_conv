open Ppxlib
module Attrs = Attrs
module Record_field_attrs = Record_field_attrs

module Sexp_of : sig
  val type_extension : core_type -> stackify:bool -> core_type
  val pattern_extension : core_type -> stackify:bool -> pattern
  val core_type : core_type -> stackify:bool -> expression

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> stackify:bool
    -> portable:bool
    -> signature_item list

  val sig_exception
    :  loc:Location.t
    -> path:string
    -> type_exception
    -> signature_item list

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> stackify:bool
    -> portable:bool
    -> structure

  val str_exception
    :  loc:Location.t
    -> path:string
    -> nonportable_magic:bool
    -> type_exception
    -> structure
end

module Of_sexp : sig
  val type_extension : core_type -> core_type
  val pattern_extension : core_type -> pattern
  val core_type : path:string -> core_type -> expression

  val sig_type_decl
    :  poly:bool
    -> loc:Location.t
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> portable:bool
    -> signature_item list

  val str_type_decl
    :  loc:Location.t
    -> poly:bool (** the type is annotated with sexp_poly instead of sexp *)
    -> path:string (** the module path within the file *)
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> portable:bool
    -> structure
end

module Sexp_grammar : sig
  val type_extension : ctxt:Expansion_context.Extension.t -> core_type -> core_type
  val pattern_extension : ctxt:Expansion_context.Extension.t -> core_type -> pattern

  val core_type
    :  tags_of_doc_comments:bool
    -> ctxt:Expansion_context.Extension.t
    -> core_type
    -> expression

  val sig_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> nonportable:bool
    -> signature_item list

  val str_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> bool (** [true] means capture doc comments as tags *)
    -> structure
end

module Sig_sexp : sig
  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> unboxed:bool
    -> rec_flag * type_declaration list
    -> stackify:bool
    -> portable:bool
    -> signature_item list
end
