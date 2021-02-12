open! Base
open! Ppxlib

val type_extension : core_type -> core_type
val core_type : loc:Location.t -> path:string -> core_type -> expression

val sig_type_decl
  :  loc:Location.t
  -> path:string
  -> rec_flag * type_declaration list
  -> signature

val str_type_decl
  :  loc:Location.t
  -> path:string
  -> rec_flag * type_declaration list
  -> structure
