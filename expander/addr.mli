open! Stdppx
open! Ppxlib

(** The information about a specific [Addr] module from [Ox] that is needed to register
    and interpret the corresponding [[@sexp.addr*]] attribute. *)
type t

val all : t list

(** If [hum:true], elides the [@] used when registering the attribute with ppxlib. (See
    the documentation of {!Ppxlib.Attribute.declare} for the meaning of [@].) *)
val attr_name : hum:bool -> t -> string

(** The name of the type [t] de-sexps, as it should be printed for humans in error
    messages (e.g. [_ Ox.Addr.Ext.t]). *)
val common_type_name : t -> string

(** The runtime type [t] de-sexps, as it is produced for generated code (e.g.
    [_ Ppx_ox_runtime.Addr.Ext.t]). *)
val runtime_type : loc:Location.t -> t -> core_type -> core_type

(** The runtime function for loading the value at a [t], as it is produced for generated
    code (e.g. [Ppx_ox_runtime.Addr.Ext.get]). *)
val runtime_get : loc:Location.t -> t -> expression
