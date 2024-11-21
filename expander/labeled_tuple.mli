(* Support for labeled tuples, a language feature currently only implemented in Jane
   Street's experimental branch of the compiler
   (https://github.com/ocaml-flambda/flambda-backend/). *)

open! Base

val has_any_label : (string option * _) list -> bool
val atom_of_label : string option -> string
