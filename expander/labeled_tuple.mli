(* Support for labeled tuples, a language feature currently only implemented in Jane
   Street's experimental branch of the compiler
   (https://github.com/ocaml-flambda/flambda-backend/). *)

open! Base
open Ppxlib_jane

val is_valid : Jane_syntax.Labeled_tuples.core_type -> bool
val atom_of_label : string option -> string
