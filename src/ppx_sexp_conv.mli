open Ppxlib

val of_sexp : Deriving.t
val sexp_of : Deriving.t
val sexp_of__stack : Deriving.t
val sexp : Deriving.t
val sexp__stack : Deriving.t
val of_sexp_poly : Deriving.t
val sexp_poly : Deriving.t
val sexp_grammar : Deriving.t

(* Useful for other ppxes to link against if expanding into templated code. *)
val registered : unit
