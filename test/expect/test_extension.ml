open! Base

(* Not sure how much people will want to use this, considering that the input is more
   complicated and specific than the output, but they have it. *)
module type S = sig
  val t_sexp_grammar : [%sexp_grammar: int Map.M(String).t]
end

module F (M : S) : sig
  val t_sexp_grammar : Sexplib0.Grammar.t [@@warning "-32"]
end =
  M

(* The grammar is illegible, so just make sure it builds. *)

let (_ : Sexp.Grammar.t) = [%sexp_grammar: int Map.M(String).t]

(* This used to give a compilation error. *)
let (_ : Sexp.Grammar.t) = [%sexp_grammar: _ list]
