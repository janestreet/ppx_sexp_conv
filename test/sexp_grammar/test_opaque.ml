open! Base

type t = (int[@sexp.opaque]) list [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  { untyped =
      Lazy (lazy (list_sexp_grammar Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar).untyped)
  }
;;

let _ = t_sexp_grammar

[@@@end]
