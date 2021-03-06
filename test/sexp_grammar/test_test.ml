open! Base

module Simple_grammar = struct
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) = int_sexp_grammar
  let _ = t_sexp_grammar

  [@@@deriving.end]
end

module Recursive_group = struct
  type 'a t = T of 'a

  and 'a u = U of 'a t option [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  let (t_sexp_grammar :
         'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a t Ppx_sexp_conv_lib.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { name_kind = Capitalized
          ; clauses =
              [ { name = "T"
                ; clause_kind =
                    List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                }
              ]
          }
    }
  ;;

  let _ = t_sexp_grammar

  let (u_sexp_grammar :
         'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a u Ppx_sexp_conv_lib.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { name_kind = Capitalized
          ; clauses =
              [ { name = "U"
                ; clause_kind =
                    List_clause
                      { args =
                          Cons
                            ( (option_sexp_grammar (t_sexp_grammar _'a_sexp_grammar))
                              .untyped
                            , Empty )
                      }
                }
              ]
          }
    }
  ;;

  let _ = u_sexp_grammar

  [@@@deriving.end]

  (* Avoid unused constructor warnings. *)
  let _ = T ()
  let _ = U None
end

module Functions = struct
  type ('a, 'b) t = 'a -> 'b [@@deriving_inline sexp_grammar]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let (t_sexp_grammar :
         'a Ppx_sexp_conv_lib.Sexp_grammar.t
       -> 'b Ppx_sexp_conv_lib.Sexp_grammar.t
       -> ('a, 'b) t Ppx_sexp_conv_lib.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar _'b_sexp_grammar -> Ppx_sexp_conv_lib.Conv.fun_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
