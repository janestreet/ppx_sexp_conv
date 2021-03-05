open Base

[@@@warning "-37"]

module Nested_inside_variant = struct
  type t = A of [ `A of int ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { name_kind = Capitalized
               ; clauses =
                   [ { name = "A"
                     ; args =
                         Cons
                           ( Variant
                               { name_kind = Any_case
                               ; clauses =
                                   [ { name = "A"
                                     ; args = Cons (int_sexp_grammar.untyped, Empty)
                                     }
                                   ]
                               }
                           , Empty )
                     }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Nested_inside_record = struct
  type t = { a : [ `A of int ] } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (List
               (Fields
                  { allow_extra_fields = false
                  ; fields =
                      [ { name = "a"
                        ; required = true
                        ; args =
                            Cons
                              ( Variant
                                  { name_kind = Any_case
                                  ; clauses =
                                      [ { name = "A"
                                        ; args = Cons (int_sexp_grammar.untyped, Empty)
                                        }
                                      ]
                                  }
                              , Empty )
                        }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
