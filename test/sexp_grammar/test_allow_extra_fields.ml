open! Base

module Allow_extra_fields = struct
  type t = { a : int } [@@sexp.allow_extra_fields] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (List
               (Fields
                  { allow_extra_fields = true
                  ; fields =
                      [ { name = "a"
                        ; required = true
                        ; args = Cons (int_sexp_grammar.untyped, Empty)
                        }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Forbid_extra_fields = struct
  type t = { a : int } [@@deriving_inline sexp_grammar]

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
                        ; args = Cons (int_sexp_grammar.untyped, Empty)
                        }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Variant_type = struct
  type t =
    | Allow_extra_fields of { foo : int } [@sexp.allow_extra_fields]
    | Forbid_extra_fields of { bar : int }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { name_kind = Capitalized
               ; clauses =
                   [ { name = "Allow_extra_fields"
                     ; args =
                         Fields
                           { allow_extra_fields = true
                           ; fields =
                               [ { name = "foo"
                                 ; required = true
                                 ; args = Cons (int_sexp_grammar.untyped, Empty)
                                 }
                               ]
                           }
                     }
                   ; { name = "Forbid_extra_fields"
                     ; args =
                         Fields
                           { allow_extra_fields = false
                           ; fields =
                               [ { name = "bar"
                                 ; required = true
                                 ; args = Cons (int_sexp_grammar.untyped, Empty)
                                 }
                               ]
                           }
                     }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let _ = Allow_extra_fields { foo = 1 }
  let _ = Forbid_extra_fields { bar = 1 }
end
