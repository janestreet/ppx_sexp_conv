open Base

[@@@warning "-37"]

module One_type = struct
  type t = T of int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { name_kind = Capitalized
               ; clauses =
                   [ { name = "T"; args = Cons (int_sexp_grammar.untyped, Empty) } ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Two_types = struct
  type t =
    | T_int of int
    | T_u of u

  and u =
    | U_int of int
    | U_t of t
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  include struct
    open struct
      let (grammars__001_ :
             Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.defn Stdlib.List.t Stdlib.Lazy.t)
        =
        lazy
          (let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
             { untyped = Tycon ("t", []) }
           and (u_sexp_grammar : u Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
             { untyped = Tycon ("u", []) }
           in
           [ { tycon = "t"
             ; tyvars = []
             ; grammar =
                 Variant
                   { name_kind = Capitalized
                   ; clauses =
                       [ { name = "T_int"; args = Cons (int_sexp_grammar.untyped, Empty) }
                       ; { name = "T_u"; args = Cons (u_sexp_grammar.untyped, Empty) }
                       ]
                   }
             }
           ; { tycon = "u"
             ; tyvars = []
             ; grammar =
                 Variant
                   { name_kind = Capitalized
                   ; clauses =
                       [ { name = "U_int"; args = Cons (int_sexp_grammar.untyped, Empty) }
                       ; { name = "U_t"; args = Cons (t_sexp_grammar.untyped, Empty) }
                       ]
                   }
             }
           ])
      ;;

      let _ = grammars__001_
    end

    let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      { untyped =
          Lazy (lazy (Recursive (Tycon ("t", []), Stdlib.Lazy.force grammars__001_)))
      }

    and (u_sexp_grammar : u Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      { untyped =
          Lazy (lazy (Recursive (Tycon ("u", []), Stdlib.Lazy.force grammars__001_)))
      }
    ;;

    let _ = t_sexp_grammar
    and _ = u_sexp_grammar
  end

  [@@@end]
end
