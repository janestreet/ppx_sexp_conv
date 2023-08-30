open! Base

module _ = struct
  (* Nonrecursive constant *)
  type t = [ `T of int ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { case_sensitivity = Case_sensitive
               ; clauses =
                   [ No_tag
                       { name = "T"
                       ; clause_kind =
                           List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                       }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  (* Recursive constant *)
  type t = [ `T of t ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  include struct
    open struct
      let (grammars__001_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t) =
        lazy
          (let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
             { untyped = Recursive ("t", []) }
           in
           [ { tycon = "t"
             ; tyvars = []
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive
                   ; clauses =
                       [ No_tag
                           { name = "T"
                           ; clause_kind =
                               List_clause { args = Cons (t_sexp_grammar.untyped, Empty) }
                           }
                       ]
                   }
             }
           ])
      ;;

      let _ = grammars__001_
    end

    let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
      { untyped = Lazy (lazy (Tycon ("t", [], Stdlib.Lazy.force grammars__001_))) }
    ;;

    let _ = t_sexp_grammar
  end

  [@@@end]
end

module _ = struct
  (* Nonrecursive parameterized *)
  type 'a t = [ `T of 'a ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { case_sensitivity = Case_sensitive
          ; clauses =
              [ No_tag
                  { name = "T"
                  ; clause_kind =
                      List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                  }
              ]
          }
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  (* Recursive parameterized *)
  type 'a t = [ `T of 'a t ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  include struct
    open struct
      let (grammars__002_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t) =
        lazy
          (let t_sexp_grammar
                 : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t
             =
             fun _'a_sexp_grammar ->
             { untyped = Recursive ("t", [ _'a_sexp_grammar.untyped ]) }
           in
           [ { tycon = "t"
             ; tyvars = [ "a" ]
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive
                   ; clauses =
                       [ No_tag
                           { name = "T"
                           ; clause_kind =
                               List_clause
                                 { args =
                                     Cons
                                       ( (t_sexp_grammar { untyped = Tyvar "a" }).untyped
                                       , Empty )
                                 }
                           }
                       ]
                   }
             }
           ])
      ;;

      let _ = grammars__002_
    end

    let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
      fun _'a_sexp_grammar ->
      { untyped =
          Tycon ("t", [ _'a_sexp_grammar.untyped ], Stdlib.Lazy.force grammars__002_)
      }
    ;;

    let _ = t_sexp_grammar
  end

  [@@@end]
end

module _ = struct
  (* Aliasing of non-parameterized type *)
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = int_sexp_grammar
  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  (* Aliasing of parameterized type *)
  type 'a t = 'a list [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    fun _'a_sexp_grammar -> list_sexp_grammar _'a_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
