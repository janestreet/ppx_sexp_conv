open Ppx_sexp_conv_lib.Conv

[@@@warning "-37"] (* allow unused constructors *)

type abstract_a [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : abstract_a) -> ()

let (abstract_a_sexp_grammar : abstract_a Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped = Any "Test_coverage_for_deriving.abstract_a" }
;;

let _ = abstract_a_sexp_grammar

[@@@end]

type abstract_b [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : abstract_b) -> ()

let (abstract_b_sexp_grammar : abstract_b Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped = Any "Test_coverage_for_deriving.abstract_b" }
;;

let _ = abstract_b_sexp_grammar

[@@@end]

type integer = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : integer) -> ()
let (integer_sexp_grammar : integer Ppx_sexp_conv_lib.Sexp_grammar.t) = int_sexp_grammar
let _ = integer_sexp_grammar

[@@@end]

type tuple = int * string [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : tuple) -> ()

let (tuple_sexp_grammar : tuple Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (List
             (Cons (int_sexp_grammar.untyped, Cons (string_sexp_grammar.untyped, Empty)))))
  }
;;

let _ = tuple_sexp_grammar

[@@@end]

type pos =
  { x : float
  ; y : float
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : pos) -> ()

let (pos_sexp_grammar : pos Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (List
             (Fields
                { allow_extra_fields = false
                ; fields =
                    [ { name = "x"
                      ; required = true
                      ; args = Cons (float_sexp_grammar.untyped, Empty)
                      }
                    ; { name = "y"
                      ; required = true
                      ; args = Cons (float_sexp_grammar.untyped, Empty)
                      }
                    ]
                })))
  }
;;

let _ = pos_sexp_grammar

[@@@end]

type 'a unary = 'a list [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : 'a unary) -> ()

let (unary_sexp_grammar :
       'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a unary Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar -> list_sexp_grammar _'a_sexp_grammar
;;

let _ = unary_sexp_grammar

[@@@end]

type enum =
  | One
  | Two
  | Three
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : enum) -> ()

let (enum_sexp_grammar : enum Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Variant
        { name_kind = Capitalized
        ; clauses =
            [ { name = "One"; clause_kind = Atom_clause }
            ; { name = "Two"; clause_kind = Atom_clause }
            ; { name = "Three"; clause_kind = Atom_clause }
            ]
        }
  }
;;

let _ = enum_sexp_grammar

[@@@end]

type ('a, 'b) which =
  | This of 'a
  | That of 'b
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : ('a, 'b) which) -> ()

let (which_sexp_grammar :
       'a Ppx_sexp_conv_lib.Sexp_grammar.t
     -> 'b Ppx_sexp_conv_lib.Sexp_grammar.t
     -> ('a, 'b) which Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar _'b_sexp_grammar ->
  { untyped =
      Variant
        { name_kind = Capitalized
        ; clauses =
            [ { name = "This"
              ; clause_kind =
                  List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
              }
            ; { name = "That"
              ; clause_kind =
                  List_clause { args = Cons (_'b_sexp_grammar.untyped, Empty) }
              }
            ]
        }
  }
;;

let _ = which_sexp_grammar

[@@@end]

type 'a optional =
  | No
  | Yes of 'a
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : 'a optional) -> ()

let (optional_sexp_grammar :
       'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a optional Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar ->
  { untyped =
      Variant
        { name_kind = Capitalized
        ; clauses =
            [ { name = "No"; clause_kind = Atom_clause }
            ; { name = "Yes"
              ; clause_kind =
                  List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
              }
            ]
        }
  }
;;

let _ = optional_sexp_grammar

[@@@end]

type empty = | [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : empty) -> ()
let (empty_sexp_grammar : empty Ppx_sexp_conv_lib.Sexp_grammar.t) = { untyped = Union [] }
let _ = empty_sexp_grammar

[@@@end]

type _ phantom = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : _ phantom) -> ()

let (phantom_sexp_grammar :
       'v_x__003_ Ppx_sexp_conv_lib.Sexp_grammar.t
     -> 'v_x__003_ phantom Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  fun _'v_x__003__sexp_grammar -> int_sexp_grammar
;;

let _ = phantom_sexp_grammar

[@@@end]

type color =
  [ `Red
  | `Blue
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : color) -> ()

let (color_sexp_grammar : color Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Variant
        { name_kind = Any_case
        ; clauses =
            [ { name = "Red"; clause_kind = Atom_clause }
            ; { name = "Blue"; clause_kind = Atom_clause }
            ]
        }
  }
;;

let _ = color_sexp_grammar

[@@@end]

type adjective =
  [ color
  | `Fast
  | `Slow
  | `Count of int
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : adjective) -> ()

let (adjective_sexp_grammar : adjective Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (Union
             [ color_sexp_grammar.untyped
             ; Variant
                 { name_kind = Any_case
                 ; clauses =
                     [ { name = "Fast"; clause_kind = Atom_clause }
                     ; { name = "Slow"; clause_kind = Atom_clause }
                     ; { name = "Count"
                       ; clause_kind =
                           List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                       }
                     ]
                 }
             ]))
  }
;;

let _ = adjective_sexp_grammar

[@@@end]

type 'a tree =
  { data : 'a
  ; children : 'a tree list
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : 'a tree) -> ()

include struct
  open struct
    let (grammars__004_ : Ppx_sexp_conv_lib.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t)
      =
      lazy
        (let (tree_sexp_grammar
              : 'a Ppx_sexp_conv_lib.Sexp_grammar.t
              -> 'a tree Ppx_sexp_conv_lib.Sexp_grammar.t)
          =
          fun _'a_sexp_grammar ->
            { untyped = Tycon ("tree", [ _'a_sexp_grammar.untyped ]) }
         in
         [ { tycon = "tree"
           ; tyvars = [ "a" ]
           ; grammar =
               List
                 (Fields
                    { allow_extra_fields = false
                    ; fields =
                        [ { name = "data"
                          ; required = true
                          ; args = Cons (Tyvar "a", Empty)
                          }
                        ; { name = "children"
                          ; required = true
                          ; args =
                              Cons
                                ( (list_sexp_grammar
                                     (tree_sexp_grammar { untyped = Tyvar "a" }))
                                  .untyped
                                , Empty )
                          }
                        ]
                    })
           }
         ])
    ;;

    let _ = grammars__004_
  end

  let (tree_sexp_grammar :
         'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a tree Ppx_sexp_conv_lib.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar ->
    { untyped =
        Recursive
          (Tycon ("tree", [ _'a_sexp_grammar.untyped ]), Stdlib.Lazy.force grammars__004_)
    }
  ;;

  let _ = tree_sexp_grammar
end

[@@@end]

type alpha = int

and beta =
  { alpha : alpha
  ; betas : beta list
  }

and gamma = beta list [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : alpha) -> ()
let _ = fun (_ : beta) -> ()
let _ = fun (_ : gamma) -> ()

include struct
  open struct
    let (grammars__005_ : Ppx_sexp_conv_lib.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t)
      =
      lazy
        (let (alpha_sexp_grammar : alpha Ppx_sexp_conv_lib.Sexp_grammar.t) =
           { untyped = Tycon ("alpha", []) }
         and (beta_sexp_grammar : beta Ppx_sexp_conv_lib.Sexp_grammar.t) =
           { untyped = Tycon ("beta", []) }
         in
         [ { tycon = "alpha"; tyvars = []; grammar = int_sexp_grammar.untyped }
         ; { tycon = "beta"
           ; tyvars = []
           ; grammar =
               List
                 (Fields
                    { allow_extra_fields = false
                    ; fields =
                        [ { name = "alpha"
                          ; required = true
                          ; args = Cons (alpha_sexp_grammar.untyped, Empty)
                          }
                        ; { name = "betas"
                          ; required = true
                          ; args =
                              Cons ((list_sexp_grammar beta_sexp_grammar).untyped, Empty)
                          }
                        ]
                    })
           }
         ])
    ;;

    let _ = grammars__005_
  end

  let (alpha_sexp_grammar : alpha Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy (lazy (Recursive (Tycon ("alpha", []), Stdlib.Lazy.force grammars__005_)))
    }

  and (beta_sexp_grammar : beta Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy (lazy (Recursive (Tycon ("beta", []), Stdlib.Lazy.force grammars__005_)))
    }
  ;;

  let _ = alpha_sexp_grammar
  and _ = beta_sexp_grammar
end

let (gamma_sexp_grammar : gamma Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped = Lazy (lazy (list_sexp_grammar beta_sexp_grammar).untyped) }
;;

let _ = gamma_sexp_grammar

[@@@end]

type record_attributes =
  { a : int [@default 0]
  ; b : bool [@sexp.bool]
  ; c : float option [@sexp.option]
  ; d : string list [@sexp.list]
  ; e : bytes array [@sexp.array]
  ; f : Ppx_sexp_conv_lib.Sexp.t [@sexp.omit_nil]
  }
[@@sexp.allow_extra_fields] [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : record_attributes) -> ()

let (record_attributes_sexp_grammar : record_attributes Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (List
             (Fields
                { allow_extra_fields = true
                ; fields =
                    [ { name = "a"
                      ; required = false
                      ; args = Cons (int_sexp_grammar.untyped, Empty)
                      }
                    ; { name = "b"; required = false; args = Empty }
                    ; { name = "c"
                      ; required = false
                      ; args = Cons (float_sexp_grammar.untyped, Empty)
                      }
                    ; { name = "d"
                      ; required = false
                      ; args = Cons (List (Many string_sexp_grammar.untyped), Empty)
                      }
                    ; { name = "e"
                      ; required = false
                      ; args = Cons (List (Many bytes_sexp_grammar.untyped), Empty)
                      }
                    ; { name = "f"
                      ; required = false
                      ; args = Cons (Ppx_sexp_conv_lib.Sexp.t_sexp_grammar.untyped, Empty)
                      }
                    ]
                })))
  }
;;

let _ = record_attributes_sexp_grammar

[@@@end]

type variant_attributes =
  | A
  | B of int list [@sexp.list]
  | C of
      { a : int [@default 0]
      ; b : bool [@sexp.bool]
      ; c : float option [@sexp.option]
      ; d : string list [@sexp.list]
      ; e : bytes array [@sexp.array]
      ; f : Ppx_sexp_conv_lib.Sexp.t [@sexp.omit_nil]
      } [@sexp.allow_extra_fields]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : variant_attributes) -> ()

let (variant_attributes_sexp_grammar :
       variant_attributes Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  { untyped =
      Lazy
        (lazy
          (Variant
             { name_kind = Capitalized
             ; clauses =
                 [ { name = "A"; clause_kind = Atom_clause }
                 ; { name = "B"
                   ; clause_kind = List_clause { args = Many int_sexp_grammar.untyped }
                   }
                 ; { name = "C"
                   ; clause_kind =
                       List_clause
                         { args =
                             Fields
                               { allow_extra_fields = true
                               ; fields =
                                   [ { name = "a"
                                     ; required = false
                                     ; args = Cons (int_sexp_grammar.untyped, Empty)
                                     }
                                   ; { name = "b"; required = false; args = Empty }
                                   ; { name = "c"
                                     ; required = false
                                     ; args = Cons (float_sexp_grammar.untyped, Empty)
                                     }
                                   ; { name = "d"
                                     ; required = false
                                     ; args =
                                         Cons
                                           (List (Many string_sexp_grammar.untyped), Empty)
                                     }
                                   ; { name = "e"
                                     ; required = false
                                     ; args =
                                         Cons
                                           (List (Many bytes_sexp_grammar.untyped), Empty)
                                     }
                                   ; { name = "f"
                                     ; required = false
                                     ; args =
                                         Cons
                                           ( Ppx_sexp_conv_lib.Sexp.t_sexp_grammar.untyped
                                           , Empty )
                                     }
                                   ]
                               }
                         }
                   }
                 ]
             }))
  }
;;

let _ = variant_attributes_sexp_grammar

[@@@end]

type polymorphic_variant_attributes =
  [ `A
  | `B of int list [@sexp.list]
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : polymorphic_variant_attributes) -> ()

let (polymorphic_variant_attributes_sexp_grammar :
       polymorphic_variant_attributes Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  { untyped =
      Lazy
        (lazy
          (Variant
             { name_kind = Any_case
             ; clauses =
                 [ { name = "A"; clause_kind = Atom_clause }
                 ; { name = "B"
                   ; clause_kind = List_clause { args = Many int_sexp_grammar.untyped }
                   }
                 ]
             }))
  }
;;

let _ = polymorphic_variant_attributes_sexp_grammar

[@@@end]

type opaque =
  { x : (string[@sexp.opaque])
  ; y : int -> int
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : opaque) -> ()

let (opaque_sexp_grammar : opaque Ppx_sexp_conv_lib.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (List
             (Fields
                { allow_extra_fields = false
                ; fields =
                    [ { name = "x"
                      ; required = true
                      ; args =
                          Cons (Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar.untyped, Empty)
                      }
                    ; { name = "y"
                      ; required = true
                      ; args =
                          Cons (Ppx_sexp_conv_lib.Conv.fun_sexp_grammar.untyped, Empty)
                      }
                    ]
                })))
  }
;;

let _ = opaque_sexp_grammar

[@@@end]
