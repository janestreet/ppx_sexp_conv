open Ppx_sexp_conv_lib.Conv

type 'a or_null = 'a Ppx_sexp_conv_lib.Or_null.t

[@@@warning "-37"] (* allow unused constructors *)

type abstract_a [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : abstract_a) -> ()

let (abstract_a_sexp_grammar : abstract_a Sexplib0.Sexp_grammar.t) =
  { untyped = Any "Test_coverage_for_deriving.abstract_a" }
;;

let _ = abstract_a_sexp_grammar

[@@@end]

type abstract_b [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : abstract_b) -> ()

let (abstract_b_sexp_grammar : abstract_b Sexplib0.Sexp_grammar.t) =
  { untyped = Any "Test_coverage_for_deriving.abstract_b" }
;;

let _ = abstract_b_sexp_grammar

[@@@end]

type integer = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : integer) -> ()
let (integer_sexp_grammar : integer Sexplib0.Sexp_grammar.t) = int_sexp_grammar
let _ = integer_sexp_grammar

[@@@end]

type tuple = int * string [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : tuple) -> ()

let tuple_sexp_grammar : tuple Sexplib0.Sexp_grammar.t =
  { untyped =
      List (Cons (int_sexp_grammar.untyped, Cons (string_sexp_grammar.untyped, Empty)))
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

let pos_sexp_grammar : pos Sexplib0.Sexp_grammar.t =
  { untyped =
      List
        (Fields
           { allow_extra_fields = false
           ; fields =
               [ No_tag
                   { name = "x"
                   ; required = true
                   ; args = Cons (float_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "y"
                   ; required = true
                   ; args = Cons (float_sexp_grammar.untyped, Empty)
                   }
               ]
           })
  }
;;

let _ = pos_sexp_grammar

[@@@end]

type 'a unary = 'a list [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : 'a unary) -> ()

let unary_sexp_grammar
  : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a unary Sexplib0.Sexp_grammar.t
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

let (enum_sexp_grammar : enum Sexplib0.Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag { name = "One"; clause_kind = Atom_clause }
            ; No_tag { name = "Two"; clause_kind = Atom_clause }
            ; No_tag { name = "Three"; clause_kind = Atom_clause }
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

let which_sexp_grammar
  : 'a 'b.
  'a Sexplib0.Sexp_grammar.t
  -> 'b Sexplib0.Sexp_grammar.t
  -> ('a, 'b) which Sexplib0.Sexp_grammar.t
  =
  fun _'a_sexp_grammar _'b_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "This"
                ; clause_kind =
                    List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                }
            ; No_tag
                { name = "That"
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

let optional_sexp_grammar
  : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a optional Sexplib0.Sexp_grammar.t
  =
  fun _'a_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag { name = "No"; clause_kind = Atom_clause }
            ; No_tag
                { name = "Yes"
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
let (empty_sexp_grammar : empty Sexplib0.Sexp_grammar.t) = { untyped = Union [] }
let _ = empty_sexp_grammar

[@@@end]

type _ phantom = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : _ phantom) -> ()

let phantom_sexp_grammar
  : 'a__090_. 'a__090_ Sexplib0.Sexp_grammar.t -> 'a__090_ phantom Sexplib0.Sexp_grammar.t
  =
  fun _'a__090__sexp_grammar -> int_sexp_grammar
;;

let _ = phantom_sexp_grammar

[@@@end]

type color =
  [ `Red
  | `Blue
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : color) -> ()

let (color_sexp_grammar : color Sexplib0.Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive
        ; clauses =
            [ No_tag { name = "Red"; clause_kind = Atom_clause }
            ; No_tag { name = "Blue"; clause_kind = Atom_clause }
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

let adjective_sexp_grammar : adjective Sexplib0.Sexp_grammar.t =
  { untyped =
      Union
        [ color_sexp_grammar.untyped
        ; Variant
            { case_sensitivity = Case_sensitive
            ; clauses =
                [ No_tag { name = "Fast"; clause_kind = Atom_clause }
                ; No_tag { name = "Slow"; clause_kind = Atom_clause }
                ; No_tag
                    { name = "Count"
                    ; clause_kind =
                        List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                    }
                ]
            }
        ]
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
    let grammars__126_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Basement.Portable_lazy.t
      =
      Basement.Portable_lazy.from_fun
        (Basement.Portability_hacks.magic_portable__needs_base_and_core
           (fun () : Sexplib0.Sexp_grammar.defn list ->
              let tree_sexp_grammar
                : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a tree Sexplib0.Sexp_grammar.t
                =
                fun _'a_sexp_grammar ->
                { untyped = Recursive ("tree", [ _'a_sexp_grammar.untyped ]) }
              in
              [ { tycon = "tree"
                ; tyvars = [ "a" ]
                ; grammar =
                    List
                      (Fields
                         { allow_extra_fields = false
                         ; fields =
                             [ No_tag
                                 { name = "data"
                                 ; required = true
                                 ; args = Cons (Tyvar "a", Empty)
                                 }
                             ; No_tag
                                 { name = "children"
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
              ]))
    ;;

    let _ = grammars__126_
  end

  let tree_sexp_grammar
    : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a tree Sexplib0.Sexp_grammar.t
    =
    fun _'a_sexp_grammar ->
    { untyped =
        Tycon
          ( "tree"
          , [ _'a_sexp_grammar.untyped ]
          , Basement.Portable_lazy.force grammars__126_ )
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
    let grammars__143_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Basement.Portable_lazy.t
      =
      Basement.Portable_lazy.from_fun
        (Basement.Portability_hacks.magic_portable__needs_base_and_core
           (fun () : Sexplib0.Sexp_grammar.defn list ->
              let alpha_sexp_grammar : alpha Sexplib0.Sexp_grammar.t =
                { untyped = Recursive ("alpha", []) }
              and beta_sexp_grammar : beta Sexplib0.Sexp_grammar.t =
                { untyped = Recursive ("beta", []) }
              in
              [ { tycon = "alpha"; tyvars = []; grammar = int_sexp_grammar.untyped }
              ; { tycon = "beta"
                ; tyvars = []
                ; grammar =
                    List
                      (Fields
                         { allow_extra_fields = false
                         ; fields =
                             [ No_tag
                                 { name = "alpha"
                                 ; required = true
                                 ; args = Cons (alpha_sexp_grammar.untyped, Empty)
                                 }
                             ; No_tag
                                 { name = "betas"
                                 ; required = true
                                 ; args =
                                     Cons
                                       ( (list_sexp_grammar beta_sexp_grammar).untyped
                                       , Empty )
                                 }
                             ]
                         })
                }
              ]))
    ;;

    let _ = grammars__143_
  end

  let alpha_sexp_grammar : alpha Sexplib0.Sexp_grammar.t =
    { untyped =
        Lazy
          (Basement.Portable_lazy.from_fun (fun () : Sexplib0.Sexp_grammar.grammar ->
             Tycon ("alpha", [], Basement.Portable_lazy.force grammars__143_)))
    }

  and beta_sexp_grammar : beta Sexplib0.Sexp_grammar.t =
    { untyped =
        Lazy
          (Basement.Portable_lazy.from_fun (fun () : Sexplib0.Sexp_grammar.grammar ->
             Tycon ("beta", [], Basement.Portable_lazy.force grammars__143_)))
    }
  ;;

  let _ = alpha_sexp_grammar
  and _ = beta_sexp_grammar
end

let gamma_sexp_grammar : gamma Sexplib0.Sexp_grammar.t =
  { untyped =
      Lazy
        (Basement.Portable_lazy.from_fun (fun () : Sexplib0.Sexp_grammar.grammar ->
           (list_sexp_grammar beta_sexp_grammar).untyped))
  }
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
  ; g : char or_null [@sexp.or_null]
  }
[@@sexp.allow_extra_fields] [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : record_attributes) -> ()

let record_attributes_sexp_grammar : record_attributes Sexplib0.Sexp_grammar.t =
  { untyped =
      List
        (Fields
           { allow_extra_fields = true
           ; fields =
               [ No_tag
                   { name = "a"
                   ; required = false
                   ; args = Cons (int_sexp_grammar.untyped, Empty)
                   }
               ; No_tag { name = "b"; required = false; args = Empty }
               ; No_tag
                   { name = "c"
                   ; required = false
                   ; args = Cons (float_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "d"
                   ; required = false
                   ; args = Cons (List (Many string_sexp_grammar.untyped), Empty)
                   }
               ; No_tag
                   { name = "e"
                   ; required = false
                   ; args = Cons (List (Many bytes_sexp_grammar.untyped), Empty)
                   }
               ; No_tag
                   { name = "f"
                   ; required = false
                   ; args = Cons (Ppx_sexp_conv_lib.Sexp.t_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "g"
                   ; required = false
                   ; args = Cons (char_sexp_grammar.untyped, Empty)
                   }
               ]
           })
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
      ; g : char or_null [@sexp.or_null]
      } [@sexp.allow_extra_fields]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : variant_attributes) -> ()

let variant_attributes_sexp_grammar : variant_attributes Sexplib0.Sexp_grammar.t =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag { name = "A"; clause_kind = Atom_clause }
            ; No_tag
                { name = "B"
                ; clause_kind = List_clause { args = Many int_sexp_grammar.untyped }
                }
            ; No_tag
                { name = "C"
                ; clause_kind =
                    List_clause
                      { args =
                          Fields
                            { allow_extra_fields = true
                            ; fields =
                                [ No_tag
                                    { name = "a"
                                    ; required = false
                                    ; args = Cons (int_sexp_grammar.untyped, Empty)
                                    }
                                ; No_tag { name = "b"; required = false; args = Empty }
                                ; No_tag
                                    { name = "c"
                                    ; required = false
                                    ; args = Cons (float_sexp_grammar.untyped, Empty)
                                    }
                                ; No_tag
                                    { name = "d"
                                    ; required = false
                                    ; args =
                                        Cons
                                          (List (Many string_sexp_grammar.untyped), Empty)
                                    }
                                ; No_tag
                                    { name = "e"
                                    ; required = false
                                    ; args =
                                        Cons
                                          (List (Many bytes_sexp_grammar.untyped), Empty)
                                    }
                                ; No_tag
                                    { name = "f"
                                    ; required = false
                                    ; args =
                                        Cons
                                          ( Ppx_sexp_conv_lib.Sexp.t_sexp_grammar.untyped
                                          , Empty )
                                    }
                                ; No_tag
                                    { name = "g"
                                    ; required = false
                                    ; args = Cons (char_sexp_grammar.untyped, Empty)
                                    }
                                ]
                            }
                      }
                }
            ]
        }
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

let polymorphic_variant_attributes_sexp_grammar
  : polymorphic_variant_attributes Sexplib0.Sexp_grammar.t
  =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive
        ; clauses =
            [ No_tag { name = "A"; clause_kind = Atom_clause }
            ; No_tag
                { name = "B"
                ; clause_kind = List_clause { args = Many int_sexp_grammar.untyped }
                }
            ]
        }
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

let opaque_sexp_grammar : opaque Sexplib0.Sexp_grammar.t =
  { untyped =
      List
        (Fields
           { allow_extra_fields = false
           ; fields =
               [ No_tag
                   { name = "x"
                   ; required = true
                   ; args = Cons (Sexplib0.Sexp_conv.opaque_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "y"
                   ; required = true
                   ; args = Cons (Sexplib0.Sexp_conv.fun_sexp_grammar.untyped, Empty)
                   }
               ]
           })
  }
;;

let _ = opaque_sexp_grammar

[@@@end]

type nonportable =
  { x : string
  ; y : int -> int
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : nonportable) -> ()

let nonportable_sexp_grammar : nonportable Sexplib0.Sexp_grammar.t =
  { untyped =
      List
        (Fields
           { allow_extra_fields = false
           ; fields =
               [ No_tag
                   { name = "x"
                   ; required = true
                   ; args = Cons (string_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "y"
                   ; required = true
                   ; args = Cons (Sexplib0.Sexp_conv.fun_sexp_grammar.untyped, Empty)
                   }
               ]
           })
  }
;;

let _ = nonportable_sexp_grammar

[@@@end]

type 'a nonportable1 =
  { x : string
  ; y : 'a -> int
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : 'a nonportable1) -> ()

let nonportable1_sexp_grammar
  : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a nonportable1 Sexplib0.Sexp_grammar.t
  =
  fun _'a_sexp_grammar ->
  { untyped =
      List
        (Fields
           { allow_extra_fields = false
           ; fields =
               [ No_tag
                   { name = "x"
                   ; required = true
                   ; args = Cons (string_sexp_grammar.untyped, Empty)
                   }
               ; No_tag
                   { name = "y"
                   ; required = true
                   ; args = Cons (Sexplib0.Sexp_conv.fun_sexp_grammar.untyped, Empty)
                   }
               ]
           })
  }
;;

let _ = nonportable1_sexp_grammar

[@@@end]

let nonportable1_sexp_grammar = nonportable1_sexp_grammar
