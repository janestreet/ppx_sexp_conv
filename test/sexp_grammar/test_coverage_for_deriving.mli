(** This file covers a lot of cases for [@@deriving], for both interface and
    implementation. They are also exported for validation. *)

type abstract_a [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val abstract_a_sexp_grammar : abstract_a Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type abstract_b [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val abstract_b_sexp_grammar : abstract_b Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type integer = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val integer_sexp_grammar : integer Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type tuple = int * string [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val tuple_sexp_grammar : tuple Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type pos =
  { x : float
  ; y : float
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val pos_sexp_grammar : pos Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a unary = 'a list [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val unary_sexp_grammar
    :  'a Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> 'a unary Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type enum =
  | One
  | Two
  | Three
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val enum_sexp_grammar : enum Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type ('a, 'b) which =
  | This of 'a
  | That of 'b
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val which_sexp_grammar
    :  'a Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> 'b Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> ('a, 'b) which Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a optional =
  | No
  | Yes of 'a
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val optional_sexp_grammar
    :  'a Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> 'a optional Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type empty = | [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val empty_sexp_grammar : empty Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type _ phantom = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val phantom_sexp_grammar
    :  'v_x__003_ Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> 'v_x__003_ phantom Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type color =
  [ `Red
  | `Blue
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val color_sexp_grammar : color Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type adjective =
  [ color
  | `Fast
  | `Slow
  | `Count of int
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val adjective_sexp_grammar : adjective Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a tree =
  { data : 'a
  ; children : 'a tree list
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val tree_sexp_grammar
    :  'a Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
    -> 'a tree Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type alpha = int

and beta =
  { alpha : alpha
  ; betas : beta list
  }

and gamma = beta list [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val alpha_sexp_grammar : alpha Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
  val beta_sexp_grammar : beta Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
  val gamma_sexp_grammar : gamma Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type record_attributes =
  { a : int
  ; b : bool
  ; c : float option
  ; d : string list
  ; e : bytes array
  ; f : Ppx_sexp_conv_lib.Sexp.t
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val record_attributes_sexp_grammar
    : record_attributes Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type variant_attributes =
  | A
  | B of int list
  | C of
      { a : int
      ; b : bool
      ; c : float option
      ; d : string list
      ; e : bytes array
      ; f : Ppx_sexp_conv_lib.Sexp.t
      }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val variant_attributes_sexp_grammar
    : variant_attributes Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type polymorphic_variant_attributes =
  [ `A
  | `B of int list
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val polymorphic_variant_attributes_sexp_grammar
    : polymorphic_variant_attributes Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type opaque =
  { x : string
  ; y : int -> int
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val opaque_sexp_grammar : opaque Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]
