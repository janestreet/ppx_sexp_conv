open! Base

type t = T : ('a[@sexp.opaque]) -> t [@@deriving_inline sexp, sexp_grammar]

let _ = fun (_ : t) -> ()

let t_of_sexp =
  (let _tp_loc = "test_gadt.ml.t" in
   function
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom (("t" | "T") as _tag) :: sexp_args) as _sexp ->
     (match sexp_args with
      | [ v0 ] ->
        let v0 = Ppx_sexp_conv_lib.Conv.opaque_of_sexp v0 in
        T v0
      | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
   | Ppx_sexp_conv_lib.Sexp.Atom ("t" | "T") as sexp ->
     Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
     Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
   | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
             : Ppx_sexp_conv_lib.Sexp.t -> t)
;;

let _ = t_of_sexp

let sexp_of_t =
  (function
    | T v0 ->
      let v0 = Ppx_sexp_conv_lib.Conv.sexp_of_opaque v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "T"; v0 ]
      : t -> Ppx_sexp_conv_lib.Sexp.t)
;;

let _ = sexp_of_t

let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (Variant
             { name_kind = Capitalized
             ; clauses =
                 [ { name = "T"
                   ; args =
                       Cons (Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar.untyped, Empty)
                   }
                 ]
             }))
  }
;;

let _ = t_sexp_grammar

[@@@end]

type nullary = Nullary : nullary [@@deriving_inline sexp, sexp_grammar]

let _ = fun (_ : nullary) -> ()

let nullary_of_sexp =
  (let _tp_loc = "test_gadt.ml.nullary" in
   function
   | Ppx_sexp_conv_lib.Sexp.Atom ("nullary" | "Nullary") -> Nullary
   | Ppx_sexp_conv_lib.Sexp.List
       (Ppx_sexp_conv_lib.Sexp.Atom ("nullary" | "Nullary") :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
     Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
   | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
     Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
   | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
             : Ppx_sexp_conv_lib.Sexp.t -> nullary)
;;

let _ = nullary_of_sexp

let sexp_of_nullary =
  (function
    | Nullary -> Ppx_sexp_conv_lib.Sexp.Atom "Nullary"
                 : nullary -> Ppx_sexp_conv_lib.Sexp.t)
;;

let _ = sexp_of_nullary

let (nullary_sexp_grammar : nullary Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  { untyped = Enum { name_kind = Capitalized; names = [ "Nullary" ] } }
;;

let _ = nullary_sexp_grammar

[@@@end]

(* We can't derive [of_sexp], but we can derive a sensible grammar for this type. *)
type _ grammar_only = Grammar_only : int -> string grammar_only
[@@warning "-37"] [@@deriving_inline sexp_grammar]

let _ = fun (_ : _ grammar_only) -> ()

let (grammar_only_sexp_grammar :
       'v_x__001_ Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t
     -> 'v_x__001_ grammar_only Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)
  =
  fun _'v_x__001__sexp_grammar ->
  { untyped =
      Variant
        { name_kind = Capitalized
        ; clauses =
            [ { name = "Grammar_only"; args = Cons (int_sexp_grammar.untyped, Empty) } ]
        }
  }
;;

let _ = grammar_only_sexp_grammar

[@@@end]
