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

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = []
    ; ggid = "hI\171z\157>\191\157\183E\134\225\246>)\251"
    ; types =
        [ ( "t"
          , Variant
              { ignore_capitalization = true
              ; alts =
                  [ "T", [ One (Grammar Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar) ] ]
              } )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = []
    ; generic_group = _the_generic_group
    ; origin = "test_gadt.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
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

let (nullary_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = []
    ; ggid = "\127z'\177\250\197 V\163t\221416\000)"
    ; types =
        [ "nullary", Variant { ignore_capitalization = true; alts = [ "Nullary", [] ] } ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = []
    ; generic_group = _the_generic_group
    ; origin = "test_gadt.ml"
    }
  in
  let (nullary_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("nullary", _the_group)
  in
  nullary_sexp_grammar
;;

let _ = nullary_sexp_grammar

[@@@end]

(* We can't derive [of_sexp], but we can derive a sensible grammar for this type. *)
type _ grammar_only = Grammar_only : int -> string grammar_only
[@@warning "-37"] [@@deriving_inline sexp_grammar]

let _ = fun (_ : _ grammar_only) -> ()

let (grammar_only_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = [ "int" ]
    ; ggid = "_Z\019'\145\145)\144\155)\026\004\024\243z4"
    ; types =
        [ ( "grammar_only"
          , Tyvar_parameterize
              ( [ "_" ]
              , Variant
                  { ignore_capitalization = true
                  ; alts = [ "Grammar_only", [ One (Tycon_index 0) ] ]
                  } ) )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = [ int_sexp_grammar ]
    ; generic_group = _the_generic_group
    ; origin = "test_gadt.ml"
    }
  in
  let (grammar_only_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("grammar_only", _the_group)
  in
  grammar_only_sexp_grammar
;;

let _ = grammar_only_sexp_grammar

[@@@end]
