open! Base

module Simple_grammar = struct
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { tycon_names = [ "int" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Tycon_index 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; instantiate_tycons = [ int_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "test_test.ml.Simple_grammar"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@deriving.end]
end

module Recursive_group = struct
  type 'a t = T of 'a

  and 'a u = U of 'a t option [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)
      , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) )
    =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { tycon_names = [ "option" ]
      ; ggid = "3\188_G\181s\242\209x\249#\138\249\222\158}"
      ; types =
          [ ( "t"
            , Tyvar_parameterize
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts = [ "T", [ One (Tyvar_index 0) ] ]
                    } ) )
          ; ( "u"
            , Tyvar_parameterize
                ( [ "a" ]
                , Variant
                    { ignore_capitalization = true
                    ; alts =
                        [ ( "U"
                          , [ One
                                (Tyvar_instantiate
                                   ( Tycon_index 0
                                   , [ Tyvar_instantiate (Recursive "t", [ Tyvar_index 0 ])
                                     ] ))
                            ] )
                        ]
                    } ) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; instantiate_tycons = [ option_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "test_test.ml.Recursive_group"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    and (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("u", _the_group)
    in
    t_sexp_grammar, u_sexp_grammar
  ;;

  let _ = t_sexp_grammar
  and _ = u_sexp_grammar

  [@@@deriving.end]

  (* Avoid unused constructor warnings. *)
  let _ = T ()
  let _ = U None
end

module Functions = struct
  type ('a, 'b) t = 'a -> 'b [@@deriving_inline sexp_grammar]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { tycon_names = []
      ; ggid = "\212M\132\163`\216\129\163\189\156\2409m\207A>"
      ; types =
          [ ( "t"
            , Tyvar_parameterize
                ([ "a"; "b" ], Grammar Ppx_sexp_conv_lib.Conv.fun_sexp_grammar) )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; instantiate_tycons = []
      ; generic_group = _the_generic_group
      ; origin = "test_test.ml.Functions"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
