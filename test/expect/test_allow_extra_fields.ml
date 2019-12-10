open! Base

module Allow_extra_fields = struct
  type t = { a : int } [@@sexp.allow_extra_fields] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\024\018\219\231,\176\148\206Y\195\132\0042H\\U"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = true
                ; fields = [ "a", { optional = false; args = [ One (Implicit_var 0) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) = Ref ("t", _the_group) in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Forbid_extra_fields = struct
  type t = { a : int } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\142\248\194uE\0077q\014\151\186\131R\n\213$"
      ; types         =
          [ ( "t"
            , Record
                { allow_extra_fields = false
                ; fields = [ "a", { optional = false; args = [ One (Implicit_var 0) ] } ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) = Ref ("t", _the_group) in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Variant_type = struct
  type t =
    | Allow_extra_fields  of { foo : int } [@sexp.allow_extra_fields]
    | Forbid_extra_fields of { bar : int }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "#\156P\178\244K\247\174D~#\241\000}u%"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = false
                ; alts                  =
                    [ ( "Allow_extra_fields"
                      , [ Fields
                            { allow_extra_fields = true
                            ; fields             =
                                [ ( "foo"
                                  , { optional = false; args = [ One (Implicit_var 0) ] }
                                  )
                                ]
                            }
                        ] )
                    ; ( "Forbid_extra_fields"
                      , [ Fields
                            { allow_extra_fields = false
                            ; fields             =
                                [ ( "bar"
                                  , { optional = false; args = [ One (Implicit_var 0) ] }
                                  )
                                ]
                            }
                        ] )
                    ]
                } )
          ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Grammar.group) =
      { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group  = _the_generic_group
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) = Ref ("t", _the_group) in
    t_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let _ = Allow_extra_fields  { foo = 1 }
  let _ = Forbid_extra_fields { bar = 1 }
end
