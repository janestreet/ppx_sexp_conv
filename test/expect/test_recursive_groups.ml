open Base

[@@@warning "-37"]

module One_type = struct
  type t = T of int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "n\221J\012\211t\018|\161\187\170\127~\186wg"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = false
                ; alts                  = [ "T", [ One (Implicit_var 0) ] ]
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

module Two_types = struct
  type t =
    | T_int of int
    | T_u   of u

  and u =
    | U_int of int
    | U_t   of t
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t)
      , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) )
    =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid          = "\155\175@s\211\217\158\016\001\0275\166\241\1558\144"
      ; types         =
          [ ( "t"
            , Variant
                { ignore_capitalization = false
                ; alts                  =
                    [ "T_int", [ One (Implicit_var 0) ]; "T_u", [ One (Recursive "u") ] ]
                } )
          ; ( "u"
            , Variant
                { ignore_capitalization = false
                ; alts                  =
                    [ "U_int", [ One (Implicit_var 0) ]; "U_t", [ One (Recursive "t") ] ]
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
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) = Ref ("t", _the_group)
    and (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) = Ref ("u", _the_group) in
    t_sexp_grammar, u_sexp_grammar
  ;;

  let _ = t_sexp_grammar
  and _ = u_sexp_grammar

  [@@@end]
end
