open! Base

type ('a, _, 'b) t = 'a * 'b

and u = (string, int, float) t [@@deriving_inline sexp_grammar]

let _ = fun (_ : ('a, _, 'b) t) -> ()
let _ = fun (_ : u            ) -> ()

let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t)
    , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Grammar.t) )
  =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Grammar.generic_group) =
    { implicit_vars = [ "string"; "int"; "float" ]
    ; ggid          = "\188\229A\199\004o'\003\160n\138\189k\130y]"
    ; types         =
        [ ( "t"
          , Explicit_bind
              ([ "a"; "_"; "b" ], List [ One (Explicit_var 0); One (Explicit_var 2) ]) )
        ; "u", Apply (Recursive "t", [ Implicit_var 0; Implicit_var 1; Implicit_var 2 ])
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Grammar.group) =
    { gid            = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; apply_implicit = [ string_sexp_grammar; int_sexp_grammar; float_sexp_grammar ]
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
