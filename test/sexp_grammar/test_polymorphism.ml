open! Base

type ('a, _, 'b) t = 'a * 'b

and u = (string, int, float) t [@@deriving_inline sexp_grammar]

let _ = fun (_ : ('a, _, 'b) t) -> ()
let _ = fun (_ : u) -> ()

let ( (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)
    , (u_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) )
  =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = [ "string"; "int"; "float" ]
    ; ggid = "\188\229A\199\004o'\003\160n\138\189k\130y]"
    ; types =
        [ ( "t"
          , Tyvar_parameterize
              ([ "a"; "_"; "b" ], List [ One (Tyvar_index 0); One (Tyvar_index 2) ]) )
        ; ( "u"
          , Tyvar_instantiate
              (Recursive "t", [ Tycon_index 0; Tycon_index 1; Tycon_index 2 ]) )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = [ string_sexp_grammar; int_sexp_grammar; float_sexp_grammar ]
    ; generic_group = _the_generic_group
    ; origin = "test_polymorphism.ml"
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

[@@@end]
