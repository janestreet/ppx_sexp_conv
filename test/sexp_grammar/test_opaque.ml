open! Base

type t = (int[@sexp.opaque]) list [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = [ "list" ]
    ; ggid = "x\162\137\186\229\219\000\136\155\017TYunT\014"
    ; types =
        [ ( "t"
          , Tyvar_instantiate
              (Tycon_index 0, [ Grammar Ppx_sexp_conv_lib.Conv.opaque_sexp_grammar ]) )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = [ list_sexp_grammar ]
    ; generic_group = _the_generic_group
    ; origin = "test_opaque.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

let _ = t_sexp_grammar

[@@@end]
