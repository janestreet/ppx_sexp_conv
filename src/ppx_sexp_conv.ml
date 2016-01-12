(* sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open! StdLabels
open Ppx_core.Std

module Type_conv = Ppx_type_conv.Std.Type_conv
module Attrs     = Ppx_sexp_conv_expander.Attrs

module Sexp_of = struct
  module E = Ppx_sexp_conv_expander.Sexp_of

  let str_type_decl =
    Type_conv.Generator.make_noarg E.str_type_decl
      ~attributes:[ Attribute.T Attrs.default
                  ; Attribute.T Attrs.drop_default
                  ; Attribute.T Attrs.drop_if
                  ]
  ;;

  let str_exception =
    Type_conv.Generator.make_noarg E.str_exception
  ;;

  let sig_type_decl =
    Type_conv.Generator.make_noarg E.sig_type_decl
  ;;

  let sig_exception =
    Type_conv.Generator.make_noarg E.sig_exception
  ;;

  let extension ~loc:_ ~path:_ ctyp = E.core_type ctyp

  let deriver =
    Type_conv.add "sexp_of"
      ~str_type_decl
      ~str_exception
      ~sig_type_decl
      ~sig_exception
      ~extension
  ;;
end

module Of_sexp = struct
  module E = Ppx_sexp_conv_expander.Of_sexp

  let str_type_decl =
    Type_conv.Generator.make_noarg (E.str_type_decl ~poly:false)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl =
    Type_conv.Generator.make_noarg (E.sig_type_decl ~poly:false)
  ;;

  let extension ~loc:_ ~path ctyp = E.core_type ~path ctyp

  let deriver =
    Type_conv.add "of_sexp"
      ~str_type_decl
      ~sig_type_decl
      ~extension
  ;;
end

module Of_sexp_poly = struct
  module E = Ppx_sexp_conv_expander.Of_sexp

  let str_type_decl =
    Type_conv.Generator.make_noarg (E.str_type_decl ~poly:true)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl =
    Type_conv.Generator.make_noarg (E.sig_type_decl ~poly:true)
  ;;

  let deriver =
    Type_conv.add "of_sexp_poly"
      ~sig_type_decl
      ~str_type_decl
  ;;
end

let () =
  Type_conv.add_alias "sexp" [Sexp_of.deriver; Of_sexp.deriver]
    ~str_exception:[Sexp_of.deriver]
    ~sig_exception:[Sexp_of.deriver]
  |> Type_conv.ignore;
  Type_conv.add_alias "sexp_poly" [Sexp_of.deriver; Of_sexp_poly.deriver]
  |> Type_conv.ignore;
;;
