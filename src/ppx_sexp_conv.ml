(* sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open Ppx_core

module Type_conv = Ppx_type_conv.Std.Type_conv
module Attrs     = Ppx_sexp_conv_expander.Attrs

module Sexp_of = struct
  module E = Ppx_sexp_conv_expander.Sexp_of
  let name = "sexp_of"

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
    Type_conv.add name
      ~str_type_decl
      ~str_exception
      ~sig_type_decl
      ~sig_exception
      ~extension
  ;;

  let () =
    Ppx_driver.register_transformation name
      ~rules:[ Context_free.Rule.extension
                 (Extension.declare name
                    Core_type Ast_pattern.(ptyp __)
                    (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
             ]
  ;;
end

module Of_sexp = struct
  module E = Ppx_sexp_conv_expander.Of_sexp
  let name = "of_sexp"

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

  let () =
    Ppx_driver.register_transformation name
      ~rules:[ Context_free.Rule.extension
                 (Extension.declare name
                    Core_type Ast_pattern.(ptyp __)
                    (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
             ]
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

let sexp_of = Sexp_of.deriver
let of_sexp = Of_sexp.deriver
let of_sexp_poly = Of_sexp_poly.deriver

let sexp =
  Type_conv.add_alias "sexp" [sexp_of; of_sexp]
    ~str_exception:[sexp_of]
    ~sig_exception:[sexp_of]

let sexp_poly =
  Type_conv.add_alias "sexp_poly" [sexp_of; of_sexp_poly]
