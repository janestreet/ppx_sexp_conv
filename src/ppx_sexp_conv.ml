(* sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open StdLabels
open Ppxlib

let register_extension name f =
  let extension = Extension.declare name Expression Ast_pattern.(ptyp __) f in
  Driver.register_transformation
    ("Ppxlib.Deriving." ^ name)
    ~rules:[ Context_free.Rule.extension extension ]
;;

let portable_and_unboxed_args () =
  Deriving.Args.(empty +> flag "portable" +> flag "unboxed")
;;

let nonportable_arg () = Deriving.Args.(empty +> flag "nonportable")

let stackify_portable_unboxed_args () =
  Deriving.Args.(empty +> flag "stackify" +> flag "portable" +> flag "unboxed")
;;

module Sexp_grammar = struct
  module E = Ppx_sexp_conv_expander.Sexp_grammar

  let name = "sexp_grammar"
  let flags = Deriving.Args.(empty +> flag "tags_of_doc_comments")
  let str_type_decl = Deriving.Generator.V2.make flags E.str_type_decl

  let sig_type_decl =
    Deriving.Generator.V2.make (nonportable_arg ()) (fun ~ctxt tds nonportable ->
      E.sig_type_decl ~ctxt tds ~nonportable)
  ;;

  let deriver = Deriving.add name ~sig_type_decl ~str_type_decl

  (* We default to [tags_of_doc_comments=true] in this case, because doc comments in a
     [%sexp_grammar] expression have no other purpose. *)
  let expr_extension =
    Extension.V3.declare
      name
      Expression
      Ast_pattern.(ptyp __)
      (E.core_type ~tags_of_doc_comments:true)
  ;;

  let type_extension =
    Extension.V3.declare name Core_type Ast_pattern.(ptyp __) E.type_extension
  ;;

  let () =
    Driver.register_transformation
      "Ppxlib.Deriving.sexp_grammar"
      ~rules:
        [ Context_free.Rule.extension expr_extension
        ; Context_free.Rule.extension type_extension
        ]
  ;;
end

module Sexp_of = struct
  module E = Ppx_sexp_conv_expander.Sexp_of

  type stackify_kind =
    | For_deriving
    | For_extension

  let name ~stackify =
    match stackify with
    | None -> "sexp_of"
    | Some For_deriving -> "sexp_of__stack"
    | Some For_extension -> "sexp_of_stack"
  ;;

  let str_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_args ())
      (fun ~loc ~path tds stackify portable unboxed ->
         E.str_type_decl ~loc ~path ~unboxed tds ~stackify ~portable)
  ;;

  let str_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.str_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable)
  ;;

  let str_exception = Deriving.Generator.make_noarg E.str_exception

  let sig_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_args ())
      (fun ~loc ~path tds stackify portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify ~portable)
  ;;

  let sig_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable)
  ;;

  let sig_exception = Deriving.Generator.make_noarg E.sig_exception

  let deriver =
    Deriving.add
      (name ~stackify:None)
      ~str_type_decl
      ~str_exception
      ~sig_type_decl
      ~sig_exception
  ;;

  let deriver_stack =
    Deriving.add
      (name ~stackify:(Some For_deriving))
      ~str_type_decl:str_type_decl_stack
      ~sig_type_decl:sig_type_decl_stack
  ;;

  let () =
    List.iter [ None; Some For_extension ] ~f:(fun stackify ->
      register_extension (name ~stackify) (fun ~loc:_ ~path:_ ctyp ->
        E.core_type ctyp ~stackify:(Option.is_some stackify)))
  ;;

  let () =
    let rules =
      List.concat_map [ None; Some For_extension ] ~f:(fun stackify ->
        [ Context_free.Rule.extension
            (Extension.declare
               (name ~stackify)
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty ->
                 E.type_extension ty ~stackify:(Option.is_some stackify)))
        ; Context_free.Rule.extension
            (Extension.declare
               (name ~stackify)
               Pattern
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty ->
                 E.pattern_extension ty ~stackify:(Option.is_some stackify)))
        ])
    in
    Driver.register_transformation (name ~stackify:None) ~rules
  ;;
end

module Of_sexp = struct
  module E = Ppx_sexp_conv_expander.Of_sexp

  let name = "of_sexp"

  let str_type_decl =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.str_type_decl ~loc ~path ~unboxed tds ~poly:false ~portable)
  ;;

  let sig_type_decl =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~poly:false ~loc ~path ~unboxed tds ~portable)
  ;;

  let deriver = Deriving.add name ~str_type_decl ~sig_type_decl
  let extension ~loc:_ ~path ctyp = E.core_type ~path ctyp
  let () = register_extension name extension

  let () =
    Driver.register_transformation
      name
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
        ; Context_free.Rule.extension
            (Extension.declare
               name
               Pattern
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.pattern_extension ty))
        ]
  ;;
end

module Of_sexp_poly = struct
  module E = Ppx_sexp_conv_expander.Of_sexp

  let str_type_decl =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.str_type_decl ~poly:true ~loc ~path ~unboxed tds ~portable)
  ;;

  let sig_type_decl =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~poly:true ~loc ~path ~unboxed tds ~portable)
  ;;

  let deriver = Deriving.add "of_sexp_poly" ~sig_type_decl ~str_type_decl
end

let sexp_of = Sexp_of.deriver
let sexp_of__stack = Sexp_of.deriver_stack
let of_sexp = Of_sexp.deriver
let of_sexp_poly = Of_sexp_poly.deriver
let sexp_grammar = Sexp_grammar.deriver

module Sexp_in_sig = struct
  module E = Ppx_sexp_conv_expander.Sig_sexp

  let sig_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_args ())
      (fun ~loc ~path tds stackify portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify ~portable)
  ;;

  let sig_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable)
  ;;

  let deriver =
    Deriving.add
      "ppx_sexp_conv: let this be a string that wouldn't parse if put in the source"
      ~sig_type_decl
  ;;

  let deriver_stack =
    Deriving.add
      "ppx_sexp_conv: let this be a string that wouldn't parse if put in the source \
       _stack"
      ~sig_type_decl:sig_type_decl_stack
  ;;
end

let sexp =
  Deriving.add_alias
    "sexp"
    [ sexp_of; of_sexp ]
    ~sig_type_decl:[ Sexp_in_sig.deriver ]
    ~str_exception:[ sexp_of ]
    ~sig_exception:[ sexp_of ]
;;

let sexp__stack =
  Deriving.add_alias
    "sexp__stack"
    [ sexp_of__stack; of_sexp ]
    ~sig_type_decl:[ Sexp_in_sig.deriver_stack ]
    ~str_exception:[ sexp_of__stack ]
    ~sig_exception:[ sexp_of__stack ]
;;

let sexp_poly = Deriving.add_alias "sexp_poly" [ sexp_of; of_sexp_poly ]
