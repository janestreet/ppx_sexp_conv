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

let stackify_portable_unboxed_local_args () =
  Deriving.Args.(
    empty +> flag "stackify" +> flag "portable" +> flag "unboxed" +> flag "localize")
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

  let pattern_extension =
    Extension.V3.declare name Pattern Ast_pattern.(ptyp __) E.pattern_extension
  ;;

  let () =
    Driver.register_transformation
      "Ppxlib.Deriving.sexp_grammar"
      ~rules:
        [ Context_free.Rule.extension expr_extension
        ; Context_free.Rule.extension type_extension
        ; Context_free.Rule.extension pattern_extension
        ]
  ;;
end

module Sexp_of = struct
  module E = Ppx_sexp_conv_expander.Sexp_of

  type stackify_kind =
    | For_deriving
    | For_extension

  type localize_kind = For_deriving

  type kind =
    | Default
    | Stackify of stackify_kind
    | Localize of localize_kind

  let name = function
    | Default -> "sexp_of"
    | Stackify For_deriving -> "sexp_of__stack"
    | Stackify For_extension -> "sexp_of__stack"
    | Localize For_deriving -> "sexp_of__local"
  ;;

  let str_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_local_args ())
      (fun ~loc ~path tds stackify portable unboxed localize ->
         E.str_type_decl ~loc ~path ~unboxed tds ~stackify ~portable ~localize)
  ;;

  let str_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.str_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable ~localize:false)
  ;;

  let str_type_decl_local =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.str_type_decl ~loc ~path ~unboxed tds ~stackify:false ~portable ~localize:true)
  ;;

  let str_exception =
    Deriving.Generator.make
      Deriving.Args.(empty +> flag "nonportable__magic_unsafe_in_parallel_programs")
      (fun ~loc ~path ec nonportable_magic ->
        E.str_exception ~loc ~path ~nonportable_magic ec)
  ;;

  let sig_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_local_args ())
      (fun ~loc ~path tds stackify portable unboxed localize ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify ~portable ~localize)
  ;;

  let sig_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable ~localize:false)
  ;;

  let sig_type_decl_local =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:false ~portable ~localize:true)
  ;;

  let sig_exception = Deriving.Generator.make_noarg E.sig_exception

  let deriver =
    Deriving.add
      (name Default)
      ~str_type_decl
      ~str_exception
      ~sig_type_decl
      ~sig_exception
  ;;

  let deriver_stack =
    Deriving.add
      (name (Stackify For_deriving))
      ~str_type_decl:str_type_decl_stack
      ~sig_type_decl:sig_type_decl_stack
  ;;

  let deriver_local =
    Deriving.add
      (name (Localize For_deriving))
      ~str_type_decl:str_type_decl_local
      ~sig_type_decl:sig_type_decl_local
  ;;

  let () =
    List.iter
      [ Default, false; Stackify For_extension, true ]
      ~f:(fun (kind, stackify) ->
        register_extension (name kind) (fun ~loc:_ ~path:_ ctyp ->
          E.core_type ctyp ~stackify))
  ;;

  let () =
    let rules =
      List.concat_map
        [ Default, false; Stackify For_extension, true ]
        ~f:(fun (kind, stackify) ->
          [ Context_free.Rule.extension
              (Extension.declare
                 (name kind)
                 Core_type
                 Ast_pattern.(ptyp __)
                 (fun ~loc:_ ~path:_ ty -> E.type_extension ty ~stackify ~localize:false))
          ; Context_free.Rule.extension
              (Extension.declare
                 (name kind)
                 Pattern
                 Ast_pattern.(ptyp __)
                 (fun ~loc:_ ~path:_ ty -> E.pattern_extension ty ~stackify))
          ])
    in
    Driver.register_transformation (name Default) ~rules
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
let sexp_of__local = Sexp_of.deriver_local
let of_sexp = Of_sexp.deriver
let of_sexp_poly = Of_sexp_poly.deriver
let sexp_grammar = Sexp_grammar.deriver

module Sexp_in_sig = struct
  module E = Ppx_sexp_conv_expander.Sig_sexp

  let sig_type_decl =
    Deriving.Generator.make
      (stackify_portable_unboxed_local_args ())
      (fun ~loc ~path tds stackify portable unboxed localize ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify ~portable ~localize)
  ;;

  let sig_type_decl_stack =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:true ~portable ~localize:false)
  ;;

  let sig_type_decl_local =
    Deriving.Generator.make
      (portable_and_unboxed_args ())
      (fun ~loc ~path tds portable unboxed ->
         E.sig_type_decl ~loc ~path ~unboxed tds ~stackify:false ~portable ~localize:true)
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

  let deriver_local =
    Deriving.add
      "ppx_sexp_conv: let this be a string that wouldn't parse if put in the source \
       _local"
      ~sig_type_decl:sig_type_decl_local
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

let sexp__local =
  Deriving.add_alias
    "sexp__local"
    [ sexp_of__local; of_sexp ]
    ~sig_type_decl:[ Sexp_in_sig.deriver_local ]
    ~str_exception:[ sexp_of__local ]
    ~sig_exception:[ sexp_of__local ]
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
let registered = ()
