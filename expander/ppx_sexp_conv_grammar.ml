open! Base
open! Ppxlib
open! Ast_builder.Default
open! Sexp.Private.Raw_grammar

module Sexp_grammar_lifter = struct
  module Atom = struct
    type t = Sexp.Private.Raw_grammar.Atom.t =
      | String
      | Bool
      | Char
      | Float
      | Int
      | This of
          { ignore_capitalization : bool
          ; string : string
          }
    [@@deriving traverse_lift]

    let lifter ~loc =
      object
        inherit [expression] lift

        inherit Ppxlib_metaquot_lifters.expression_lifters loc
      end
    ;;
  end

  type atom = Atom.t
  type var_name = Sexp.Private.Raw_grammar.var_name
  type type_name = Sexp.Private.Raw_grammar.type_name

  let lift_string ~loc s = pexp_constant ~loc (Pconst_string (s, None))
  let lift_var_name = lift_string
  let lift_type_name = lift_string

  type 't type_ = 't Sexp.Private.Raw_grammar.type_ =
    | Any
    | Tyvar_instantiate of 't type_ * 't type_ list
    | Atom of atom
    | Tyvar_parameterize of var_name list * 't type_
    | Tyvar_index of int
    | Grammar of 't
    | Tycon_index of int
    | List of 't sequence_type
    | Option of 't type_
    | Record of 't record_type
    | Recursive of type_name
    | Union of 't type_ list
    | Variant of 't variant_type

  and 't sequence_type = 't component list

  and 't component = 't Sexp.Private.Raw_grammar.component =
    | One of 't type_
    | Optional of 't type_
    | Many of 't type_
    | Fields of 't record_type

  and 't variant_type = 't Sexp.Private.Raw_grammar.variant_type =
    { ignore_capitalization : bool
    ; alts : (label * 't sequence_type) list
    }

  and 't record_type = 't Sexp.Private.Raw_grammar.record_type =
    { allow_extra_fields : bool
    ; fields : (label * 't field) list
    }

  and 't field = 't Sexp.Private.Raw_grammar.field =
    { optional : bool
    ; args : 't sequence_type
    }

  (* [traverse_lift] generates references to a type named [t] even if it doesn't exist. So
     make sure it exists. *)
  and t = unit [@@deriving traverse_lift, traverse_map]

  let lifter ~loc =
    let atom_lifter = Atom.lifter ~loc in
    object
      inherit [expression] lift

      inherit Ppxlib_metaquot_lifters.expression_lifters loc

      method atom atom = atom_lifter#t atom

      method label = lift_string ~loc

      method var_name = lift_var_name ~loc

      method type_name = lift_type_name ~loc
    end
  ;;
end

module Str_generate_sexp_grammar = struct
  module Var_name = String
  module Type_name = String

  module Longident = struct
    include Longident
    include (val Comparator.make ~compare ~sexp_of_t)
  end

  let debug_s message values =
    Caml.prerr_endline (Sexp.to_string_hum (Sexp.message message values))
  ;;

  let _ = debug_s

  (* [grammar] and [generic_group] form the generic part of grammar: it's the information
     we can compute statically  from the AST *)
  type grammar =
    | Inline of grammar Sexp.Private.Raw_grammar.type_ loc
    (* E.g.,[Ref_same_group "t"] vs [Ref_other_group (Ldot (Lident "t", "Foo"))]. *)
    | Ref_same_group of Type_name.t loc
    | Ref_other_group of Longident.t loc

  type generic_group =
    { tycon_names : var_name list
    ; ggid : generic_group_id
    ; types : (type_name * grammar type_) list
    }

  (* [group] has extra information collected from scope at runtime, necessary to interpret
     [tycon_names] in [generic_group] *)
  type group = { instantiate_tycons : grammar list }

  let id_mapper =
    object
      inherit Sexp_grammar_lifter.map

      method var_name x = x

      method unit () = ()

      method type_name x = x

      method list f l = List.map l ~f

      method label x = x

      method int x = x

      method bool x = x

      method atom x = x
    end
  ;;

  let erase_locs
    : grammar Sexp.Private.Raw_grammar.type_ -> grammar Sexp.Private.Raw_grammar.type_
    =
    id_mapper#type_ (function
      | Inline x -> Inline { x with loc = Location.none }
      | Ref_same_group x -> Ref_same_group { x with loc = Location.none }
      | Ref_other_group x -> Ref_other_group { x with loc = Location.none })
  ;;

  let make_generic_group ~tycon_names ~types =
    let ggid =
      let types = List.map types ~f:(fun (x, g) -> x, erase_locs g) in
      Caml.Digest.string (Caml.Marshal.to_string types [])
    in
    { tycon_names; types; ggid }
  ;;

  type t =
    { grammars : grammar Map.M(Type_name).t
    ; generic_group : generic_group
    ; group : group
    ; loc : Location.t
    ; module_path : string
    }

  let impossible ~loc s =
    Location.raise_errorf ~loc "ppx_sexp_conv: sexp_grammar: Impossible! %s" s
  ;;

  let not_supported ~loc s =
    Location.raise_errorf ~loc "ppx_sexp_conv: sexp_grammar doesn't support %s" s
  ;;

  module Env : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val create : rec_flag -> type_declaration list -> t
    val is_in_this_recursive_group : t -> Type_name.t -> bool

    (** [add_tycon ~loc t id] registers [id] as a type constructor and returns the correct
        [Tycon_index _] to refer to [id]. *)
    val add_tycon : loc:Location.t -> t -> Longident.t -> 'a type_

    (** [type_for_var ~loc t type_name var] returns the right grammar (typically
        [Tyvar_index _]) for [var_name] in [type_name] in this recursive group. *)
    val type_for_var : loc:Location.t -> t -> Type_name.t -> Var_name.t -> 'a type_

    (** Register some type variables as being universally quantified. *)
    val within_polymorphic_record_field : t -> Var_name.t loc list -> t

    (** [with_tyvar_parameterize ~loc t type_name type_] wraps [type_] in
        [Tyvar_parameterize] if [t] indicates that [type_name] has type variables that might
        otherwise be free in [type_]. *)
    val with_tyvar_parameterize
      :  loc:Location.t
      -> t
      -> Type_name.t
      -> 'a type_
      -> 'a type_

    (** For the [tycon_names] field of a [generic_group] value. *)
    val tycon_names : t -> Type_name.t list

    (** For the [instantiate_tycons] field of a [group] value *)
    val instantiate_tycons : t -> grammar list
  end = struct
    module Type_variables = struct
      type t = Var_name.t option array

      let sexp_of_t : t -> Sexp.t = Array.sexp_of_t (Option.sexp_of_t Var_name.sexp_of_t)

      let create td =
        List.map td.ptype_params ~f:(fun (ctype, _variance) ->
          match ctype.ptyp_desc with
          | Ptyp_any -> None
          | Ptyp_var s -> Some s
          | _ -> Location.raise_errorf ~loc:ctype.ptyp_loc "not a type parameter")
        |> Array.of_list
      ;;

      let find t name =
        Array.find_mapi t ~f:(fun i var_name ->
          match Option.equal String.equal var_name (Some name) with
          | true -> Some i
          | false -> None)
      ;;

      let parameterize t type_ =
        match t with
        | [||] -> type_
        | _ ->
          let variables = Array.map t ~f:(Option.value ~default:"_") |> Array.to_list in
          Tyvar_parameterize (variables, type_)
      ;;
    end

    (* The kitchen-sink type for information used as we generate the grammars. *)
    type init =
      { (* Type variables of type declarations in this recursive group. *)
        tyvars : Type_variables.t Map.M(Type_name).t
      ; (* Type constructors used in this mutually recursive group, filled in as we traverse
           the type declarations. The ints represent the type constructor indexes, for use
           with [Tycon_index] *)
        mutable tycons : int loc Map.M(Longident).t
      ; rec_flag : rec_flag
      }

    type t =
      | Init of init
      | Within_polymorphic_record_field of
          { (* [first_class_polymorphism] represents universally-quantified variables
               introduced by polymorphic record fields. (We actually do support polymorphic
               record fields!)

               Values of these types can't occur in serializable values so we're
               substituting them with the unsatisfiable grammar [Union []]. *)
            first_class_polymorphism : Set.M(Var_name).t
          ; t : t
          }

    let sexp_of_init { tyvars; tycons; rec_flag } : Sexp.t =
      List
        [ Atom "T"
        ; List
            [ List
                [ Atom "tyvars"
                ; Map.sexp_of_m__t (module Type_name) Type_variables.sexp_of_t tyvars
                ]
            ; List
                [ Atom "tycons"
                ; Map.sexp_of_m__t
                    (module struct
                      type t = Longident.t

                      let sexp_of_t t = sexp_of_string (Longident.name t)
                    end)
                    (fun { loc = _; txt = int } -> sexp_of_int int)
                    tycons
                ]
            ; List [ Atom "rec_flag"; Ast_traverse.sexp_of#rec_flag rec_flag ]
            ]
        ]
    ;;

    let rec sexp_of_t t : Sexp.t =
      match t with
      | Init init -> sexp_of_init init
      | Within_polymorphic_record_field { first_class_polymorphism; t } ->
        List
          [ Atom "Within_polymorphic_record_field"
          ; List
              [ List
                  [ Atom "first_class_polymorphism"
                  ; Set.sexp_of_m__t (module Type_name) first_class_polymorphism
                  ]
              ; List [ Atom "t"; sexp_of_t t ]
              ]
          ]
    ;;

    let _ = sexp_of_t

    let create rec_flag tds : t =
      Init
        { tyvars =
            List.map tds ~f:(fun td -> td.ptype_name.txt, Type_variables.create td)
            |> Map.of_alist_exn (module Type_name)
        ; tycons = Map.empty (module Longident)
        ; rec_flag
        }
    ;;

    let rec get_init t =
      match t with
      | Init init -> init
      | Within_polymorphic_record_field { first_class_polymorphism = _; t } -> get_init t
    ;;

    let add_tycon ~loc t lident =
      let init = get_init t in
      match Map.find init.tycons lident with
      | Some i -> Tycon_index i.txt
      | None ->
        let i = Map.length init.tycons in
        init.tycons <- Map.add_exn init.tycons ~key:lident ~data:{ loc; txt = i };
        Tycon_index i
    ;;

    let sort_by_index m ~f =
      Map.to_alist m |> List.sort ~compare:(fun (_, i) (_, j) -> compare (f i) (f j))
    ;;

    let tycon_names t =
      let init = get_init t in
      List.map
        (sort_by_index init.tycons ~f:(fun x -> x.txt))
        ~f:(fun (lid, _) -> Longident.name lid)
    ;;

    let instantiate_tycons t =
      let init = get_init t in
      List.map
        (sort_by_index init.tycons ~f:(fun x -> x.txt))
        ~f:(fun (lident, { loc; txt = _ }) -> Ref_other_group { loc; txt = lident })
    ;;

    let is_in_this_recursive_group t type_name =
      let init = get_init t in
      match init.rec_flag with
      | Nonrecursive -> false
      | Recursive -> Map.mem init.tyvars type_name
    ;;

    let variables_of_type ~loc init type_name =
      match Map.find init.tyvars type_name with
      | None -> impossible ~loc ("unknown type name: " ^ type_name)
      | Some variables -> variables
    ;;

    let rec type_for_var ~loc t type_name var_name =
      match t with
      | Within_polymorphic_record_field { first_class_polymorphism; t } ->
        (match Set.mem first_class_polymorphism var_name with
         | true -> Union []
         | false -> type_for_var ~loc t type_name var_name)
      | Init init ->
        (match Type_variables.find (variables_of_type ~loc init type_name) var_name with
         | None ->
           Location.raise_errorf ~loc "no sexp grammar for type parameter '%s" var_name
         | Some i -> Tyvar_index i)
    ;;

    let within_polymorphic_record_field t type_names =
      Within_polymorphic_record_field
        { first_class_polymorphism =
            List.map type_names ~f:(fun { loc = _; txt } -> txt)
            |> Set.of_list (module Type_name)
        ; t
        }
    ;;

    let with_tyvar_parameterize ~loc t type_name type_ =
      let init = get_init t in
      Type_variables.parameterize (variables_of_type ~loc init type_name) type_
    ;;
  end

  let _ = Env.sexp_of_t

  module Row_field = struct
    type t =
      | Inherit of core_type
      | Tag_nullary of label
      (** [Tag_sexp_list (label, ctype)] means one of these forms:
          - [`Label of ctype sexp_list]
          - [`Label of ctype list [@sexp.list]]. *)
      | Tag_sexp_list of label * core_type
      | Tag_tuple of label * core_type

    let core_type_within_sexp_list args sexp_list_attribute attribute_of =
      match args with
      | [ [%type: [%t? type_] sexp_list] ] -> Some type_
      | [ [%type: [%t? type_]] ]
        when Option.is_some (Attribute.get sexp_list_attribute attribute_of) ->
        (match type_ with
         | [%type: [%t? type_] list] -> Some type_
         | _ -> Attrs.invalid_attribute ~loc:type_.ptyp_loc sexp_list_attribute "_ list")
      | _ -> None
    ;;

    let create row_field =
      match row_field.prf_desc with
      | Rinherit ctype -> Inherit ctype
      | Rtag ({ loc; txt = label }, nullary, possible_arg_types) ->
        (match nullary, possible_arg_types with
         | true, [] -> Tag_nullary label
         | false, ([ ctype ] as args) ->
           (match core_type_within_sexp_list args Attrs.list_poly row_field with
            | Some ctype -> Tag_sexp_list (label, ctype)
            | None -> Tag_tuple (label, ctype))
         | false, [] ->
           impossible ~loc "polymorphic variant constructor neither nullary nor not"
         | true, _ :: _ | false, _ :: _ :: _ ->
           not_supported
             ~loc
             "polymorphic variants with intersection types ([`A of _ & _])")
    ;;
  end

  module Opaque = struct
    type t =
      | Not_opaque of core_type
      | Opaque

    let create ctype : t =
      match Attribute.get Attrs.opaque ctype with
      | Some () -> Opaque
      | None ->
        (match ctype.ptyp_desc with
         | Ptyp_constr ({ loc = _; txt = Lident "sexp_opaque" }, _) -> Opaque
         | _ -> Not_opaque ctype)
    ;;
  end

  let sexp_grammar_suffix = "_sexp_grammar"

  (* Given [(u, v) A.B.F(M)(N).t], [type_of_type_constructor] returns
     {[
       fun [u_grammar; v_grammar] ->
         Tyvar_instantiate
           ( Grammar A.B.f__t_sexp_grammar
           , [ M.t_sexp_grammar; N.t_sexp_grammar; u_grammar; v_grammar ])

     ]}, which is the usual scheme implemented by [Ast_builder.*.type_constr_conv] but
     defunctionalized because polymorphic sexp_grammars are constructors not functions.
  *)
  let type_of_type_constructor ~loc env lident args =
    let apply_if_args tycon = function
      | [] -> tycon
      | args -> Tyvar_instantiate (tycon, args)
    in
    match lident with
    | Lident type_name when Env.is_in_this_recursive_group env type_name ->
      apply_if_args (Recursive type_name) args
    | Lident _ | Ldot ((Lident _ | Ldot _), _) ->
      apply_if_args (Env.add_tycon ~loc env lident) args
    | Lapply _ -> impossible ~loc ("Expected type name, got " ^ Longident.name lident)
    | Ldot ((Lapply _ as module_path), n) ->
      let suffix_n functor_ = String.uncapitalize functor_ ^ "__" ^ n in
      let rec gather_lapply functor_args : Longident.t -> Longident.t * _ = function
        | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
        | Lident functor_ -> Lident (suffix_n functor_), functor_args
        | Ldot (functor_path, functor_) ->
          Ldot (functor_path, suffix_n functor_), functor_args
      in
      let ident, functor_args = gather_lapply [] module_path in
      let tycon = Env.add_tycon ~loc env ident in
      let functor_args =
        List.map functor_args ~f:(fun functor_arg ->
          Env.add_tycon ~loc env (Ldot (functor_arg, "t")))
      in
      Tyvar_instantiate (tycon, functor_args @ args)
  ;;

  (* We use a fully qualified module path here because it does not make sense for the user
     to shadow these grammars with other values. *)
  let unsupported_builtin ~loc type_name =
    let ( +.+ ) a b = Ldot (a, b) in
    Grammar
      (Ref_other_group { loc; txt = Lident "Ppx_sexp_conv_lib" +.+ "Conv" +.+ type_name })
  ;;

  let type_of_core_type env0 type_name ctype =
    let rec type_of_core_type env ctype =
      match Opaque.create ctype with
      | Opaque -> unsupported_builtin ~loc:ctype.ptyp_loc "opaque"
      | Not_opaque { ptyp_desc; ptyp_loc = loc; ptyp_attributes = _; ptyp_loc_stack = _ }
        ->
        (match ptyp_desc with
         | Ptyp_any ->
           (* For consistency with [%of_sexp: _] which treats [_] as unsatisfiable. *)
           Union []
         | Ptyp_var s -> Env.type_for_var ~loc env type_name s
         | Ptyp_arrow (_, _, _) -> unsupported_builtin ~loc "fun"
         | Ptyp_tuple core_types ->
           List
             (List.map core_types ~f:(fun core_type ->
                One (type_of_core_type env core_type)))
         | Ptyp_constr (ident, args) ->
           type_of_type_constructor
             ~loc
             env
             ident.txt
             (List.map args ~f:(type_of_core_type env))
         | Ptyp_object (_, _) -> not_supported ~loc "objects"
         | Ptyp_class (_, _) -> not_supported ~loc "classes"
         | Ptyp_alias (_, _) -> not_supported ~loc "aliases"
         | Ptyp_variant (row_fields, Closed, None) -> type_of_row_fields env row_fields
         | Ptyp_variant (_, Open, _) | Ptyp_variant (_, _, Some _) ->
           not_supported ~loc "polymorphic variants with < or >"
         | Ptyp_poly (first_class_variables, core_type) ->
           let env = Env.within_polymorphic_record_field env first_class_variables in
           type_of_core_type env core_type
         | Ptyp_package _ -> not_supported ~loc "packed modules"
         | Ptyp_extension _ -> not_supported ~loc "extension nodes")
    and type_of_row_fields env row_fields =
      let alts, inherits =
        List.partition_map row_fields ~f:(fun row_field ->
          match Row_field.create row_field with
          | Inherit ctype -> Second ((type_of_core_type env) ctype)
          | Tag_nullary label -> First (label, [])
          | Tag_sexp_list (label, ctype) ->
            First (label, [ Many (type_of_core_type env ctype) ])
          | Tag_tuple (label, ctype) ->
            First (label, [ One (type_of_core_type env ctype) ]))
      in
      let types =
        match alts with
        | [] -> None
        | _ :: _ -> Some (Variant { ignore_capitalization = false; alts })
      in
      match Option.to_list types @ inherits with
      | [] -> unsupported_builtin ~loc:ctype.ptyp_loc "empty"
      | [ type_ ] -> type_
      | _ :: _ as types -> Union types
    in
    (type_of_core_type env0) ctype
  ;;

  let record_type_of_label_declarations env type_name lds ~allow_extra_fields =
    { allow_extra_fields
    ; fields =
        List.map lds ~f:(fun ld ->
          let { pld_name; pld_mutable = _; pld_type; pld_loc; pld_attributes = _ } =
            ld
          in
          let field =
            match Attrs.Record_field_handler.Of_sexp.create ~loc:pld_loc ld with
            | None ->
              { optional = false
              ; args = [ One (type_of_core_type env type_name pld_type) ]
              }
            | Some (`default _ | `omit_nil) ->
              { optional = true
              ; args = [ One (type_of_core_type env type_name pld_type) ]
              }
            | Some `sexp_bool -> { optional = true; args = [] }
            | Some (`sexp_array core_type | `sexp_list core_type) ->
              { optional = true
              ; args =
                  [ One (List [ Many (type_of_core_type env type_name core_type) ]) ]
              }
            | Some (`sexp_option core_type) ->
              { optional = true
              ; args = [ One (type_of_core_type env type_name core_type) ]
              }
          in
          pld_name.txt, field)
    }
  ;;

  module Constructor_declaration = struct
    type t =
      | Record of
          { allow_extra_fields : bool
          ; fields : label_declaration list
          ; label : label
          }
      | Tuple_regular of label * core_type list
      (** [Tuple_sexp_list (label, ctype)] means one of these forms:
          - [Label of ctype sexp_list]
          - [Label of ctype list [@sexp.list]]. *)
      | Tuple_sexp_list of label * core_type

    let create ({ pcd_name; pcd_args; _ } as cd) =
      let label = pcd_name.txt in
      match pcd_args with
      | Pcstr_record fields ->
        Record
          { allow_extra_fields =
              Option.is_some (Attribute.get Attrs.allow_extra_fields_cd cd)
          ; fields
          ; label
          }
      | Pcstr_tuple args ->
        (match Row_field.core_type_within_sexp_list args Attrs.list_variant cd with
         | Some ctype -> Tuple_sexp_list (label, ctype)
         | None -> Tuple_regular (label, args))
    ;;
  end

  let variant env type_name constructor_declarations =
    let alts =
      List.map constructor_declarations ~f:(fun constructor_declaration ->
        match Constructor_declaration.create constructor_declaration with
        | Record { allow_extra_fields; fields; label } ->
          let fields =
            record_type_of_label_declarations env type_name fields ~allow_extra_fields
          in
          label, [ Fields fields ]
        | Tuple_regular (label, ctypes) ->
          let f ctype = One (type_of_core_type env type_name ctype) in
          label, List.map ctypes ~f
        | Tuple_sexp_list (label, ctype) ->
          label, [ Many (type_of_core_type env type_name ctype) ])
    in
    Variant { ignore_capitalization = true; alts }
  ;;

  let type_of_type_declaration env td =
    let loc = td.ptype_loc in
    let type_name = td.ptype_name.txt in
    let type_ =
      match td.ptype_kind with
      | Ptype_variant alts -> variant env type_name alts
      | Ptype_record lds ->
        Record
          (record_type_of_label_declarations
             env
             type_name
             lds
             ~allow_extra_fields:
               (Attribute.get Attrs.allow_extra_fields_td td |> Option.is_some))
      | Ptype_open -> not_supported ~loc "open types"
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> unsupported_builtin ~loc "empty"
         | Some core_type -> type_of_core_type env type_name core_type)
    in
    Env.with_tyvar_parameterize ~loc env td.ptype_name.txt type_
  ;;

  let create ~loc ~path rec_flag tds : t =
    let env = Env.create rec_flag tds in
    let grammars =
      List.map tds ~f:(fun { ptype_name; _ } -> ptype_name.txt, Ref_same_group ptype_name)
      |> Map.of_alist_exn (module Type_name)
    in
    let types =
      List.map tds ~f:(fun td -> td.ptype_name.txt, type_of_type_declaration env td)
    in
    let generic_group = make_generic_group ~tycon_names:(Env.tycon_names env) ~types in
    let group = { instantiate_tycons = Env.instantiate_tycons env } in
    { grammars; generic_group; group; loc; module_path = path }
  ;;

  let collect_type_variables_of_polymorphic_grammar core_type =
    match core_type with
    | [%type: < for_all : [%t? { ptyp_desc = Ptyp_poly (variables, core_type); _ }] > ] ->
      let var_names = List.map variables ~f:(fun { txt; loc = _ } -> txt) in
      var_names, core_type
    | { ptyp_desc = Ptyp_object _; _ } ->
      not_supported
        ~loc:core_type.ptyp_loc
        "objects, except the syntax [%sexp_grammar: < for_all : 'a 'b . ... >] to \
         generate grammars of polymorphic types"
    | _ -> [], core_type
  ;;

  let singleton ~loc ~path core_type : t =
    let name = { loc; txt = "dummy_type_name_from_sexp_grammar" } in
    let params, core_type =
      let type_variables, core_type =
        collect_type_variables_of_polymorphic_grammar core_type
      in
      ( List.map type_variables ~f:(fun var_name -> ptyp_var ~loc var_name, Invariant)
      , core_type )
    in
    let td =
      type_declaration
        ~loc
        ~name
        ~params
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some core_type)
    in
    let env = Env.create Recursive [ td ] in
    let types = [ name.txt, type_of_type_declaration env td ] in
    { grammars = Map.singleton (module Type_name) name.txt (Ref_same_group name)
    ; generic_group = make_generic_group ~tycon_names:(Env.tycon_names env) ~types
    ; group = { instantiate_tycons = Env.instantiate_tycons env }
    ; loc
    ; module_path = path
    }
  ;;

  module Pattern = struct
    let the_generic_group ~loc = [%pat? _the_generic_group]
    let the_group ~loc = [%pat? _the_group]
  end

  module Expression = struct
    let the_generic_group ~loc = [%expr _the_generic_group]
    let the_group ~loc = [%expr _the_group]
    let list ~loc xs ~f = elist ~loc (List.map xs ~f:(f ~loc))
    let tuple2 a b ~loc (a_, b_) = [%expr [%e a ~loc a_], [%e b ~loc b_]]

    let map_lident_last lident ~f =
      match lident with
      | Lident x -> Lident (f x)
      | Ldot (lident, x) -> Ldot (lident, f x)
      | Lapply _ -> invalid_arg "Lapply"
    ;;

    let of_grammar ~loc =
      let type_lifter = Sexp_grammar_lifter.lifter ~loc in
      let rec of_grammar = function
        | Inline { loc; txt = type_ } ->
          [%expr Inline [%e type_lifter#type_ of_grammar type_]]
        | Ref_same_group { loc; txt = type_name } ->
          [%expr Ref ([%e type_lifter#type_name type_name], [%e the_group ~loc])]
        | Ref_other_group { loc; txt = lid } ->
          pexp_ident
            ~loc
            { loc
            ; txt =
                map_lident_last lid ~f:(fun type_name -> type_name ^ sexp_grammar_suffix)
            }
      in
      of_grammar
    ;;

    let of_type ~loc = (Sexp_grammar_lifter.lifter ~loc)#type_ (of_grammar ~loc)

    let of_generic_group ~loc { tycon_names; ggid; types } =
      [%expr
        { tycon_names = [%e list ~loc tycon_names ~f:Sexp_grammar_lifter.lift_var_name]
        ; ggid = [%e estring ~loc ggid]
        ; types =
            [%e list ~loc types ~f:(tuple2 Sexp_grammar_lifter.lift_type_name of_type)]
        }]
    ;;

    let of_group ~loc { instantiate_tycons } ~module_path =
      [%expr
        { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
        ; instantiate_tycons = [%e list ~loc instantiate_tycons ~f:of_grammar]
        ; generic_group = [%e the_generic_group ~loc]
        ; origin = [%e estring ~loc module_path]
        }]
    ;;
  end

  let sexp_grammar_name ~loc type_name =
    { loc; txt = Lident (type_name ^ sexp_grammar_suffix) }
  ;;

  let to_pat_and_expr { grammars; generic_group; group; loc; module_path } =
    let bindings =
      Map.to_alist grammars
      |> List.map ~f:(fun (type_name, grammar) ->
        let pat =
          [%pat?
                 ([%p ppat_var ~loc { loc; txt = type_name ^ sexp_grammar_suffix }] :
                    Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t)]
        in
        let expr = Expression.of_grammar ~loc grammar in
        pat, expr)
    in
    let pat = ppat_tuple (List.map bindings ~f:fst) ~loc in
    let generic_group = Expression.of_generic_group ~loc generic_group in
    let group = Expression.of_group ~loc group ~module_path in
    let grammars =
      pexp_let
        Nonrecursive
        (List.map bindings ~f:(fun (pat, expr) -> value_binding ~loc ~pat ~expr))
        (pexp_tuple
           ~loc
           (Map.to_alist grammars
            |> List.map ~f:(fun (type_name, _) ->
              pexp_ident ~loc (sexp_grammar_name ~loc type_name))))
        ~loc
    in
    let expr =
      [%expr
        let ([%p Pattern.the_generic_group ~loc]
             : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group)
          =
          [%e generic_group]
        in
        let ([%p Pattern.the_group ~loc]
             : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group)
          =
          [%e group]
        in
        [%e grammars]]
    in
    pat, expr
  ;;

  let grammar_of_tds ~loc ~path (rec_flag, tds) =
    let pat, expr = create ~loc ~path rec_flag tds |> to_pat_and_expr in
    [%str let [%p pat] = [%e expr]]
  ;;

  let sexp_grammar ~loc ~path core_type =
    singleton ~loc ~path core_type |> to_pat_and_expr |> snd
  ;;
end

(* Generates the signature for generation of sexp grammar *)
module Sig_generate_sexp_grammar = struct
  let type_of_sexp_grammar ~loc _ = [%type: Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t]

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (fun n -> n ^ "_sexp_grammar") td.ptype_name)
           ~type_:(type_of_sexp_grammar ~loc td)
           ~prim:[]))
  ;;
end

let type_extension ty = Sig_generate_sexp_grammar.type_of_sexp_grammar ty ~loc:ty.ptyp_loc

let core_type ~loc ~path ty =
  Merlin_helpers.hide_expression (Str_generate_sexp_grammar.sexp_grammar ~loc ~path ty)
;;

let sig_type_decl = Sig_generate_sexp_grammar.mk_sig
let str_type_decl = Str_generate_sexp_grammar.grammar_of_tds
