open! Stdppx
open! Ppxlib
open Attrs
open Ast_builder.Default

module Generic = struct
  type nullary = { must_type_check : unit Lifted.t }

  type unary =
    { param : core_type
    ; must_type_check : unit Lifted.t
    }

  type 'specific t =
    | Omit_nil
    | Sexp_array of unary
    | Sexp_bool of nullary
    | Sexp_list of unary
    | Sexp_option of unary
    | Sexp_or_null of unary
    | Specific of 'specific
end

open Generic

let get_attribute attr ld ~f =
  Option.map (Attribute.get attr ld) ~f:(fun x -> f x, Attribute.name attr)
;;

let must_type_check ~loc ~name ~actual ~expect type_description =
  let actual = Helpers.strip_attributes#core_type actual in
  let expect = Helpers.strip_attributes#core_type expect in
  let message =
    Printf.sprintf "ppx_sexp_conv: expected [%s] or equivalent" type_description
  in
  [%expr
    fun ([%p pvar ~loc name] : [%t actual]) ->
      let _ : _ =
        ([%e evar ~loc name] : [%t expect]) [@error_message [%e estring ~loc message]]
      in
      ()]
  |> Lifted.create ~loc ~prefix:"_must_type_check" ~ty:[%type: _]
  |> Lifted.map ~f:(fun (_ : expression) -> ())
;;

let get_unary ld attr ~short_type_name ~make_type f =
  get_attribute attr ~f:(fun () ->
    let loc = ld.pld_type.ptyp_loc in
    match ld.pld_type.ptyp_desc with
    | Ptyp_constr (_, [ param ]) ->
      let must_type_check =
        must_type_check
          ~loc
          ~name:ld.pld_name.txt
          ~actual:ld.pld_type
          ~expect:(make_type ~loc param)
          ("_ " ^ short_type_name)
      in
      f { param; must_type_check }
    | _ ->
      Location.raise_errorf
        ~loc
        "ppx_sexp_conv: [@%s] is only allowed on [_ %s] or equivalent."
        (Attribute.name attr)
        short_type_name)
;;

let create ~loc specific_getters ld ~if_no_attribute =
  let generic_getters =
    [ get_attribute omit_nil ~f:(fun () -> Omit_nil)
    ; get_attribute bool ~f:(fun () ->
        let must_type_check =
          must_type_check
            ~loc
            ~name:ld.pld_name.txt
            ~actual:ld.pld_type
            ~expect:[%type: Stdlib.Bool.t]
            "bool"
        in
        Sexp_bool { must_type_check })
    ; get_unary
        ld
        option
        ~short_type_name:"option"
        ~make_type:(fun ~loc ty -> [%type: [%t ty] Stdlib.Option.t])
        (fun x -> Sexp_option x)
    ; get_unary
        ld
        or_null
        ~short_type_name:"or_null"
        ~make_type:(fun ~loc ty -> [%type: [%t ty] Basement.Or_null_shim.t])
        (fun x -> Sexp_or_null x)
    ; get_unary
        ld
        list
        ~short_type_name:"list"
        ~make_type:(fun ~loc ty -> [%type: [%t ty] Stdlib.List.t])
        (fun x -> Sexp_list x)
    ; get_unary
        ld
        array
        ~short_type_name:"array"
        ~make_type:(fun ~loc ty -> [%type: [%t ty] Stdlib.Array.t])
        (fun x -> Sexp_array x)
    ]
  in
  let wrapped_getters =
    List.map specific_getters ~f:(fun get ld ->
      Option.map (get ld) ~f:(fun (specific, string) -> Specific specific, string))
  in
  let getters = List.concat [ wrapped_getters; generic_getters ] in
  match List.filter_map getters ~f:(fun f -> f ld) with
  | [] -> Specific if_no_attribute
  | [ (v, _) ] -> v
  | _ :: _ :: _ as attributes ->
    Location.raise_errorf
      ~loc
      "The following elements are mutually exclusive: %s"
      (List.map attributes ~f:(fun (_, name) -> Printf.sprintf "[@%s]" name)
       |> String.concat ~sep:" ")
;;

let strip_attributes = Ppx_helpers.strip_attributes

let lift_default ~loc ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  Lifted.create ~loc ~prefix:"default" ~ty expr
;;

let lift_drop_default ~loc ~stackify ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  let ty =
    match stackify with
    | false -> [%type: [%t ty] -> [%t ty] -> Stdlib.Bool.t]
    | true -> [%type: [%t ty] -> [%t ty] -> Stdlib.Bool.t]
  in
  Lifted.create ~loc ~prefix:"drop_default" ~ty expr
;;

let lift_drop_if ~loc ~stackify ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  let ty =
    match stackify with
    | false -> [%type: [%t ty] -> Stdlib.Bool.t]
    | true -> [%type: [%t ty] -> Stdlib.Bool.t]
  in
  Lifted.create ~loc ~prefix:"drop_if" ~ty expr
;;

module Of_sexp = struct
  type t =
    | Default of expression Lifted.t
    | Required

  let create ~loc ld =
    create
      ~loc
      [ get_attribute default ~f:(fun { to_lift = default } ->
          Default (lift_default ~loc ld default))
      ]
      ld
      ~if_no_attribute:Required
  ;;
end

module Sexp_of = struct
  module Drop = struct
    type t =
      | Compare of { local : bool }
      | Equal of { local : bool }
      | Sexp
      | Func of expression Lifted.t
  end

  type t =
    | Drop_default of Drop.t
    | Drop_if of expression Lifted.t
    | Keep

  let create ~loc ~stackify ld =
    create
      ~loc
      [ get_attribute drop_default ~f:(fun { to_lift = e } ->
          Drop_default (Func (lift_drop_default ~loc ~stackify ld e)))
      ; get_attribute drop_default_equal ~f:(fun () ->
          Drop_default (Equal { local = false }))
      ; get_attribute drop_default_compare ~f:(fun () ->
          Drop_default (Compare { local = false }))
      ; get_attribute drop_default_equal_local ~f:(fun () ->
          Drop_default (Equal { local = true }))
      ; get_attribute drop_default_compare_local ~f:(fun () ->
          Drop_default (Compare { local = true }))
      ; get_attribute drop_default_sexp ~f:(fun () -> Drop_default Sexp)
      ; get_attribute drop_if ~f:(fun { to_lift = x } ->
          Drop_if (lift_drop_if ~loc ~stackify ld x))
      ]
      ld
      ~if_no_attribute:Keep
  ;;
end
