open! Base

module Abstract = struct
  type t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Abstract.t" in
     fun t -> Sexplib0.Sexp_conv_error.empty_type _tp_loc t
              : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp
  let sexp_of_t = (fun v -> (fun _v -> assert false) v : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Tuple = struct
  type t = int * int * int [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Tuple.t" in
     function
     | Sexplib0.Sexp.List [ v0; v1; v2 ] ->
       let v0 = int_of_sexp v0
       and v1 = int_of_sexp v1
       and v2 = int_of_sexp v2 in
       v0, v1, v2
     | sexp -> Sexplib0.Sexp_conv_error.tuple_of_size_n_expected _tp_loc 3 sexp
               : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (v0, v1, v2) ->
       let v0 = sexp_of_int v0
       and v1 = sexp_of_int v1
       and v2 = sexp_of_int v2 in
       Sexplib0.Sexp.List [ v0; v1; v2 ]
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Record.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !c_field with
              | ( Stdlib.Option.Some a_value
                , Stdlib.Option.Some b_value
                , Stdlib.Option.Some c_value ) -> { a = a_value; b = b_value; c = c_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) !c_field Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c } ->
       let bnds = [] in
       let bnds =
         let arg = sexp_of_int v_c in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] :: bnds
       in
       let bnds =
         let arg = sexp_of_int v_b in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
       in
       let bnds =
         let arg = sexp_of_int v_a in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Mutable_record = struct
  type t =
    { mutable a : int
    ; mutable b : int
    ; mutable c : int
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Mutable_record.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !c_field with
              | ( Stdlib.Option.Some a_value
                , Stdlib.Option.Some b_value
                , Stdlib.Option.Some c_value ) -> { a = a_value; b = b_value; c = c_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) !c_field Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c } ->
       let bnds = [] in
       let bnds =
         let arg = sexp_of_int v_c in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] :: bnds
       in
       let bnds =
         let arg = sexp_of_int v_b in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
       in
       let bnds =
         let arg = sexp_of_int v_a in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant = struct
  type t =
    | A
    | B of int * int
    | C of
        { a : int
        ; b : int
        ; d : int
        }
    | D of
        { mutable a : int
        ; mutable b : int
        ; mutable t : int
        }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Variant.t" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("b" | "B") as _tag) :: sexp_args) as _sexp
       ->
       (match sexp_args with
        | [ v0; v1 ] ->
          let v0 = int_of_sexp v0
          and v1 = int_of_sexp v1 in
          B (v0, v1)
        | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("c" | "C") as _tag) :: field_sexps) as
       sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and d_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "d" ->
              (match !d_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 d_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !d_field with
              | ( Stdlib.Option.Some a_value
                , Stdlib.Option.Some b_value
                , Stdlib.Option.Some d_value ) -> C { a = a_value; b = b_value; d = d_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) !d_field Stdlib.Option.None, "d"
                  ])))
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("d" | "D") as _tag) :: field_sexps) as
       sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and t_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "t" ->
              (match !t_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 t_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !t_field with
              | ( Stdlib.Option.Some a_value
                , Stdlib.Option.Some b_value
                , Stdlib.Option.Some t_value ) -> D { a = a_value; b = b_value; t = t_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) !t_field Stdlib.Option.None, "t"
                  ])))
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp ->
       Sexplib0.Sexp_conv_error.stag_no_args _tp_loc sexp
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.Atom ("d" | "D") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag _tp_loc sexp
               : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | A -> Sexplib0.Sexp.Atom "A"
      | B (v0, v1) ->
        let v0 = sexp_of_int v0
        and v1 = sexp_of_int v1 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; v0; v1 ]
      | C { a = v_a; b = v_b; d = v_d } ->
        let bnds = [] in
        let bnds =
          let arg = sexp_of_int v_d in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_b in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_a in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds)
      | D { a = v_a; b = v_b; t = v_t } ->
        let bnds = [] in
        let bnds =
          let arg = sexp_of_int v_t in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_b in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_int v_a in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds)
        : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant = struct
  type t =
    [ `A
    | `B of int
    ]
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let _tp_loc = "expansion.ml.Poly_variant.t" in
     function
     | Sexplib0.Sexp.Atom atom as _sexp ->
       (match atom with
        | "A" -> `A
        | "B" -> Sexplib0.Sexp_conv_error.ptag_takes_args _tp_loc _sexp
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom :: sexp_args) as _sexp ->
       (match atom with
        | "B" as _tag ->
          (match sexp_args with
           | [ v0 ] ->
             let v0 = int_of_sexp v0 in
             `B v0
           | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
        | "A" -> Sexplib0.Sexp_conv_error.ptag_no_args _tp_loc _sexp
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Poly_variant.t" in
     fun sexp ->
       try __t_of_sexp__ sexp with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found _tp_loc sexp
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | `A -> Sexplib0.Sexp.Atom "A"
      | `B v0 -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int v0 ]
                 : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Inline_poly_variant = struct
  type t =
    [ Poly_variant.t
    | `C of int * int
    ]
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let _tp_loc = "expansion.ml.Inline_poly_variant.t" in
     fun sexp ->
       try (Poly_variant.__t_of_sexp__ sexp :> t) with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         (match sexp with
          | Sexplib0.Sexp.Atom atom as _sexp ->
            (match atom with
             | "C" -> Sexplib0.Sexp_conv_error.ptag_takes_args _tp_loc _sexp
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom :: sexp_args) as _sexp ->
            (match atom with
             | "C" as _tag ->
               (match sexp_args with
                | [ v0 ] ->
                  let v0 =
                    match v0 with
                    | Sexplib0.Sexp.List [ v0; v1 ] ->
                      let v0 = int_of_sexp v0
                      and v1 = int_of_sexp v1 in
                      v0, v1
                    | sexp ->
                      Sexplib0.Sexp_conv_error.tuple_of_size_n_expected _tp_loc 2 sexp
                  in
                  `C v0
                | _ -> Sexplib0.Sexp_conv_error.ptag_incorrect_n_args _tp_loc _tag _sexp)
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var _tp_loc sexp
          | Sexplib0.Sexp.List [] as sexp ->
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var _tp_loc sexp)
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Inline_poly_variant.t" in
     fun sexp ->
       try __t_of_sexp__ sexp with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found _tp_loc sexp
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
      | #Poly_variant.t as v -> Poly_variant.sexp_of_t v
      | `C v0 ->
        Sexplib0.Sexp.List
          [ Sexplib0.Sexp.Atom "C"
          ; (let v0, v1 = v0 in
             let v0 = sexp_of_int v0
             and v1 = sexp_of_int v1 in
             Sexplib0.Sexp.List [ v0; v1 ])
          ]
        : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Recursive = struct
  type t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let _tp_loc = "expansion.ml.Recursive.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag) :: sexp_args) as _sexp ->
       (match sexp_args with
        | [ v0 ] ->
          let v0 = t_of_sexp v0 in
          Banana v0
        | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp ->
       Sexplib0.Sexp_conv_error.stag_no_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag _tp_loc sexp
               : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
      | Banana v0 ->
        let v0 = sexp_of_t v0 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; v0 ]
      | Orange -> Sexplib0.Sexp.Atom "Orange"
                  : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Nonrecursive = struct
  open Recursive

  type nonrec t = t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (t_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (sexp_of_t : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Mutually_recursive = struct
  type a =
    | A
    | B of b
    | C of
        { a : a
        ; b : b
        ; c : c
        }

  and b =
    { a : a
    ; b : b
    }

  and c = int [@@deriving_inline sexp]

  let _ = fun (_ : a) -> ()
  let _ = fun (_ : b) -> ()
  let _ = fun (_ : c) -> ()

  let rec a_of_sexp =
    (let _tp_loc = "expansion.ml.Mutually_recursive.a" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("b" | "B") as _tag) :: sexp_args) as _sexp
       ->
       (match sexp_args with
        | [ v0 ] ->
          let v0 = b_of_sexp v0 in
          B v0
        | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("c" | "C") as _tag) :: field_sexps) as
       sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = a_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = b_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = c_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !c_field with
              | ( Stdlib.Option.Some a_value
                , Stdlib.Option.Some b_value
                , Stdlib.Option.Some c_value ) -> C { a = a_value; b = b_value; c = c_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ; Sexplib0.Sexp_conv.( = ) !c_field Stdlib.Option.None, "c"
                  ])))
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp ->
       Sexplib0.Sexp_conv_error.stag_no_args _tp_loc sexp
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag _tp_loc sexp
               : Sexplib0.Sexp.t -> a)

  and b_of_sexp =
    (let _tp_loc = "expansion.ml.Mutually_recursive.b" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = a_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = b_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field with
              | Stdlib.Option.Some a_value, Stdlib.Option.Some b_value ->
                { a = a_value; b = b_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                  ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                  ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> b)

  and c_of_sexp =
    (let _tp_loc = "expansion.ml.Mutually_recursive.c" in
     fun t -> int_of_sexp t
              : Sexplib0.Sexp.t -> c)
  ;;

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
      | A -> Sexplib0.Sexp.Atom "A"
      | B v0 ->
        let v0 = sexp_of_b v0 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; v0 ]
      | C { a = v_a; b = v_b; c = v_c } ->
        let bnds = [] in
        let bnds =
          let arg = sexp_of_c v_c in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_b v_b in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_a v_a in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds)
        : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = v_a; b = v_b } ->
       let bnds = [] in
       let bnds =
         let arg = sexp_of_b v_b in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
       in
       let bnds =
         let arg = sexp_of_a v_a in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (fun v -> sexp_of_int v : c -> Sexplib0.Sexp.t)

  let _ = sexp_of_a
  and _ = sexp_of_b
  and _ = sexp_of_c

  [@@@end]
end

module Alias = struct
  type t = Recursive.t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (Recursive.t_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (Recursive.sexp_of_t : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Re_export = struct
  type t = Recursive.t =
    | Banana of t
    | Orange
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let _tp_loc = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag) :: sexp_args) as _sexp ->
       (match sexp_args with
        | [ v0 ] ->
          let v0 = t_of_sexp v0 in
          Banana v0
        | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp ->
       Sexplib0.Sexp_conv_error.stag_no_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag _tp_loc sexp
               : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
      | Banana v0 ->
        let v0 = sexp_of_t v0 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; v0 ]
      | Orange -> Sexplib0.Sexp.Atom "Orange"
                  : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Unary = struct
  type 'a t = 'a list option [@@deriving_inline sexp]

  let _ = fun (_ : 'a t) -> ()

  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
    let _tp_loc = "expansion.ml.Unary.t" in
    fun _of_a t -> option_of_sexp (list_of_sexp _of_a) t
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a v -> sexp_of_option (sexp_of_list _of_a) v
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Binary = struct
  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
    'a 'b. (Sexplib0.Sexp.t -> 'a) -> (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t
    -> ('a, 'b) t
    =
    Either.t_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
    'a 'b. ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t
    -> Sexplib0.Sexp.t
    =
    Either.sexp_of_t
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module First_order = struct
  type 'a t = 'a -> 'a [@@deriving_inline sexp]

  let _ = fun (_ : 'a t) -> ()

  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
    let _tp_loc = "expansion.ml.First_order.t" in
    fun _of_a t -> Sexplib0.Sexp_conv.fun_of_sexp t
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a v ->
    (fun _f ->
       let open Sexplib0.Sexp_conv in
       sexp_of_fun ignore)
      v
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Second_order = struct
  type ('a, 'b) t = ('a -> 'a) -> ('a -> 'b) -> ('b -> 'b) -> 'a -> 'b
  [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
    'a 'b. (Sexplib0.Sexp.t -> 'a) -> (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t
    -> ('a, 'b) t
    =
    let _tp_loc = "expansion.ml.Second_order.t" in
    fun _of_a _of_b t -> Sexplib0.Sexp_conv.fun_of_sexp t
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
    'a 'b. ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t
    -> Sexplib0.Sexp.t
    =
    fun _of_a _of_b v ->
    (fun _f ->
       let open Sexplib0.Sexp_conv in
       sexp_of_fun ignore)
      v
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Named_arguments = struct
  type t = ?a:int -> b:int -> int -> int [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (Sexplib0.Sexp_conv.fun_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp

  let sexp_of_t =
    (fun v ->
       (fun _f ->
          let open Sexplib0.Sexp_conv in
          sexp_of_fun ignore)
         v
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Gadt = struct
  type _ t =
    | A : _ option t
    | B : int -> int t
    | C : 'a list -> unit t
  [@@deriving_inline sexp_of]

  let _ = fun (_ : _ t) -> ()

  let sexp_of_t
    : type v_x__001_. (v_x__001_ -> Sexplib0.Sexp.t) -> v_x__001_ t -> Sexplib0.Sexp.t
    =
    fun _of_v_x__001_ -> function
      | A -> Sexplib0.Sexp.Atom "A"
      | B v0 ->
        let v0 = sexp_of_int v0 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; v0 ]
      | C v0 ->
        let v0 = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") v0 in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; v0 ]
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Recursive_record_containing_variant = struct
  type t =
    { a : [ `A of t ]
    ; b : [ `B ] [@sexp_drop_default Poly.equal] [@default `B]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let rec t_of_sexp =
    (let _tp_loc = "expansion.ml.Recursive_record_containing_variant.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue =
                   (fun sexp ->
                      try
                        match sexp with
                        | Sexplib0.Sexp.Atom atom as _sexp ->
                          (match atom with
                           | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args _tp_loc _sexp
                           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                        | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom :: sexp_args) as _sexp
                          ->
                          (match atom with
                           | "A" as _tag ->
                             (match sexp_args with
                              | [ v0 ] ->
                                let v0 = t_of_sexp v0 in
                                `A v0
                              | _ ->
                                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                  _tp_loc
                                  _tag
                                  _sexp)
                           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
                          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var _tp_loc sexp
                        | Sexplib0.Sexp.List [] as sexp ->
                          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var _tp_loc sexp
                      with
                      | Sexplib0.Sexp_conv_error.No_variant_match ->
                        Sexplib0.Sexp_conv_error.no_matching_variant_found _tp_loc sexp)
                     _field_sexp
                 in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue =
                   (fun sexp ->
                      try
                        match sexp with
                        | Sexplib0.Sexp.Atom atom as _sexp ->
                          (match atom with
                           | "B" -> `B
                           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                        | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom :: _) as _sexp ->
                          (match atom with
                           | "B" -> Sexplib0.Sexp_conv_error.ptag_no_args _tp_loc _sexp
                           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
                          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var _tp_loc sexp
                        | Sexplib0.Sexp.List [] as sexp ->
                          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var _tp_loc sexp
                      with
                      | Sexplib0.Sexp_conv_error.No_variant_match ->
                        Sexplib0.Sexp_conv_error.no_matching_variant_found _tp_loc sexp)
                     _field_sexp
                 in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field with
              | Stdlib.Option.Some a_value, b_value ->
                { a = a_value
                ; b =
                    (match b_value with
                     | Stdlib.Option.None -> `B
                     | Stdlib.Option.Some v -> v)
                }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a" ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (fun { a = v_a; b = v_b } ->
       let bnds = [] in
       let bnds =
         if Poly.equal `B v_b
         then bnds
         else (
           let arg = (fun `B -> Sexplib0.Sexp.Atom "B") v_b in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         let arg =
           let (`A v0) = v_a in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v0 ]
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_record = struct
  type t =
    { a : 'a. 'a list
    ; b : 'b. 'b option
    ; c : 'c. 'c
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Poly_record.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a, b, c =
         let a_field = ref Stdlib.Option.None
         and b_field = ref Stdlib.Option.None
         and c_field = ref Stdlib.Option.None
         and duplicates = ref []
         and extra = ref [] in
         let rec iter = function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
             :: tail ->
             let _field_sexp () =
               match _field_sexps with
               | [ x ] -> x
               | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
               | _ -> assert false
             in
             (match field_name with
              | "a" ->
                (match !a_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue =
                     (let _of_a sexp =
                        Sexplib0.Sexp_conv_error.record_poly_field_value _tp_loc sexp
                      in
                      list_of_sexp _of_a)
                       _field_sexp
                   in
                   a_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "b" ->
                (match !b_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue =
                     (let _of_b sexp =
                        Sexplib0.Sexp_conv_error.record_poly_field_value _tp_loc sexp
                      in
                      option_of_sexp _of_b)
                       _field_sexp
                   in
                   b_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | "c" ->
                (match !c_field with
                 | Stdlib.Option.None ->
                   let _field_sexp = _field_sexp () in
                   let fvalue =
                     (let _of_c sexp =
                        Sexplib0.Sexp_conv_error.record_poly_field_value _tp_loc sexp
                      in
                      _of_c)
                       _field_sexp
                   in
                   c_field := Stdlib.Option.Some fvalue
                 | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
              | _ ->
                if !Sexplib0.Sexp_conv.record_check_extra_fields
                then extra := field_name :: !extra
                else ());
             iter tail
           | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
             Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
           | [] -> ()
         in
         iter field_sexps;
         match !duplicates with
         | _ :: _ ->
           Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
         | [] ->
           (match !extra with
            | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
            | [] ->
              (match !a_field, !b_field, !c_field with
               | ( Stdlib.Option.Some a_value
                 , Stdlib.Option.Some b_value
                 , Stdlib.Option.Some c_value ) -> a_value, b_value, c_value
               | _ ->
                 Sexplib0.Sexp_conv_error.record_undefined_elements
                   _tp_loc
                   sexp
                   [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a"
                   ; Sexplib0.Sexp_conv.( = ) !b_field Stdlib.Option.None, "b"
                   ; Sexplib0.Sexp_conv.( = ) !c_field Stdlib.Option.None, "c"
                   ]))
       in
       { a; b; c }
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c } ->
       let bnds = [] in
       let bnds =
         let arg =
           (let _of_c = Sexplib0.Sexp_conv.sexp_of_opaque in
            _of_c)
             v_c
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] :: bnds
       in
       let bnds =
         let arg =
           (let _of_b = Sexplib0.Sexp_conv.sexp_of_opaque in
            sexp_of_option _of_b)
             v_b
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
       in
       let bnds =
         let arg =
           (let _of_a = Sexplib0.Sexp_conv.sexp_of_opaque in
            sexp_of_list _of_a)
             v_a
         in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_defaults = struct
  type t =
    { a : int [@default 0]
    ; b : int [@default 0] [@sexp_drop_default.compare]
    ; c : int [@default 0] [@sexp_drop_default.equal]
    ; d : int [@default 0] [@sexp_drop_default.sexp]
    ; e : int [@default 0] [@sexp_drop_default ( = )]
    ; f : int [@sexp_drop_if ( = ) 0]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Record_with_defaults.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and d_field = ref Stdlib.Option.None
       and e_field = ref Stdlib.Option.None
       and f_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "d" ->
              (match !d_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 d_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "e" ->
              (match !e_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 e_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "f" ->
              (match !f_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 f_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field, !b_field, !c_field, !d_field, !e_field, !f_field with
              | a_value, b_value, c_value, d_value, e_value, Stdlib.Option.Some f_value ->
                { a =
                    (match a_value with
                     | Stdlib.Option.None -> 0
                     | Stdlib.Option.Some v -> v)
                ; b =
                    (match b_value with
                     | Stdlib.Option.None -> 0
                     | Stdlib.Option.Some v -> v)
                ; c =
                    (match c_value with
                     | Stdlib.Option.None -> 0
                     | Stdlib.Option.Some v -> v)
                ; d =
                    (match d_value with
                     | Stdlib.Option.None -> 0
                     | Stdlib.Option.Some v -> v)
                ; e =
                    (match e_value with
                     | Stdlib.Option.None -> 0
                     | Stdlib.Option.Some v -> v)
                ; f = f_value
                }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !f_field Stdlib.Option.None, "f" ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c; d = v_d; e = v_e; f = v_f } ->
       let bnds = [] in
       let bnds =
         if (( = ) 0) v_f
         then bnds
         else (
           let arg = sexp_of_int v_f in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         if 0 = v_e
         then bnds
         else (
           let arg = sexp_of_int v_e in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         let arg = sexp_of_int v_d in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int 0) arg
         then bnds
         else (
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         if [%equal: int] 0 v_c
         then bnds
         else (
           let arg = sexp_of_int v_c in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         if [%compare.equal: int] 0 v_b
         then bnds
         else (
           let arg = sexp_of_int v_b in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         let arg = sexp_of_int v_a in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_special_types = struct
  type t =
    { a : int option [@sexp.option]
    ; b : int list [@sexp.list]
    ; c : int array [@sexp.array]
    ; d : bool [@sexp.bool]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Record_with_special_types.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and d_field = ref false
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = list_of_sexp int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = array_of_sexp int_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "d" ->
              if !d_field
              then duplicates := field_name :: !duplicates
              else (
                match _field_sexps with
                | [] -> d_field := true
                | _ :: _ ->
                  Sexplib0.Sexp_conv_error.record_sexp_bool_with_payload _tp_loc sexp)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             let a_value, b_value, c_value, d_value =
               !a_field, !b_field, !c_field, !d_field
             in
             { a = a_value
             ; b =
                 (match b_value with
                  | Stdlib.Option.None -> []
                  | Stdlib.Option.Some v -> v)
             ; c =
                 (match c_value with
                  | Stdlib.Option.None -> [||]
                  | Stdlib.Option.Some v -> v)
             ; d = d_value
             }))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c; d = v_d } ->
       let bnds = [] in
       let bnds =
         if v_d
         then (
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           bnd :: bnds)
         else bnds
       in
       let bnds =
         if
           match v_c with
           | [||] -> true
           | _ -> false
         then bnds
         else (
           let arg = (sexp_of_array sexp_of_int) v_c in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         if
           match v_b with
           | [] -> true
           | _ -> false
         then bnds
         else (
           let arg = (sexp_of_list sexp_of_int) v_b in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] in
           bnd :: bnds)
       in
       let bnds =
         match v_a with
         | Stdlib.Option.None -> bnds
         | Stdlib.Option.Some v ->
           let arg = sexp_of_int v in
           let bnd = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] in
           bnd :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_omit_nil = struct
  type t =
    { a : int option [@sexp.omit_nil]
    ; b : int list [@sexp.omit_nil]
    ; c : unit [@sexp.omit_nil]
    ; d : int [@sexp.omit_nil]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Record_with_omit_nil.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and b_field = ref Stdlib.Option.None
       and c_field = ref Stdlib.Option.None
       and d_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = option_of_sexp int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "b" ->
              (match !b_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = list_of_sexp int_of_sexp _field_sexp in
                 b_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "c" ->
              (match !c_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = unit_of_sexp _field_sexp in
                 c_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | "d" ->
              (match !d_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 d_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ ->
              if !Sexplib0.Sexp_conv.record_check_extra_fields
              then extra := field_name :: !extra
              else ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             let a_value, b_value, c_value, d_value =
               !a_field, !b_field, !c_field, !d_field
             in
             { a =
                 (match a_value with
                  | Stdlib.Option.Some v -> v
                  | Stdlib.Option.None ->
                    (try option_of_sexp int_of_sexp (Sexplib0.Sexp.List []) with
                     | Sexplib0.Sexp_conv_error.Of_sexp_error (e, _sexp) ->
                       raise (Sexplib0.Sexp_conv_error.Of_sexp_error (e, sexp))))
             ; b =
                 (match b_value with
                  | Stdlib.Option.Some v -> v
                  | Stdlib.Option.None ->
                    (try list_of_sexp int_of_sexp (Sexplib0.Sexp.List []) with
                     | Sexplib0.Sexp_conv_error.Of_sexp_error (e, _sexp) ->
                       raise (Sexplib0.Sexp_conv_error.Of_sexp_error (e, sexp))))
             ; c =
                 (match c_value with
                  | Stdlib.Option.Some v -> v
                  | Stdlib.Option.None ->
                    (try unit_of_sexp (Sexplib0.Sexp.List []) with
                     | Sexplib0.Sexp_conv_error.Of_sexp_error (e, _sexp) ->
                       raise (Sexplib0.Sexp_conv_error.Of_sexp_error (e, sexp))))
             ; d =
                 (match d_value with
                  | Stdlib.Option.Some v -> v
                  | Stdlib.Option.None ->
                    (try int_of_sexp (Sexplib0.Sexp.List []) with
                     | Sexplib0.Sexp_conv_error.Of_sexp_error (e, _sexp) ->
                       raise (Sexplib0.Sexp_conv_error.Of_sexp_error (e, sexp))))
             }))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a; b = v_b; c = v_c; d = v_d } ->
       let bnds = [] in
       let bnds =
         match sexp_of_int v_d with
         | Sexplib0.Sexp.List [] -> bnds
         | arg -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg ] :: bnds
       in
       let bnds =
         match sexp_of_unit v_c with
         | Sexplib0.Sexp.List [] -> bnds
         | arg -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg ] :: bnds
       in
       let bnds =
         match sexp_of_list sexp_of_int v_b with
         | Sexplib0.Sexp.List [] -> bnds
         | arg -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg ] :: bnds
       in
       let bnds =
         match sexp_of_option sexp_of_int v_a with
         | Sexplib0.Sexp.List [] -> bnds
         | arg -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("a" | "A") as _tag) :: sexp_args) as _sexp
       -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp ->
       Sexplib0.Sexp_conv_error.stag_takes_args _tp_loc sexp
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag _tp_loc sexp
               : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let _tp_loc = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom as _sexp ->
       (match atom with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args _tp_loc _sexp
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom :: sexp_args) as _sexp ->
       (match atom with
        | "A" as _tag -> `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var _tp_loc sexp
     | Sexplib0.Sexp.List [] as sexp ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp ->
       try __t_of_sexp__ sexp with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found _tp_loc sexp
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Record_allowing_extra_fields.t" in
     function
     | Sexplib0.Sexp.List field_sexps as sexp ->
       let a_field = ref Stdlib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom field_name :: (([] | [ _ ]) as _field_sexps))
           :: tail ->
           let _field_sexp () =
             match _field_sexps with
             | [ x ] -> x
             | [] -> Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
             | _ -> assert false
           in
           (match field_name with
            | "a" ->
              (match !a_field with
               | Stdlib.Option.None ->
                 let _field_sexp = _field_sexp () in
                 let fvalue = int_of_sexp _field_sexp in
                 a_field := Stdlib.Option.Some fvalue
               | Stdlib.Option.Some _ -> duplicates := field_name :: !duplicates)
            | _ -> ());
           iter tail
         | ((Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _) as sexp) :: _ ->
           Sexplib0.Sexp_conv_error.record_only_pairs_expected _tp_loc sexp
         | [] -> ()
       in
       iter field_sexps;
       (match !duplicates with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields _tp_loc !duplicates sexp
        | [] ->
          (match !extra with
           | _ :: _ -> Sexplib0.Sexp_conv_error.record_extra_fields _tp_loc !extra sexp
           | [] ->
             (match !a_field with
              | Stdlib.Option.Some a_value -> { a = a_value }
              | _ ->
                Sexplib0.Sexp_conv_error.record_undefined_elements
                  _tp_loc
                  sexp
                  [ Sexplib0.Sexp_conv.( = ) !a_field Stdlib.Option.None, "a" ])))
     | Sexplib0.Sexp.Atom _ as sexp ->
       Sexplib0.Sexp_conv_error.record_list_instead_atom _tp_loc sexp
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = v_a } ->
       let bnds = [] in
       let bnds =
         let arg = sexp_of_int v_a in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg ] :: bnds
       in
       Sexplib0.Sexp.List bnds
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let _tp_loc = "expansion.ml.Opaque.t" in
     fun t -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp t
              : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun v -> sexp_of_list Sexplib0.Sexp_conv.sexp_of_opaque v : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end
