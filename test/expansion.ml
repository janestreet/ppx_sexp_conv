open! Base

open struct
  type _shadow_constructors =
    | []
    | ( :: )
    | None
    | Some
end

module Abstract = struct
  type t [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__002_ = "expansion.ml.Abstract.t" in
     fun x__003_ -> Sexplib0.Sexp_conv_error.empty_type error_source__002_ x__003_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp
  let sexp_of_t = (fun _ -> assert false : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]
end

module Tuple = struct
  type t = int * int * int [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__012_ = "expansion.ml.Tuple.t" in
     function
     | Sexplib0.Sexp.List [ arg0__005_; arg1__006_; arg2__007_ ] ->
       let res0__008_ = int_of_sexp arg0__005_
       and res1__009_ = int_of_sexp arg1__006_
       and res2__010_ = int_of_sexp arg2__007_ in
       res0__008_, res1__009_, res2__010_
     | sexp__011_ ->
       Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__012_ 3 sexp__011_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (arg0__013_, arg1__014_, arg2__015_) ->
       let res0__016_ = sexp_of_int arg0__013_
       and res1__017_ = sexp_of_int arg1__014_
       and res2__018_ = sexp_of_int arg2__015_ in
       Sexplib0.Sexp.List [ res0__016_; res1__017_; res2__018_ ]
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
    (let error_source__020_ = "expansion.ml.Record.t" in
     fun x__021_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__020_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv = int_of_sexp
                          ; rest = Empty
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, ()))) : t -> { a; b; c })
         x__021_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__023_; b = b__025_; c = c__027_ } ->
       let bnds__022_ = ([] : _ Stdlib.List.t) in
       let bnds__022_ =
         let arg__028_ = sexp_of_int c__027_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__028_ ] :: bnds__022_
           : _ Stdlib.List.t)
       in
       let bnds__022_ =
         let arg__026_ = sexp_of_int b__025_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__026_ ] :: bnds__022_
           : _ Stdlib.List.t)
       in
       let bnds__022_ =
         let arg__024_ = sexp_of_int a__023_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__024_ ] :: bnds__022_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__022_
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
    (let error_source__030_ = "expansion.ml.Mutable_record.t" in
     fun x__031_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__030_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv = int_of_sexp
                          ; rest = Empty
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, ()))) : t -> { a; b; c })
         x__031_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__033_; b = b__035_; c = c__037_ } ->
       let bnds__032_ = ([] : _ Stdlib.List.t) in
       let bnds__032_ =
         let arg__038_ = sexp_of_int c__037_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__038_ ] :: bnds__032_
           : _ Stdlib.List.t)
       in
       let bnds__032_ =
         let arg__036_ = sexp_of_int b__035_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__036_ ] :: bnds__032_
           : _ Stdlib.List.t)
       in
       let bnds__032_ =
         let arg__034_ = sexp_of_int a__033_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__034_ ] :: bnds__032_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__032_
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
    (let error_source__041_ = "expansion.ml.Variant.t" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__044_) :: sexp_args__045_) as
       _sexp__043_ ->
       (match sexp_args__045_ with
        | [ arg0__046_; arg1__047_ ] ->
          let res0__048_ = int_of_sexp arg0__046_
          and res1__049_ = int_of_sexp arg1__047_ in
          B (res0__048_, res1__049_)
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__041_
            _tag__044_
            _sexp__043_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__051_) as sexp__050_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__050_
         ~caller:error_source__041_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "d"
                          ; kind = Required
                          ; conv = int_of_sexp
                          ; rest = Empty
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "d" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (d, ()))) : t -> C { a; b; d })
         sexps__051_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("d" | "D") :: sexps__053_) as sexp__052_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__052_
         ~caller:error_source__041_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "t"
                          ; kind = Required
                          ; conv = int_of_sexp
                          ; rest = Empty
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "t" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (t, ()))) : t -> D { a; b; t })
         sexps__053_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__042_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__041_ sexp__042_
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp__042_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__041_ sexp__042_
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp__042_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__041_ sexp__042_
     | Sexplib0.Sexp.Atom ("d" | "D") as sexp__042_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__041_ sexp__042_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__040_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__041_ sexp__040_
     | Sexplib0.Sexp.List [] as sexp__040_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__041_ sexp__040_
     | sexp__040_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__041_ sexp__040_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B (arg0__054_, arg1__055_) ->
       let res0__056_ = sexp_of_int arg0__054_
       and res1__057_ = sexp_of_int arg1__055_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__056_; res1__057_ ]
     | C { a = a__059_; b = b__061_; d = d__063_ } ->
       let bnds__058_ = ([] : _ Stdlib.List.t) in
       let bnds__058_ =
         let arg__064_ = sexp_of_int d__063_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__064_ ] :: bnds__058_
           : _ Stdlib.List.t)
       in
       let bnds__058_ =
         let arg__062_ = sexp_of_int b__061_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__062_ ] :: bnds__058_
           : _ Stdlib.List.t)
       in
       let bnds__058_ =
         let arg__060_ = sexp_of_int a__059_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__060_ ] :: bnds__058_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__058_)
     | D { a = a__066_; b = b__068_; t = t__070_ } ->
       let bnds__065_ = ([] : _ Stdlib.List.t) in
       let bnds__065_ =
         let arg__071_ = sexp_of_int t__070_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__071_ ] :: bnds__065_
           : _ Stdlib.List.t)
       in
       let bnds__065_ =
         let arg__069_ = sexp_of_int b__068_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__069_ ] :: bnds__065_
           : _ Stdlib.List.t)
       in
       let bnds__065_ =
         let arg__067_ = sexp_of_int a__066_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__067_ ] :: bnds__065_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__065_)
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
    (let error_source__077_ = "expansion.ml.Poly_variant.t" in
     function
     | Sexplib0.Sexp.Atom atom__073_ as _sexp__075_ ->
       (match atom__073_ with
        | "A" -> `A
        | "B" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__077_ _sexp__075_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__073_ :: sexp_args__076_) as
       _sexp__075_ ->
       (match atom__073_ with
        | "B" as _tag__078_ ->
          (match sexp_args__076_ with
           | [ arg0__079_ ] ->
             let res0__080_ = int_of_sexp arg0__079_ in
             `B res0__080_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__077_
               _tag__078_
               _sexp__075_)
        | "A" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__077_ _sexp__075_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__074_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__077_ sexp__074_
     | Sexplib0.Sexp.List [] as sexp__074_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__077_ sexp__074_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__082_ = "expansion.ml.Poly_variant.t" in
     fun sexp__081_ ->
       try __t_of_sexp__ sexp__081_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__082_ sexp__081_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | `A -> Sexplib0.Sexp.Atom "A"
     | `B v__083_ -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int v__083_ ]
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
    (let error_source__095_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__084_ ->
       try (Poly_variant.__t_of_sexp__ sexp__084_ :> t) with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         (match sexp__084_ with
          | Sexplib0.Sexp.Atom atom__085_ as _sexp__087_ ->
            (match atom__085_ with
             | "C" ->
               Sexplib0.Sexp_conv_error.ptag_takes_args error_source__095_ _sexp__087_
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__085_ :: sexp_args__088_) as
            _sexp__087_ ->
            (match atom__085_ with
             | "C" as _tag__089_ ->
               (match sexp_args__088_ with
                | [ arg0__096_ ] ->
                  let res0__097_ =
                    match arg0__096_ with
                    | Sexplib0.Sexp.List [ arg0__090_; arg1__091_ ] ->
                      let res0__092_ = int_of_sexp arg0__090_
                      and res1__093_ = int_of_sexp arg1__091_ in
                      res0__092_, res1__093_
                    | sexp__094_ ->
                      Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                        error_source__095_
                        2
                        sexp__094_
                  in
                  `C res0__097_
                | _ ->
                  Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                    error_source__095_
                    _tag__089_
                    _sexp__087_)
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__086_ ->
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
              error_source__095_
              sexp__086_
          | Sexplib0.Sexp.List [] as sexp__086_ ->
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
              error_source__095_
              sexp__086_)
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__099_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__098_ ->
       try __t_of_sexp__ sexp__098_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__099_ sexp__098_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | #Poly_variant.t as v__100_ -> Poly_variant.sexp_of_t v__100_
     | `C v__101_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "C"
         ; (let arg0__102_, arg1__103_ = v__101_ in
            let res0__104_ = sexp_of_int arg0__102_
            and res1__105_ = sexp_of_int arg1__103_ in
            Sexplib0.Sexp.List [ res0__104_; res1__105_ ])
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
    (let error_source__108_ = "expansion.ml.Recursive.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__111_) :: sexp_args__112_) as
       _sexp__110_ ->
       (match sexp_args__112_ with
        | [ arg0__113_ ] ->
          let res0__114_ = t_of_sexp arg0__113_ in
          Banana res0__114_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__108_
            _tag__111_
            _sexp__110_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__109_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__108_ sexp__109_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__109_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__108_ sexp__109_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__107_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__108_ sexp__107_
     | Sexplib0.Sexp.List [] as sexp__107_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__108_ sexp__107_
     | sexp__107_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__108_ sexp__107_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__115_ ->
       let res0__116_ = sexp_of_t arg0__115_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__116_ ]
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

  and c = a [@@deriving_inline sexp]

  let _ = fun (_ : a) -> ()
  let _ = fun (_ : b) -> ()
  let _ = fun (_ : c) -> ()

  let rec a_of_sexp =
    (let error_source__120_ = "expansion.ml.Mutually_recursive.a" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__123_) :: sexp_args__124_) as
       _sexp__122_ ->
       (match sexp_args__124_ with
        | [ arg0__125_ ] ->
          let res0__126_ = b_of_sexp arg0__125_ in
          B res0__126_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__120_
            _tag__123_
            _sexp__122_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__128_) as sexp__127_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__127_
         ~caller:error_source__120_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = a_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv = b_of_sexp
                    ; rest =
                        Field
                          { name = "c"; kind = Required; conv = c_of_sexp; rest = Empty }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, ()))) : a -> C { a; b; c })
         sexps__128_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__121_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__120_ sexp__121_
     | Sexplib0.Sexp.Atom ("b" | "B") as sexp__121_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__120_ sexp__121_
     | Sexplib0.Sexp.Atom ("c" | "C") as sexp__121_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__120_ sexp__121_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__119_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__120_ sexp__119_
     | Sexplib0.Sexp.List [] as sexp__119_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__120_ sexp__119_
     | sexp__119_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__120_ sexp__119_
      : Sexplib0.Sexp.t -> a)

  and b_of_sexp =
    (let error_source__130_ = "expansion.ml.Mutually_recursive.b" in
     fun x__131_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__130_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv = a_of_sexp
              ; rest =
                  Field { name = "b"; kind = Required; conv = b_of_sexp; rest = Empty }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : b -> { a; b })
         x__131_
      : Sexplib0.Sexp.t -> b)

  and c_of_sexp = (fun x__133_ -> a_of_sexp x__133_ : Sexplib0.Sexp.t -> c)

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B arg0__134_ ->
       let res0__135_ = sexp_of_b arg0__134_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__135_ ]
     | C { a = a__137_; b = b__139_; c = c__141_ } ->
       let bnds__136_ = ([] : _ Stdlib.List.t) in
       let bnds__136_ =
         let arg__142_ = sexp_of_c c__141_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__142_ ] :: bnds__136_
           : _ Stdlib.List.t)
       in
       let bnds__136_ =
         let arg__140_ = sexp_of_b b__139_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__140_ ] :: bnds__136_
           : _ Stdlib.List.t)
       in
       let bnds__136_ =
         let arg__138_ = sexp_of_a a__137_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__138_ ] :: bnds__136_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__136_)
      : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = a__144_; b = b__146_ } ->
       let bnds__143_ = ([] : _ Stdlib.List.t) in
       let bnds__143_ =
         let arg__147_ = sexp_of_b b__146_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__147_ ] :: bnds__143_
           : _ Stdlib.List.t)
       in
       let bnds__143_ =
         let arg__145_ = sexp_of_a a__144_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__145_ ] :: bnds__143_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__143_
      : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (fun x__148_ -> sexp_of_a x__148_ : c -> Sexplib0.Sexp.t)

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
    (let error_source__152_ = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__155_) :: sexp_args__156_) as
       _sexp__154_ ->
       (match sexp_args__156_ with
        | [ arg0__157_ ] ->
          let res0__158_ = t_of_sexp arg0__157_ in
          Banana res0__158_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__152_
            _tag__155_
            _sexp__154_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__153_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__152_ sexp__153_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__153_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__152_ sexp__153_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__151_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__152_ sexp__151_
     | Sexplib0.Sexp.List [] as sexp__151_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__152_ sexp__151_
     | sexp__151_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__152_ sexp__151_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__159_ ->
       let res0__160_ = sexp_of_t arg0__159_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__160_ ]
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
    fun _of_a__161_ x__163_ -> option_of_sexp (list_of_sexp _of_a__161_) x__163_
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__164_ x__165_ -> sexp_of_option (sexp_of_list _of_a__164_) x__165_
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Binary = struct
  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
        'a 'b.
        (Sexplib0.Sexp.t -> 'a)
        -> (Sexplib0.Sexp.t -> 'b)
        -> Sexplib0.Sexp.t
        -> ('a, 'b) t
    =
    Either.t_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
        'a 'b.
        ('a -> Sexplib0.Sexp.t)
        -> ('b -> Sexplib0.Sexp.t)
        -> ('a, 'b) t
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
    fun _of_a__173_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__175_ _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Second_order = struct
  type ('a, 'b) t = ('a -> 'a) -> ('a -> 'b) -> ('b -> 'b) -> 'a -> 'b
  [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp :
        'a 'b.
        (Sexplib0.Sexp.t -> 'a)
        -> (Sexplib0.Sexp.t -> 'b)
        -> Sexplib0.Sexp.t
        -> ('a, 'b) t
    =
    fun _of_a__176_ _of_b__177_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
        'a 'b.
        ('a -> Sexplib0.Sexp.t)
        -> ('b -> Sexplib0.Sexp.t)
        -> ('a, 'b) t
        -> Sexplib0.Sexp.t
    =
    fun _of_a__179_ _of_b__180_ _ ->
    Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
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
    (fun _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
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

  let sexp_of_t : 'a__182_. ('a__182_ -> Sexplib0.Sexp.t) -> 'a__182_ t -> Sexplib0.Sexp.t
    =
    fun (type a__188_) : ((a__188_ -> Sexplib0.Sexp.t) -> a__188_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__183_ -> function
    | A -> Sexplib0.Sexp.Atom "A"
    | B arg0__184_ ->
      let res0__185_ = sexp_of_int arg0__184_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__185_ ]
    | C arg0__186_ ->
      let res0__187_ = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") arg0__186_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; res0__187_ ]
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
    (let (default__191_ : [ `B ]) = `B in
     let error_source__190_ = "expansion.ml.Recursive_record_containing_variant.t" in
     fun x__207_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__190_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun sexp__206_ ->
                    try
                      match sexp__206_ with
                      | Sexplib0.Sexp.Atom atom__199_ as _sexp__201_ ->
                        (match atom__199_ with
                         | "A" ->
                           Sexplib0.Sexp_conv_error.ptag_takes_args
                             error_source__190_
                             _sexp__201_
                         | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                      | Sexplib0.Sexp.List
                          (Sexplib0.Sexp.Atom atom__199_ :: sexp_args__202_) as
                        _sexp__201_ ->
                        (match atom__199_ with
                         | "A" as _tag__203_ ->
                           (match sexp_args__202_ with
                            | [ arg0__204_ ] ->
                              let res0__205_ = t_of_sexp arg0__204_ in
                              `A res0__205_
                            | _ ->
                              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                error_source__190_
                                _tag__203_
                                _sexp__201_)
                         | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__200_ ->
                        Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                          error_source__190_
                          sexp__200_
                      | Sexplib0.Sexp.List [] as sexp__200_ ->
                        Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                          error_source__190_
                          sexp__200_
                    with
                    | Sexplib0.Sexp_conv_error.No_variant_match ->
                      Sexplib0.Sexp_conv_error.no_matching_variant_found
                        error_source__190_
                        sexp__206_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__191_)
                    ; conv =
                        (fun sexp__197_ ->
                          try
                            match sexp__197_ with
                            | Sexplib0.Sexp.Atom atom__193_ as _sexp__195_ ->
                              (match atom__193_ with
                               | "B" -> `B
                               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                            | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__193_ :: _) as
                              _sexp__195_ ->
                              (match atom__193_ with
                               | "B" ->
                                 Sexplib0.Sexp_conv_error.ptag_no_args
                                   error_source__190_
                                   _sexp__195_
                               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                            | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__194_
                              ->
                              Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                                error_source__190_
                                sexp__194_
                            | Sexplib0.Sexp.List [] as sexp__194_ ->
                              Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                                error_source__190_
                                sexp__194_
                          with
                          | Sexplib0.Sexp_conv_error.No_variant_match ->
                            Sexplib0.Sexp_conv_error.no_matching_variant_found
                              error_source__190_
                              sexp__197_)
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : t -> { a; b })
         x__207_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (let (default__214_ : [ `B ]) = `B
     and (drop_default__213_ : [ `B ] -> [ `B ] -> Stdlib.Bool.t) = Poly.equal in
     fun { a = a__209_; b = b__215_ } ->
       let bnds__208_ = ([] : _ Stdlib.List.t) in
       let bnds__208_ =
         if drop_default__213_ default__214_ b__215_
         then bnds__208_
         else (
           let arg__217_ = (fun `B -> Sexplib0.Sexp.Atom "B") b__215_ in
           let bnd__216_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__217_ ] in
           (bnd__216_ :: bnds__208_ : _ Stdlib.List.t))
       in
       let bnds__208_ =
         let arg__210_ =
           let (`A v__211_) = a__209_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v__211_ ]
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__210_ ] :: bnds__208_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__208_
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
    (let error_source__219_ = "expansion.ml.Poly_record.t" in
     fun x__229_ ->
       let open struct
         type a__220_ = { a__220_ : 'a. 'a list } [@@unboxed]
         type b__221_ = { b__221_ : 'b. 'b option } [@@unboxed]
         type c__222_ = { c__222_ : 'c. 'c } [@@unboxed]
       end in
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__219_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun sexp__227_ ->
                    { a__220_ =
                        (let _a__228_ =
                           Sexplib0.Sexp_conv_error.record_poly_field_value
                             error_source__219_
                         in
                         list_of_sexp _a__228_ sexp__227_)
                    })
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun sexp__225_ ->
                          { b__221_ =
                              (let _b__226_ =
                                 Sexplib0.Sexp_conv_error.record_poly_field_value
                                   error_source__219_
                               in
                               option_of_sexp _b__226_ sexp__225_)
                          })
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun sexp__223_ ->
                                { c__222_ =
                                    (let _c__224_ =
                                       Sexplib0.Sexp_conv_error.record_poly_field_value
                                         error_source__219_
                                     in
                                     _c__224_ sexp__223_)
                                })
                          ; rest = Empty
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun ({ a__220_ = a }, ({ b__221_ = b }, ({ c__222_ = c }, ()))) : t ->
           { a; b; c })
         x__229_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__231_; b = b__234_; c = c__237_ } ->
       let bnds__230_ = ([] : _ Stdlib.List.t) in
       let bnds__230_ =
         let arg__238_ =
           let _of_c__239_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           _of_c__239_ c__237_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__238_ ] :: bnds__230_
           : _ Stdlib.List.t)
       in
       let bnds__230_ =
         let arg__235_ =
           let _of_b__236_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_option _of_b__236_ b__234_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__235_ ] :: bnds__230_
           : _ Stdlib.List.t)
       in
       let bnds__230_ =
         let arg__232_ =
           let _of_a__233_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_list _of_a__233_ a__231_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__232_ ] :: bnds__230_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__230_
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
    (let (default__242_ : int) = 0
     and (default__243_ : int) = 0
     and (default__244_ : int) = 0
     and (default__245_ : int) = 0
     and (default__246_ : int) = 0 in
     let error_source__241_ = "expansion.ml.Record_with_defaults.t" in
     fun x__247_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__241_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__246_)
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__245_)
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__244_)
                          ; conv = int_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__243_)
                                ; conv = int_of_sexp
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__242_)
                                      ; conv = int_of_sexp
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv = int_of_sexp
                                            ; rest = Empty
                                            }
                                      }
                                }
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | "d" -> 3
           | "e" -> 4
           | "f" -> 5
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, (d, (e, (f, ())))))) : t -> { a; b; c; d; e; f })
         x__247_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let (default__252_ : int) = 0
     and (default__257_ : int) = 0
     and (default__262_ : int) = 0
     and (default__268_ : int) = 0
     and (drop_default__267_ : int -> int -> Stdlib.Bool.t) = ( = )
     and (drop_if__273_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t) = fun () -> ( = ) 0 in
     fun { a = a__249_; b = b__253_; c = c__258_; d = d__263_; e = e__269_; f = f__274_ } ->
       let bnds__248_ = ([] : _ Stdlib.List.t) in
       let bnds__248_ =
         if (drop_if__273_ ()) f__274_
         then bnds__248_
         else (
           let arg__276_ = sexp_of_int f__274_ in
           let bnd__275_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__276_ ] in
           (bnd__275_ :: bnds__248_ : _ Stdlib.List.t))
       in
       let bnds__248_ =
         if drop_default__267_ default__268_ e__269_
         then bnds__248_
         else (
           let arg__271_ = sexp_of_int e__269_ in
           let bnd__270_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__271_ ] in
           (bnd__270_ :: bnds__248_ : _ Stdlib.List.t))
       in
       let bnds__248_ =
         let arg__265_ = sexp_of_int d__263_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__262_) arg__265_
         then bnds__248_
         else (
           let bnd__264_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__265_ ] in
           (bnd__264_ :: bnds__248_ : _ Stdlib.List.t))
       in
       let bnds__248_ =
         if [%equal: int] default__257_ c__258_
         then bnds__248_
         else (
           let arg__260_ = sexp_of_int c__258_ in
           let bnd__259_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__260_ ] in
           (bnd__259_ :: bnds__248_ : _ Stdlib.List.t))
       in
       let bnds__248_ =
         if [%compare.equal: int] default__252_ b__253_
         then bnds__248_
         else (
           let arg__255_ = sexp_of_int b__253_ in
           let bnd__254_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__255_ ] in
           (bnd__254_ :: bnds__248_ : _ Stdlib.List.t))
       in
       let bnds__248_ =
         let arg__250_ = sexp_of_int a__249_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__250_ ] :: bnds__248_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__248_
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
    (let error_source__284_ = "expansion.ml.Record_with_special_types.t" in
     fun x__285_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__284_
         ~fields:
           (Field
              { name = "a"
              ; kind = Sexp_option
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Sexp_list
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Sexp_array
                          ; conv = int_of_sexp
                          ; rest =
                              Field
                                { name = "d"; kind = Sexp_bool; conv = (); rest = Empty }
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | "d" -> 3
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, (d, ())))) : t -> { a; b; c; d })
         x__285_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__287_; b = b__292_; c = c__296_; d = d__299_ } ->
       let bnds__286_ = ([] : _ Stdlib.List.t) in
       let bnds__286_ =
         if d__299_
         then (
           let bnd__300_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__300_ :: bnds__286_ : _ Stdlib.List.t))
         else bnds__286_
       in
       let bnds__286_ =
         if match c__296_ with
            | [||] -> true
            | _ -> false
         then bnds__286_
         else (
           let arg__298_ = (sexp_of_array sexp_of_int) c__296_ in
           let bnd__297_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__298_ ] in
           (bnd__297_ :: bnds__286_ : _ Stdlib.List.t))
       in
       let bnds__286_ =
         if match b__292_ with
            | [] -> true
            | _ -> false
         then bnds__286_
         else (
           let arg__294_ = (sexp_of_list sexp_of_int) b__292_ in
           let bnd__293_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__294_ ] in
           (bnd__293_ :: bnds__286_ : _ Stdlib.List.t))
       in
       let bnds__286_ =
         match a__287_ with
         | Stdlib.Option.None -> bnds__286_
         | Stdlib.Option.Some v__288_ ->
           let arg__290_ = sexp_of_int v__288_ in
           let bnd__289_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__290_ ] in
           (bnd__289_ :: bnds__286_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__286_
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
    (let error_source__302_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__303_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__302_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; conv = option_of_sexp int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; conv = list_of_sexp int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; conv = unit_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; conv = int_of_sexp
                                ; rest = Empty
                                }
                          }
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | "c" -> 2
           | "d" -> 3
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, (d, ())))) : t -> { a; b; c; d })
         x__303_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__305_; b = b__307_; c = c__309_; d = d__311_ } ->
       let bnds__304_ = ([] : _ Stdlib.List.t) in
       let bnds__304_ =
         match sexp_of_int d__311_ with
         | Sexplib0.Sexp.List [] -> bnds__304_
         | arg__312_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__312_ ] :: bnds__304_
             : _ Stdlib.List.t)
       in
       let bnds__304_ =
         match sexp_of_unit c__309_ with
         | Sexplib0.Sexp.List [] -> bnds__304_
         | arg__310_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__310_ ] :: bnds__304_
             : _ Stdlib.List.t)
       in
       let bnds__304_ =
         match sexp_of_list sexp_of_int b__307_ with
         | Sexplib0.Sexp.List [] -> bnds__304_
         | arg__308_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__308_ ] :: bnds__304_
             : _ Stdlib.List.t)
       in
       let bnds__304_ =
         match sexp_of_option sexp_of_int a__305_ with
         | Sexplib0.Sexp.List [] -> bnds__304_
         | arg__306_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__306_ ] :: bnds__304_
             : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__304_
      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__315_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__318_) :: sexp_args__319_) as
       _sexp__317_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__319_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__316_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__315_ sexp__316_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__314_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__315_ sexp__314_
     | Sexplib0.Sexp.List [] as sexp__314_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__315_ sexp__314_
     | sexp__314_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__315_ sexp__314_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__320_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__320_)
      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__327_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__322_ as _sexp__324_ ->
       (match atom__322_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__327_ _sexp__324_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__322_ :: sexp_args__325_) as
       _sexp__324_ ->
       (match atom__322_ with
        | "A" as _tag__326_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__325_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__323_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__327_ sexp__323_
     | Sexplib0.Sexp.List [] as sexp__323_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__327_ sexp__323_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__329_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__328_ ->
       try __t_of_sexp__ sexp__328_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__329_ sexp__328_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__330_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__330_)
      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__332_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__333_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__332_
         ~fields:(Field { name = "a"; kind = Required; conv = int_of_sexp; rest = Empty })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t -> { a })
         x__333_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__335_ } ->
       let bnds__334_ = ([] : _ Stdlib.List.t) in
       let bnds__334_ =
         let arg__336_ = sexp_of_int a__335_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__336_ ] :: bnds__334_
           : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__334_
      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__338_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__338_
      : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__339_ -> sexp_of_list Sexplib0.Sexp_conv.sexp_of_opaque x__339_
      : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end
