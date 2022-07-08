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

  and c = int [@@deriving_inline sexp]

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

  and c_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> c)

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
      | A -> Sexplib0.Sexp.Atom "A"
      | B arg0__133_ ->
        let res0__134_ = sexp_of_b arg0__133_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__134_ ]
      | C { a = a__136_; b = b__138_; c = c__140_ } ->
        let bnds__135_ = ([] : _ Stdlib.List.t) in
        let bnds__135_ =
          let arg__141_ = sexp_of_c c__140_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__141_ ] :: bnds__135_
           : _ Stdlib.List.t)
        in
        let bnds__135_ =
          let arg__139_ = sexp_of_b b__138_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__139_ ] :: bnds__135_
           : _ Stdlib.List.t)
        in
        let bnds__135_ =
          let arg__137_ = sexp_of_a a__136_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__137_ ] :: bnds__135_
           : _ Stdlib.List.t)
        in
        Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__135_)
        : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = a__143_; b = b__145_ } ->
       let bnds__142_ = ([] : _ Stdlib.List.t) in
       let bnds__142_ =
         let arg__146_ = sexp_of_b b__145_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__146_ ] :: bnds__142_
          : _ Stdlib.List.t)
       in
       let bnds__142_ =
         let arg__144_ = sexp_of_a a__143_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__144_ ] :: bnds__142_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__142_
       : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (sexp_of_int : c -> Sexplib0.Sexp.t)

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
    (let error_source__150_ = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__153_) :: sexp_args__154_) as
       _sexp__152_ ->
       (match sexp_args__154_ with
        | [ arg0__155_ ] ->
          let res0__156_ = t_of_sexp arg0__155_ in
          Banana res0__156_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__150_
            _tag__153_
            _sexp__152_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__151_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__150_ sexp__151_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__151_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__150_ sexp__151_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__149_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__150_ sexp__149_
     | Sexplib0.Sexp.List [] as sexp__149_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__150_ sexp__149_
     | sexp__149_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__150_ sexp__149_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
      | Banana arg0__157_ ->
        let res0__158_ = sexp_of_t arg0__157_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__158_ ]
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
    fun _of_a__159_ x__161_ -> option_of_sexp (list_of_sexp _of_a__159_) x__161_
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__162_ x__163_ -> sexp_of_option (sexp_of_list _of_a__162_) x__163_
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
    fun _of_a__171_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__173_ _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
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
    fun _of_a__174_ _of_b__175_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t :
    'a 'b.
    ('a -> Sexplib0.Sexp.t)
    -> ('b -> Sexplib0.Sexp.t)
    -> ('a, 'b) t
    -> Sexplib0.Sexp.t
    =
    fun _of_a__177_ _of_b__178_ _ ->
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

  let sexp_of_t : 'a__180_. ('a__180_ -> Sexplib0.Sexp.t) -> 'a__180_ t -> Sexplib0.Sexp.t
    =
    fun (type a__186_) : ((a__186_ -> Sexplib0.Sexp.t) -> a__186_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__181_ -> function
      | A -> Sexplib0.Sexp.Atom "A"
      | B arg0__182_ ->
        let res0__183_ = sexp_of_int arg0__182_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__183_ ]
      | C arg0__184_ ->
        let res0__185_ = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") arg0__184_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; res0__185_ ]
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
    (let (default__189_ : [ `B ]) = `B in
     let error_source__188_ = "expansion.ml.Recursive_record_containing_variant.t" in
     fun x__205_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__188_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun sexp__204_ ->
                     try
                       match sexp__204_ with
                       | Sexplib0.Sexp.Atom atom__197_ as _sexp__199_ ->
                         (match atom__197_ with
                          | "A" ->
                            Sexplib0.Sexp_conv_error.ptag_takes_args
                              error_source__188_
                              _sexp__199_
                          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                       | Sexplib0.Sexp.List
                           (Sexplib0.Sexp.Atom atom__197_ :: sexp_args__200_) as
                         _sexp__199_ ->
                         (match atom__197_ with
                          | "A" as _tag__201_ ->
                            (match sexp_args__200_ with
                             | [ arg0__202_ ] ->
                               let res0__203_ = t_of_sexp arg0__202_ in
                               `A res0__203_
                             | _ ->
                               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                 error_source__188_
                                 _tag__201_
                                 _sexp__199_)
                          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__198_ ->
                         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                           error_source__188_
                           sexp__198_
                       | Sexplib0.Sexp.List [] as sexp__198_ ->
                         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                           error_source__188_
                           sexp__198_
                     with
                     | Sexplib0.Sexp_conv_error.No_variant_match ->
                       Sexplib0.Sexp_conv_error.no_matching_variant_found
                         error_source__188_
                         sexp__204_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__189_)
                    ; conv =
                        (fun sexp__195_ ->
                           try
                             match sexp__195_ with
                             | Sexplib0.Sexp.Atom atom__191_ as _sexp__193_ ->
                               (match atom__191_ with
                                | "B" -> `B
                                | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                             | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__191_ :: _) as
                               _sexp__193_ ->
                               (match atom__191_ with
                                | "B" ->
                                  Sexplib0.Sexp_conv_error.ptag_no_args
                                    error_source__188_
                                    _sexp__193_
                                | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                             | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__192_
                               ->
                               Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                                 error_source__188_
                                 sexp__192_
                             | Sexplib0.Sexp.List [] as sexp__192_ ->
                               Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                                 error_source__188_
                                 sexp__192_
                           with
                           | Sexplib0.Sexp_conv_error.No_variant_match ->
                             Sexplib0.Sexp_conv_error.no_matching_variant_found
                               error_source__188_
                               sexp__195_)
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : t -> { a; b })
         x__205_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (let (default__212_ : [ `B ]) = `B
     and (drop_default__211_ : [ `B ] -> [ `B ] -> Stdlib.Bool.t) = Poly.equal in
     fun { a = a__207_; b = b__213_ } ->
       let bnds__206_ = ([] : _ Stdlib.List.t) in
       let bnds__206_ =
         if drop_default__211_ default__212_ b__213_
         then bnds__206_
         else (
           let arg__215_ = (fun `B -> Sexplib0.Sexp.Atom "B") b__213_ in
           let bnd__214_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__215_ ] in
           (bnd__214_ :: bnds__206_ : _ Stdlib.List.t))
       in
       let bnds__206_ =
         let arg__208_ =
           let (`A v__209_) = a__207_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v__209_ ]
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__208_ ] :: bnds__206_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__206_
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
    (let error_source__217_ = "expansion.ml.Poly_record.t" in
     fun x__227_ ->
       let open struct
         type a__218_ = { a__218_ : 'a. 'a list } [@@unboxed]
         type b__219_ = { b__219_ : 'b. 'b option } [@@unboxed]
         type c__220_ = { c__220_ : 'c. 'c } [@@unboxed]
       end in
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__217_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun sexp__225_ ->
                     { a__218_ =
                         (let _a__226_ =
                            Sexplib0.Sexp_conv_error.record_poly_field_value
                              error_source__217_
                          in
                          list_of_sexp _a__226_ sexp__225_)
                     })
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun sexp__223_ ->
                           { b__219_ =
                               (let _b__224_ =
                                  Sexplib0.Sexp_conv_error.record_poly_field_value
                                    error_source__217_
                                in
                                option_of_sexp _b__224_ sexp__223_)
                           })
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun sexp__221_ ->
                                 { c__220_ =
                                     (let _c__222_ =
                                        Sexplib0.Sexp_conv_error.record_poly_field_value
                                          error_source__217_
                                      in
                                      _c__222_ sexp__221_)
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
         ~create:(fun ({ a__218_ = a }, ({ b__219_ = b }, ({ c__220_ = c }, ()))) : t ->
           { a; b; c })
         x__227_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__229_; b = b__232_; c = c__235_ } ->
       let bnds__228_ = ([] : _ Stdlib.List.t) in
       let bnds__228_ =
         let arg__236_ =
           let _of_c__237_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           _of_c__237_ c__235_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__236_ ] :: bnds__228_
          : _ Stdlib.List.t)
       in
       let bnds__228_ =
         let arg__233_ =
           let _of_b__234_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_option _of_b__234_ b__232_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__233_ ] :: bnds__228_
          : _ Stdlib.List.t)
       in
       let bnds__228_ =
         let arg__230_ =
           let _of_a__231_ = Sexplib0.Sexp_conv.sexp_of_opaque in
           sexp_of_list _of_a__231_ a__229_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__230_ ] :: bnds__228_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__228_
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
    (let (default__240_ : int) = 0
     and (default__241_ : int) = 0
     and (default__242_ : int) = 0
     and (default__243_ : int) = 0
     and (default__244_ : int) = 0 in
     let error_source__239_ = "expansion.ml.Record_with_defaults.t" in
     fun x__245_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__239_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__244_)
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__243_)
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__242_)
                          ; conv = int_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__241_)
                                ; conv = int_of_sexp
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__240_)
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
         x__245_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let (default__250_ : int) = 0
     and (default__255_ : int) = 0
     and (default__260_ : int) = 0
     and (default__266_ : int) = 0
     and (drop_default__265_ : int -> int -> Stdlib.Bool.t) = ( = )
     and (drop_if__271_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t) = fun () -> ( = ) 0 in
     fun { a = a__247_; b = b__251_; c = c__256_; d = d__261_; e = e__267_; f = f__272_ } ->
       let bnds__246_ = ([] : _ Stdlib.List.t) in
       let bnds__246_ =
         if (drop_if__271_ ()) f__272_
         then bnds__246_
         else (
           let arg__274_ = sexp_of_int f__272_ in
           let bnd__273_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__274_ ] in
           (bnd__273_ :: bnds__246_ : _ Stdlib.List.t))
       in
       let bnds__246_ =
         if drop_default__265_ default__266_ e__267_
         then bnds__246_
         else (
           let arg__269_ = sexp_of_int e__267_ in
           let bnd__268_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__269_ ] in
           (bnd__268_ :: bnds__246_ : _ Stdlib.List.t))
       in
       let bnds__246_ =
         let arg__263_ = sexp_of_int d__261_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__260_) arg__263_
         then bnds__246_
         else (
           let bnd__262_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__263_ ] in
           (bnd__262_ :: bnds__246_ : _ Stdlib.List.t))
       in
       let bnds__246_ =
         if [%equal: int] default__255_ c__256_
         then bnds__246_
         else (
           let arg__258_ = sexp_of_int c__256_ in
           let bnd__257_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__258_ ] in
           (bnd__257_ :: bnds__246_ : _ Stdlib.List.t))
       in
       let bnds__246_ =
         if [%compare.equal: int] default__250_ b__251_
         then bnds__246_
         else (
           let arg__253_ = sexp_of_int b__251_ in
           let bnd__252_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__253_ ] in
           (bnd__252_ :: bnds__246_ : _ Stdlib.List.t))
       in
       let bnds__246_ =
         let arg__248_ = sexp_of_int a__247_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__248_ ] :: bnds__246_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__246_
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
    (let error_source__282_ = "expansion.ml.Record_with_special_types.t" in
     fun x__283_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__282_
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
         x__283_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__285_; b = b__290_; c = c__294_; d = d__297_ } ->
       let bnds__284_ = ([] : _ Stdlib.List.t) in
       let bnds__284_ =
         if d__297_
         then (
           let bnd__298_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__298_ :: bnds__284_ : _ Stdlib.List.t))
         else bnds__284_
       in
       let bnds__284_ =
         if match c__294_ with
           | [||] -> true
           | _ -> false
         then bnds__284_
         else (
           let arg__296_ = (sexp_of_array sexp_of_int) c__294_ in
           let bnd__295_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__296_ ] in
           (bnd__295_ :: bnds__284_ : _ Stdlib.List.t))
       in
       let bnds__284_ =
         if match b__290_ with
           | [] -> true
           | _ -> false
         then bnds__284_
         else (
           let arg__292_ = (sexp_of_list sexp_of_int) b__290_ in
           let bnd__291_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__292_ ] in
           (bnd__291_ :: bnds__284_ : _ Stdlib.List.t))
       in
       let bnds__284_ =
         match a__285_ with
         | Stdlib.Option.None -> bnds__284_
         | Stdlib.Option.Some v__286_ ->
           let arg__288_ = sexp_of_int v__286_ in
           let bnd__287_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__288_ ] in
           (bnd__287_ :: bnds__284_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__284_
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
    (let error_source__300_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__301_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__300_
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
         x__301_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__303_; b = b__305_; c = c__307_; d = d__309_ } ->
       let bnds__302_ = ([] : _ Stdlib.List.t) in
       let bnds__302_ =
         match sexp_of_int d__309_ with
         | Sexplib0.Sexp.List [] -> bnds__302_
         | arg__310_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__310_ ] :: bnds__302_
            : _ Stdlib.List.t)
       in
       let bnds__302_ =
         match sexp_of_unit c__307_ with
         | Sexplib0.Sexp.List [] -> bnds__302_
         | arg__308_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__308_ ] :: bnds__302_
            : _ Stdlib.List.t)
       in
       let bnds__302_ =
         match sexp_of_list sexp_of_int b__305_ with
         | Sexplib0.Sexp.List [] -> bnds__302_
         | arg__306_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__306_ ] :: bnds__302_
            : _ Stdlib.List.t)
       in
       let bnds__302_ =
         match sexp_of_option sexp_of_int a__303_ with
         | Sexplib0.Sexp.List [] -> bnds__302_
         | arg__304_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__304_ ] :: bnds__302_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__302_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__313_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__316_) :: sexp_args__317_) as
       _sexp__315_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__317_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__314_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__313_ sexp__314_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__312_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__313_ sexp__312_
     | Sexplib0.Sexp.List [] as sexp__312_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__313_ sexp__312_
     | sexp__312_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__313_ sexp__312_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__318_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__318_)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__325_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__320_ as _sexp__322_ ->
       (match atom__320_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__325_ _sexp__322_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__320_ :: sexp_args__323_) as
       _sexp__322_ ->
       (match atom__320_ with
        | "A" as _tag__324_ -> `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__323_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__321_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__325_ sexp__321_
     | Sexplib0.Sexp.List [] as sexp__321_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__325_ sexp__321_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__327_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__326_ ->
       try __t_of_sexp__ sexp__326_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__327_ sexp__326_
         : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__328_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__328_)
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__330_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__331_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__330_
         ~fields:(Field { name = "a"; kind = Required; conv = int_of_sexp; rest = Empty })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t -> { a })
         x__331_
       : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__333_ } ->
       let bnds__332_ = ([] : _ Stdlib.List.t) in
       let bnds__332_ =
         let arg__334_ = sexp_of_int a__333_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__334_ ] :: bnds__332_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__332_
       : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__336_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__336_
                    : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__337_ -> sexp_of_list Sexplib0.Sexp_conv.sexp_of_opaque x__337_
                    : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end
