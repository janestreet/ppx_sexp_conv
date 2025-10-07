open! Base

[@@@disable_unused_warnings]

open struct
  type _shadow_constructors =
    | []
    | ( :: )
    | None
    | Some
end

module%template Abstract = struct
  type t [@@deriving_inline sexp [@alloc stack]]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__002_ = "expansion.ml.Abstract.t" in
     fun x__003_ -> Sexplib0.Sexp_conv_error.empty_type error_source__002_ x__003_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp
  let sexp_of_t = (fun _ -> assert false : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  let sexp_of_t__stack =
    (fun _ -> exclave_ assert false : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

  [@@@end]
end

module Tuple = struct
  type t = int * int * int [@@deriving_inline sexp ~stackify]

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

  let sexp_of_t__stack =
    (fun (arg0__019_, arg1__020_, arg2__021_) -> exclave_
       let res0__022_ = sexp_of_int__stack arg0__019_
       and res1__023_ = sexp_of_int__stack arg1__020_
       and res2__024_ = sexp_of_int__stack arg2__021_ in
       Sexplib0.Sexp.List [ res0__022_; res1__023_; res2__024_ ]
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

  [@@@end]
end

module Record = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    }
  [@@deriving_inline sexp ~stackify]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__026_ = "expansion.ml.Record.t" in
     fun x__033_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__026_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__031_ ->
                    let _x__032_ = int_of_sexp x__031_ in
                    fun () -> _x__032_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__029_ ->
                          let _x__030_ = int_of_sexp x__029_ in
                          fun () -> _x__030_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__027_ ->
                                let _x__028_ = int_of_sexp x__027_ in
                                fun () -> _x__028_)
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
         ~create:(fun (a, (b, (c, ()))) : t ->
           let a = a () in
           let b = b () in
           let c = c () in
           { a; b; c })
         x__033_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__035_; b = b__037_; c = c__039_ } ->
       let bnds__034_ = ([] : _ Stdlib.List.t) in
       let bnds__034_ =
         let arg__040_ = sexp_of_int c__039_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__040_ ] :: bnds__034_
          : _ Stdlib.List.t)
       in
       let bnds__034_ =
         let arg__038_ = sexp_of_int b__037_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__038_ ] :: bnds__034_
          : _ Stdlib.List.t)
       in
       let bnds__034_ =
         let arg__036_ = sexp_of_int a__035_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__036_ ] :: bnds__034_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__034_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (fun { a = a__042_; b = b__044_; c = c__046_ } -> exclave_
       let bnds__041_ = ([] : _ Stdlib.List.t) in
       let bnds__041_ =
         let arg__047_ = sexp_of_int__stack c__046_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__047_ ] :: bnds__041_
          : _ Stdlib.List.t)
       in
       let bnds__041_ =
         let arg__045_ = sexp_of_int__stack b__044_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__045_ ] :: bnds__041_
          : _ Stdlib.List.t)
       in
       let bnds__041_ =
         let arg__043_ = sexp_of_int__stack a__042_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__043_ ] :: bnds__041_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__041_
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

  [@@@end]
end

module Mutable_record = struct
  type t =
    { mutable a : int
    ; mutable b : int
    ; mutable c : int
    }
  [@@deriving_inline sexp ~stackify]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__049_ = "expansion.ml.Mutable_record.t" in
     fun x__056_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__049_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__054_ ->
                    let _x__055_ = int_of_sexp x__054_ in
                    fun () -> _x__055_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__052_ ->
                          let _x__053_ = int_of_sexp x__052_ in
                          fun () -> _x__053_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__050_ ->
                                let _x__051_ = int_of_sexp x__050_ in
                                fun () -> _x__051_)
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
         ~create:(fun (a, (b, (c, ()))) : t ->
           let a = a () in
           let b = b () in
           let c = c () in
           { a; b; c })
         x__056_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__058_; b = b__060_; c = c__062_ } ->
       let bnds__057_ = ([] : _ Stdlib.List.t) in
       let bnds__057_ =
         let arg__063_ = sexp_of_int c__062_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__063_ ] :: bnds__057_
          : _ Stdlib.List.t)
       in
       let bnds__057_ =
         let arg__061_ = sexp_of_int b__060_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__061_ ] :: bnds__057_
          : _ Stdlib.List.t)
       in
       let bnds__057_ =
         let arg__059_ = sexp_of_int a__058_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__059_ ] :: bnds__057_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__057_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (fun { a = a__065_; b = b__067_; c = c__069_ } -> exclave_
       let bnds__064_ = ([] : _ Stdlib.List.t) in
       let bnds__064_ =
         let arg__070_ = sexp_of_int__stack c__069_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__070_ ] :: bnds__064_
          : _ Stdlib.List.t)
       in
       let bnds__064_ =
         let arg__068_ = sexp_of_int__stack b__067_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__068_ ] :: bnds__064_
          : _ Stdlib.List.t)
       in
       let bnds__064_ =
         let arg__066_ = sexp_of_int__stack a__065_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__066_ ] :: bnds__064_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__064_
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

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
  [@@deriving_inline sexp ~stackify]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__073_ = "expansion.ml.Variant.t" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__076_) :: sexp_args__077_) as
       _sexp__075_ ->
       (match sexp_args__077_ with
        | [ arg0__078_; arg1__079_ ] ->
          let res0__080_ = int_of_sexp arg0__078_
          and res1__081_ = int_of_sexp arg1__079_ in
          B (res0__080_, res1__081_)
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__073_
            _tag__076_
            _sexp__075_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__089_) as sexp__088_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__088_
         ~caller:error_source__073_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__086_ ->
                    let _x__087_ = int_of_sexp x__086_ in
                    fun () -> _x__087_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__084_ ->
                          let _x__085_ = int_of_sexp x__084_ in
                          fun () -> _x__085_)
                    ; rest =
                        Field
                          { name = "d"
                          ; kind = Required
                          ; conv =
                              (fun x__082_ ->
                                let _x__083_ = int_of_sexp x__082_ in
                                fun () -> _x__083_)
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
         ~create:(fun (a, (b, (d, ()))) : t ->
           let a = a () in
           let b = b () in
           let d = d () in
           C { a; b; d })
         sexps__089_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("d" | "D") :: sexps__097_) as sexp__096_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__096_
         ~caller:error_source__073_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__094_ ->
                    let _x__095_ = int_of_sexp x__094_ in
                    fun () -> _x__095_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__092_ ->
                          let _x__093_ = int_of_sexp x__092_ in
                          fun () -> _x__093_)
                    ; rest =
                        Field
                          { name = "t"
                          ; kind = Required
                          ; conv =
                              (fun x__090_ ->
                                let _x__091_ = int_of_sexp x__090_ in
                                fun () -> _x__091_)
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
         ~create:(fun (a, (b, (t, ()))) : t ->
           let a = a () in
           let b = b () in
           let t = t () in
           D { a; b; t })
         sexps__097_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__074_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__073_ sexp__074_
     | Sexplib0.Sexp.Atom ("b" | "B" | "c" | "C" | "d" | "D") as sexp__074_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__073_ sexp__074_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__072_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__073_ sexp__072_
     | Sexplib0.Sexp.List [] as sexp__072_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__073_ sexp__072_
     | sexp__072_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__073_
         [ "A"; "B"; "C"; "D" ]
         sexp__072_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B (arg0__098_, arg1__099_) ->
       let res0__100_ = sexp_of_int arg0__098_
       and res1__101_ = sexp_of_int arg1__099_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__100_; res1__101_ ]
     | C { a = a__103_; b = b__105_; d = d__107_ } ->
       let bnds__102_ = ([] : _ Stdlib.List.t) in
       let bnds__102_ =
         let arg__108_ = sexp_of_int d__107_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__108_ ] :: bnds__102_
          : _ Stdlib.List.t)
       in
       let bnds__102_ =
         let arg__106_ = sexp_of_int b__105_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__106_ ] :: bnds__102_
          : _ Stdlib.List.t)
       in
       let bnds__102_ =
         let arg__104_ = sexp_of_int a__103_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__104_ ] :: bnds__102_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__102_)
     | D { a = a__110_; b = b__112_; t = t__114_ } ->
       let bnds__109_ = ([] : _ Stdlib.List.t) in
       let bnds__109_ =
         let arg__115_ = sexp_of_int t__114_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__115_ ] :: bnds__109_
          : _ Stdlib.List.t)
       in
       let bnds__109_ =
         let arg__113_ = sexp_of_int b__112_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__113_ ] :: bnds__109_
          : _ Stdlib.List.t)
       in
       let bnds__109_ =
         let arg__111_ = sexp_of_int a__110_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__111_ ] :: bnds__109_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__109_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (function
     | A -> exclave_ Sexplib0.Sexp.Atom "A"
     | B (arg0__116_, arg1__117_) ->
       exclave_
       let res0__118_ = sexp_of_int__stack arg0__116_
       and res1__119_ = sexp_of_int__stack arg1__117_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__118_; res1__119_ ]
     | C { a = a__121_; b = b__123_; d = d__125_ } ->
       exclave_
       let bnds__120_ = ([] : _ Stdlib.List.t) in
       let bnds__120_ =
         let arg__126_ = sexp_of_int__stack d__125_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__126_ ] :: bnds__120_
          : _ Stdlib.List.t)
       in
       let bnds__120_ =
         let arg__124_ = sexp_of_int__stack b__123_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__124_ ] :: bnds__120_
          : _ Stdlib.List.t)
       in
       let bnds__120_ =
         let arg__122_ = sexp_of_int__stack a__121_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__122_ ] :: bnds__120_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__120_)
     | D { a = a__128_; b = b__130_; t = t__132_ } ->
       exclave_
       let bnds__127_ = ([] : _ Stdlib.List.t) in
       let bnds__127_ =
         let arg__133_ = sexp_of_int__stack t__132_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__133_ ] :: bnds__127_
          : _ Stdlib.List.t)
       in
       let bnds__127_ =
         let arg__131_ = sexp_of_int__stack b__130_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__131_ ] :: bnds__127_
          : _ Stdlib.List.t)
       in
       let bnds__127_ =
         let arg__129_ = sexp_of_int__stack a__128_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__129_ ] :: bnds__127_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__127_)
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

  [@@@end]
end

module Poly_variant = struct
  type t =
    [ `A
    | `B of int
    ]
  [@@deriving_inline sexp ~stackify]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__139_ = "expansion.ml.Poly_variant.t" in
     function
     | Sexplib0.Sexp.Atom atom__135_ as _sexp__137_ ->
       (match atom__135_ with
        | "A" -> `A
        | "B" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__139_ _sexp__137_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__135_ :: sexp_args__138_) as
       _sexp__137_ ->
       (match atom__135_ with
        | "B" as _tag__140_ ->
          (match sexp_args__138_ with
           | arg0__141_ :: [] ->
             let res0__142_ = int_of_sexp arg0__141_ in
             `B res0__142_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__139_
               _tag__140_
               _sexp__137_)
        | "A" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__139_ _sexp__137_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__136_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__139_ sexp__136_
     | Sexplib0.Sexp.List [] as sexp__136_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__139_ sexp__136_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__144_ = "expansion.ml.Poly_variant.t" in
     fun sexp__143_ ->
       try __t_of_sexp__ sexp__143_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__144_ sexp__143_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | `A -> Sexplib0.Sexp.Atom "A"
     | `B v__145_ -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int v__145_ ]
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (function
     | `A -> exclave_ Sexplib0.Sexp.Atom "A"
     | `B v__146_ ->
       exclave_ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int__stack v__146_ ]
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

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
    (let error_source__158_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__147_ ->
       try (Poly_variant.__t_of_sexp__ sexp__147_ :> t) with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         (match sexp__147_ with
          | Sexplib0.Sexp.Atom atom__148_ as _sexp__150_ ->
            (match atom__148_ with
             | "C" ->
               Sexplib0.Sexp_conv_error.ptag_takes_args error_source__158_ _sexp__150_
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__148_ :: sexp_args__151_) as
            _sexp__150_ ->
            (match atom__148_ with
             | "C" as _tag__152_ ->
               (match sexp_args__151_ with
                | arg0__159_ :: [] ->
                  let res0__160_ =
                    match arg0__159_ with
                    | Sexplib0.Sexp.List [ arg0__153_; arg1__154_ ] ->
                      let res0__155_ = int_of_sexp arg0__153_
                      and res1__156_ = int_of_sexp arg1__154_ in
                      res0__155_, res1__156_
                    | sexp__157_ ->
                      Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                        error_source__158_
                        2
                        sexp__157_
                  in
                  `C res0__160_
                | _ ->
                  Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                    error_source__158_
                    _tag__152_
                    _sexp__150_)
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__149_ ->
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
              error_source__158_
              sexp__149_
          | Sexplib0.Sexp.List [] as sexp__149_ ->
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
              error_source__158_
              sexp__149_)
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__162_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__161_ ->
       try __t_of_sexp__ sexp__161_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__162_ sexp__161_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | #Poly_variant.t as v__163_ -> Poly_variant.sexp_of_t v__163_
     | `C v__164_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "C"
         ; (let arg0__165_, arg1__166_ = v__164_ in
            let res0__167_ = sexp_of_int arg0__165_
            and res1__168_ = sexp_of_int arg1__166_ in
            Sexplib0.Sexp.List [ res0__167_; res1__168_ ])
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
    (let error_source__171_ = "expansion.ml.Recursive.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__174_) :: sexp_args__175_) as
       _sexp__173_ ->
       (match sexp_args__175_ with
        | arg0__176_ :: [] ->
          let res0__177_ = t_of_sexp arg0__176_ in
          Banana res0__177_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__171_
            _tag__174_
            _sexp__173_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__172_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__171_ sexp__172_
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__172_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__171_ sexp__172_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__170_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__171_ sexp__170_
     | Sexplib0.Sexp.List [] as sexp__170_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__171_ sexp__170_
     | sexp__170_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__171_
         [ "Banana"; "Orange" ]
         sexp__170_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__178_ ->
       let res0__179_ = sexp_of_t arg0__178_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__179_ ]
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
    (let error_source__183_ = "expansion.ml.Mutually_recursive.a" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__186_) :: sexp_args__187_) as
       _sexp__185_ ->
       (match sexp_args__187_ with
        | arg0__188_ :: [] ->
          let res0__189_ = b_of_sexp arg0__188_ in
          B res0__189_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__183_
            _tag__186_
            _sexp__185_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__197_) as sexp__196_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__196_
         ~caller:error_source__183_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__194_ ->
                    let _x__195_ = a_of_sexp x__194_ in
                    fun () -> _x__195_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__192_ ->
                          let _x__193_ = b_of_sexp x__192_ in
                          fun () -> _x__193_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__190_ ->
                                let _x__191_ = c_of_sexp x__190_ in
                                fun () -> _x__191_)
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
         ~create:(fun (a, (b, (c, ()))) : a ->
           let a = a () in
           let b = b () in
           let c = c () in
           C { a; b; c })
         sexps__197_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__184_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__183_ sexp__184_
     | Sexplib0.Sexp.Atom ("b" | "B" | "c" | "C") as sexp__184_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__183_ sexp__184_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__182_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__183_ sexp__182_
     | Sexplib0.Sexp.List [] as sexp__182_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__183_ sexp__182_
     | sexp__182_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__183_
         [ "A"; "B"; "C" ]
         sexp__182_
     : Sexplib0.Sexp.t -> a)

  and b_of_sexp =
    (let error_source__199_ = "expansion.ml.Mutually_recursive.b" in
     fun x__204_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__199_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__202_ ->
                    let _x__203_ = a_of_sexp x__202_ in
                    fun () -> _x__203_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__200_ ->
                          let _x__201_ = b_of_sexp x__200_ in
                          fun () -> _x__201_)
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : b ->
           let a = a () in
           let b = b () in
           { a; b })
         x__204_
     : Sexplib0.Sexp.t -> b)

  and c_of_sexp = (fun x__206_ -> a_of_sexp x__206_ : Sexplib0.Sexp.t -> c)

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B arg0__207_ ->
       let res0__208_ = sexp_of_b arg0__207_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__208_ ]
     | C { a = a__210_; b = b__212_; c = c__214_ } ->
       let bnds__209_ = ([] : _ Stdlib.List.t) in
       let bnds__209_ =
         let arg__215_ = sexp_of_c c__214_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__215_ ] :: bnds__209_
          : _ Stdlib.List.t)
       in
       let bnds__209_ =
         let arg__213_ = sexp_of_b b__212_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__213_ ] :: bnds__209_
          : _ Stdlib.List.t)
       in
       let bnds__209_ =
         let arg__211_ = sexp_of_a a__210_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__211_ ] :: bnds__209_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__209_)
     : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = a__217_; b = b__219_ } ->
       let bnds__216_ = ([] : _ Stdlib.List.t) in
       let bnds__216_ =
         let arg__220_ = sexp_of_b b__219_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__220_ ] :: bnds__216_
          : _ Stdlib.List.t)
       in
       let bnds__216_ =
         let arg__218_ = sexp_of_a a__217_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__218_ ] :: bnds__216_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__216_
     : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (fun x__221_ -> sexp_of_a x__221_ : c -> Sexplib0.Sexp.t)

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
    (let error_source__225_ = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__228_) :: sexp_args__229_) as
       _sexp__227_ ->
       (match sexp_args__229_ with
        | arg0__230_ :: [] ->
          let res0__231_ = t_of_sexp arg0__230_ in
          Banana res0__231_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__225_
            _tag__228_
            _sexp__227_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__226_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__225_ sexp__226_
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__226_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__225_ sexp__226_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__224_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__225_ sexp__224_
     | Sexplib0.Sexp.List [] as sexp__224_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__225_ sexp__224_
     | sexp__224_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__225_
         [ "Banana"; "Orange" ]
         sexp__224_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__232_ ->
       let res0__233_ = sexp_of_t arg0__232_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__233_ ]
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
    fun _of_a__234_ x__236_ -> option_of_sexp (list_of_sexp _of_a__234_) x__236_
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__237_ x__238_ -> sexp_of_option (sexp_of_list _of_a__237_) x__238_
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Binary = struct
  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp
    : 'a 'b.
    (Sexplib0.Sexp.t -> 'a) -> (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t
    =
    Either.t_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t
    : 'a 'b.
    ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
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
    fun _of_a__246_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__248_ _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Second_order = struct
  type ('a, 'b) t = ('a -> 'a) -> ('a -> 'b) -> ('b -> 'b) -> 'a -> 'b
  [@@deriving_inline sexp]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let t_of_sexp
    : 'a 'b.
    (Sexplib0.Sexp.t -> 'a) -> (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t
    =
    fun _of_a__249_ _of_b__250_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t
    : 'a 'b.
    ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
    =
    fun _of_a__252_ _of_b__253_ _ ->
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

  let sexp_of_t : 'a__255_. ('a__255_ -> Sexplib0.Sexp.t) -> 'a__255_ t -> Sexplib0.Sexp.t
    =
    fun (type a__261_) : ((a__261_ -> Sexplib0.Sexp.t) -> a__261_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__256_ -> function
    | A -> Sexplib0.Sexp.Atom "A"
    | B arg0__257_ ->
      let res0__258_ = sexp_of_int arg0__257_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__258_ ]
    | C arg0__259_ ->
      let res0__260_ = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") arg0__259_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; res0__260_ ]
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
    (let default__264_ : [ `B ] = `B in
     let error_source__263_ = "expansion.ml.Recursive_record_containing_variant.t" in
     fun x__284_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__263_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__282_ ->
                    let _x__283_ =
                      (fun sexp__281_ ->
                        try
                          match sexp__281_ with
                          | Sexplib0.Sexp.Atom atom__274_ as _sexp__276_ ->
                            (match atom__274_ with
                             | "A" ->
                               Sexplib0.Sexp_conv_error.ptag_takes_args
                                 error_source__263_
                                 _sexp__276_
                             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                          | Sexplib0.Sexp.List
                              (Sexplib0.Sexp.Atom atom__274_ :: sexp_args__277_) as
                            _sexp__276_ ->
                            (match atom__274_ with
                             | "A" as _tag__278_ ->
                               (match sexp_args__277_ with
                                | arg0__279_ :: [] ->
                                  let res0__280_ = t_of_sexp arg0__279_ in
                                  `A res0__280_
                                | _ ->
                                  Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                    error_source__263_
                                    _tag__278_
                                    _sexp__276_)
                             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__275_
                            ->
                            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                              error_source__263_
                              sexp__275_
                          | Sexplib0.Sexp.List [] as sexp__275_ ->
                            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                              error_source__263_
                              sexp__275_
                        with
                        | Sexplib0.Sexp_conv_error.No_variant_match ->
                          Sexplib0.Sexp_conv_error.no_matching_variant_found
                            error_source__263_
                            sexp__281_)
                        x__282_
                    in
                    fun () -> _x__283_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__264_)
                    ; conv =
                        (fun x__271_ ->
                          let _x__272_ =
                            (fun sexp__270_ ->
                              try
                                match sexp__270_ with
                                | Sexplib0.Sexp.Atom atom__266_ as _sexp__268_ ->
                                  (match atom__266_ with
                                   | "B" -> `B
                                   | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                                | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__266_ :: _)
                                  as _sexp__268_ ->
                                  (match atom__266_ with
                                   | "B" ->
                                     Sexplib0.Sexp_conv_error.ptag_no_args
                                       error_source__263_
                                       _sexp__268_
                                   | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                                | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as
                                  sexp__267_ ->
                                  Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                                    error_source__263_
                                    sexp__267_
                                | Sexplib0.Sexp.List [] as sexp__267_ ->
                                  Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                                    error_source__263_
                                    sexp__267_
                              with
                              | Sexplib0.Sexp_conv_error.No_variant_match ->
                                Sexplib0.Sexp_conv_error.no_matching_variant_found
                                  error_source__263_
                                  sexp__270_)
                              x__271_
                          in
                          fun () -> _x__272_)
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : t ->
           let a = a () in
           let b = b () in
           { a; b })
         x__284_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (let default__291_ : [ `B ] = `B
     and drop_default__290_ : [ `B ] -> [ `B ] -> Stdlib.Bool.t = Poly.equal in
     fun { a = a__286_; b = b__292_ } ->
       let bnds__285_ = ([] : _ Stdlib.List.t) in
       let bnds__285_ =
         if drop_default__290_ default__291_ b__292_
         then bnds__285_
         else (
           let arg__294_ = (fun `B -> Sexplib0.Sexp.Atom "B") b__292_ in
           let bnd__293_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__294_ ] in
           (bnd__293_ :: bnds__285_ : _ Stdlib.List.t))
       in
       let bnds__285_ =
         let arg__287_ =
           let (`A v__288_) = a__286_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v__288_ ]
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__287_ ] :: bnds__285_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__285_
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
    (let error_source__296_ = "expansion.ml.Poly_record.t" in
     fun x__309_ ->
       let open struct
         type a__297_ = { a__297_ : 'a. 'a list } [@@unboxed]
         type b__298_ = { b__298_ : 'b. 'b option } [@@unboxed]
         type c__299_ = { c__299_ : 'c. 'c } [@@unboxed]
       end in
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__296_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun sexp__306_ ->
                    let _x__308_ =
                      { a__297_ =
                          (let _a__307_ =
                             Sexplib0.Sexp_conv_error.record_poly_field_value
                               error_source__296_
                           in
                           list_of_sexp _a__307_ sexp__306_)
                      }
                    in
                    fun () -> _x__308_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun sexp__303_ ->
                          let _x__305_ =
                            { b__298_ =
                                (let _b__304_ =
                                   Sexplib0.Sexp_conv_error.record_poly_field_value
                                     error_source__296_
                                 in
                                 option_of_sexp _b__304_ sexp__303_)
                            }
                          in
                          fun () -> _x__305_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun sexp__300_ ->
                                let _x__302_ =
                                  { c__299_ =
                                      (let _c__301_ =
                                         Sexplib0.Sexp_conv_error.record_poly_field_value
                                           error_source__296_
                                       in
                                       _c__301_ sexp__300_)
                                  }
                                in
                                fun () -> _x__302_)
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
         ~create:(fun (a, (b, (c, ()))) : t ->
           let { a__297_ = a } = a () in
           let { b__298_ = b } = b () in
           let { c__299_ = c } = c () in
           { a; b; c })
         x__309_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__311_; b = b__314_; c = c__317_ } ->
       let bnds__310_ = ([] : _ Stdlib.List.t) in
       let bnds__310_ =
         let arg__318_ =
           let _of_c__319_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           _of_c__319_ c__317_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__318_ ] :: bnds__310_
          : _ Stdlib.List.t)
       in
       let bnds__310_ =
         let arg__315_ =
           let _of_b__316_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_option _of_b__316_ b__314_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__315_ ] :: bnds__310_
          : _ Stdlib.List.t)
       in
       let bnds__310_ =
         let arg__312_ =
           let _of_a__313_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_list _of_a__313_ a__311_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__312_ ] :: bnds__310_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__310_
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
    (let default__326_ : int = 0
     and default__325_ : int = 0
     and default__324_ : int = 0
     and default__323_ : int = 0
     and default__322_ : int = 0 in
     let error_source__321_ = "expansion.ml.Record_with_defaults.t" in
     fun x__339_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__321_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__322_)
              ; conv =
                  (fun x__337_ ->
                    let _x__338_ = int_of_sexp x__337_ in
                    fun () -> _x__338_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__323_)
                    ; conv =
                        (fun x__335_ ->
                          let _x__336_ = int_of_sexp x__335_ in
                          fun () -> _x__336_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__324_)
                          ; conv =
                              (fun x__333_ ->
                                let _x__334_ = int_of_sexp x__333_ in
                                fun () -> _x__334_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__325_)
                                ; conv =
                                    (fun x__331_ ->
                                      let _x__332_ = int_of_sexp x__331_ in
                                      fun () -> _x__332_)
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__326_)
                                      ; conv =
                                          (fun x__329_ ->
                                            let _x__330_ = int_of_sexp x__329_ in
                                            fun () -> _x__330_)
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv =
                                                (fun x__327_ ->
                                                  let _x__328_ = int_of_sexp x__327_ in
                                                  fun () -> _x__328_)
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
         ~create:(fun (a, (b, (c, (d, (e, (f, ())))))) : t ->
           let a = a () in
           let b = b () in
           let c = c () in
           let d = d () in
           let e = e () in
           let f = f () in
           { a; b; c; d; e; f })
         x__339_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__344_ : int = 0
     and default__349_ : int = 0
     and default__354_ : int = 0
     and default__360_ : int = 0
     and drop_default__359_ : int -> int -> Stdlib.Bool.t = ( = )
     and drop_if__365_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t = fun () -> ( = ) 0 in
     fun { a = a__341_; b = b__345_; c = c__350_; d = d__355_; e = e__361_; f = f__366_ } ->
       let bnds__340_ = ([] : _ Stdlib.List.t) in
       let bnds__340_ =
         if (drop_if__365_ ()) f__366_
         then bnds__340_
         else (
           let arg__368_ = sexp_of_int f__366_ in
           let bnd__367_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__368_ ] in
           (bnd__367_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         if drop_default__359_ default__360_ e__361_
         then bnds__340_
         else (
           let arg__363_ = sexp_of_int e__361_ in
           let bnd__362_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__363_ ] in
           (bnd__362_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         let arg__357_ = sexp_of_int d__355_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__354_) arg__357_
         then bnds__340_
         else (
           let bnd__356_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__357_ ] in
           (bnd__356_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         if [%equal: int] default__349_ c__350_
         then bnds__340_
         else (
           let arg__352_ = sexp_of_int c__350_ in
           let bnd__351_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__352_ ] in
           (bnd__351_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         if [%compare.equal: int] default__344_ b__345_
         then bnds__340_
         else (
           let arg__347_ = sexp_of_int b__345_ in
           let bnd__346_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__347_ ] in
           (bnd__346_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         let arg__342_ = sexp_of_int a__341_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__342_ ] :: bnds__340_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__340_
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
    (let error_source__376_ = "expansion.ml.Record_with_special_types.t" in
     fun x__377_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__376_
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
         x__377_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__379_; b = b__384_; c = c__388_; d = d__391_ } ->
       let bnds__378_ = ([] : _ Stdlib.List.t) in
       let bnds__378_ =
         if d__391_
         then (
           let bnd__392_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__392_ :: bnds__378_ : _ Stdlib.List.t))
         else bnds__378_
       in
       let bnds__378_ =
         if match c__388_ with
            | [||] -> true
            | _ -> false
         then bnds__378_
         else (
           let arg__390_ = (sexp_of_array sexp_of_int) c__388_ in
           let bnd__389_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__390_ ] in
           (bnd__389_ :: bnds__378_ : _ Stdlib.List.t))
       in
       let bnds__378_ =
         if match b__384_ with
            | [] -> true
            | _ -> false
         then bnds__378_
         else (
           let arg__386_ = (sexp_of_list sexp_of_int) b__384_ in
           let bnd__385_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__386_ ] in
           (bnd__385_ :: bnds__378_ : _ Stdlib.List.t))
       in
       let bnds__378_ =
         match a__379_ with
         | Stdlib.Option.None -> bnds__378_
         | Stdlib.Option.Some v__380_ ->
           let arg__382_ = sexp_of_int v__380_ in
           let bnd__381_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__382_ ] in
           (bnd__381_ :: bnds__378_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__378_
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
    (let error_source__394_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__403_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__394_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; conv =
                  (fun x__401_ ->
                    let _x__402_ = (option_of_sexp int_of_sexp) x__401_ in
                    fun () -> _x__402_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; conv =
                        (fun x__399_ ->
                          let _x__400_ = (list_of_sexp int_of_sexp) x__399_ in
                          fun () -> _x__400_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; conv =
                              (fun x__397_ ->
                                let _x__398_ = unit_of_sexp x__397_ in
                                fun () -> _x__398_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; conv =
                                    (fun x__395_ ->
                                      let _x__396_ = int_of_sexp x__395_ in
                                      fun () -> _x__396_)
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
         ~create:(fun (a, (b, (c, (d, ())))) : t ->
           let a = a () in
           let b = b () in
           let c = c () in
           let d = d () in
           { a; b; c; d })
         x__403_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__405_; b = b__407_; c = c__409_; d = d__411_ } ->
       let bnds__404_ = ([] : _ Stdlib.List.t) in
       let bnds__404_ =
         match sexp_of_int d__411_ with
         | Sexplib0.Sexp.List [] -> bnds__404_
         | arg__412_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__412_ ] :: bnds__404_
            : _ Stdlib.List.t)
       in
       let bnds__404_ =
         match sexp_of_unit c__409_ with
         | Sexplib0.Sexp.List [] -> bnds__404_
         | arg__410_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__410_ ] :: bnds__404_
            : _ Stdlib.List.t)
       in
       let bnds__404_ =
         match sexp_of_list sexp_of_int b__407_ with
         | Sexplib0.Sexp.List [] -> bnds__404_
         | arg__408_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__408_ ] :: bnds__404_
            : _ Stdlib.List.t)
       in
       let bnds__404_ =
         match sexp_of_option sexp_of_int a__405_ with
         | Sexplib0.Sexp.List [] -> bnds__404_
         | arg__406_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__406_ ] :: bnds__404_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__404_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__415_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__418_) :: sexp_args__419_) as
       _sexp__417_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__419_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__416_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__415_ sexp__416_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__414_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__415_ sexp__414_
     | Sexplib0.Sexp.List [] as sexp__414_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__415_ sexp__414_
     | sexp__414_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__415_ [ "A" ] sexp__414_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__420_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__420_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__427_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__422_ as _sexp__424_ ->
       (match atom__422_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__427_ _sexp__424_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__422_ :: sexp_args__425_) as
       _sexp__424_ ->
       (match atom__422_ with
        | "A" as _tag__426_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__425_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__423_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__427_ sexp__423_
     | Sexplib0.Sexp.List [] as sexp__423_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__427_ sexp__423_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__429_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__428_ ->
       try __t_of_sexp__ sexp__428_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__429_ sexp__428_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__430_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__430_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__432_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__435_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__432_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__433_ ->
                    let _x__434_ = int_of_sexp x__433_ in
                    fun () -> _x__434_)
              ; rest = Empty
              })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t ->
           let a = a () in
           { a })
         x__435_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__437_ } ->
       let bnds__436_ = ([] : _ Stdlib.List.t) in
       let bnds__436_ =
         let arg__438_ = sexp_of_int a__437_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__438_ ] :: bnds__436_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__436_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__440_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__440_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__441_ -> sexp_of_list (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) x__441_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

[@@@expand_inline
  let [%sexp_of: Functor(T).t] = ()
  let [%of_sexp: Functor(T).t] = ()]

let sexp_of_functor__t = ()
let functor__t_of_sexp = ()

[@@@end]

module Portable = struct
  type t =
    { u : int u
    ; b : int
    }

  and 'a u =
    { t : t
    ; a : 'a
    }
  [@@deriving_inline sexp ~portable]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : 'a u) -> ()

  include struct
    let rec t_of_sexp @ portable =
      (let error_source__443_ = "expansion.ml.Portable.t" in
       fun x__448_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__443_
           ~fields:
             (Field
                { name = "u"
                ; kind = Required
                ; conv =
                    (fun x__446_ ->
                      let _x__447_ = (u_of_sexp int_of_sexp) x__446_ in
                      fun () -> _x__447_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__444_ ->
                            let _x__445_ = int_of_sexp x__444_ in
                            fun () -> _x__445_)
                      ; rest = Empty
                      }
                })
           ~index_of_field:(function
             | "u" -> 0
             | "b" -> 1
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (u, (b, ())) : t ->
             let u = u () in
             let b = b () in
             { u; b })
           x__448_
       : Sexplib0.Sexp.t -> t)

    and u_of_sexp : 'a. ((Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a u) @ portable =
      let error_source__451_ = "expansion.ml.Portable.u" in
      fun _of_a__449_ x__456_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__451_
          ~fields:
            (Field
               { name = "t"
               ; kind = Required
               ; conv =
                   (fun x__454_ ->
                     let _x__455_ = t_of_sexp x__454_ in
                     fun () -> _x__455_)
               ; rest =
                   Field
                     { name = "a"
                     ; kind = Required
                     ; conv =
                         (fun x__452_ ->
                           let _x__453_ = _of_a__449_ x__452_ in
                           fun () -> _x__453_)
                     ; rest = Empty
                     }
               })
          ~index_of_field:(function
            | "t" -> 0
            | "a" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (t, (a, ())) : _ u ->
            let t = t () in
            let a = a () in
            { t; a })
          x__456_
    ;;

    let _ = t_of_sexp
    and _ = u_of_sexp
  end

  let _ @ portable = t_of_sexp
  and _ @ portable = u_of_sexp

  include struct
    let rec sexp_of_t @ portable =
      (fun { u = u__458_; b = b__460_ } ->
         let bnds__457_ = ([] : _ Stdlib.List.t) in
         let bnds__457_ =
           let arg__461_ = sexp_of_int b__460_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__461_ ] :: bnds__457_
            : _ Stdlib.List.t)
         in
         let bnds__457_ =
           let arg__459_ = sexp_of_u sexp_of_int u__458_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "u"; arg__459_ ] :: bnds__457_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__457_
       : t -> Sexplib0.Sexp.t)

    and sexp_of_u : 'a. (('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t) @ portable =
      fun _of_a__462_ { t = t__464_; a = a__466_ } ->
      let bnds__463_ = ([] : _ Stdlib.List.t) in
      let bnds__463_ =
        let arg__467_ = _of_a__462_ a__466_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__467_ ] :: bnds__463_
         : _ Stdlib.List.t)
      in
      let bnds__463_ =
        let arg__465_ = sexp_of_t t__464_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__465_ ] :: bnds__463_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__463_
    ;;

    let _ = sexp_of_t
    and _ = sexp_of_u
  end

  let _ @ portable = sexp_of_t
  and _ @ portable = sexp_of_u

  [@@@end]
end
