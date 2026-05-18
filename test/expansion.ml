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
                    let _x__032_ = (int_of_sexp [@inlined never]) x__031_ in
                    fun () -> _x__032_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__029_ ->
                          let _x__030_ = (int_of_sexp [@inlined never]) x__029_ in
                          fun () -> _x__030_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__027_ ->
                                let _x__028_ = (int_of_sexp [@inlined never]) x__027_ in
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
                    let _x__055_ = (int_of_sexp [@inlined never]) x__054_ in
                    fun () -> _x__055_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__052_ ->
                          let _x__053_ = (int_of_sexp [@inlined never]) x__052_ in
                          fun () -> _x__053_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__050_ ->
                                let _x__051_ = (int_of_sexp [@inlined never]) x__050_ in
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
                    let _x__087_ = (int_of_sexp [@inlined never]) x__086_ in
                    fun () -> _x__087_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__084_ ->
                          let _x__085_ = (int_of_sexp [@inlined never]) x__084_ in
                          fun () -> _x__085_)
                    ; rest =
                        Field
                          { name = "d"
                          ; kind = Required
                          ; conv =
                              (fun x__082_ ->
                                let _x__083_ = (int_of_sexp [@inlined never]) x__082_ in
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
                    let _x__095_ = (int_of_sexp [@inlined never]) x__094_ in
                    fun () -> _x__095_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__092_ ->
                          let _x__093_ = (int_of_sexp [@inlined never]) x__092_ in
                          fun () -> _x__093_)
                    ; rest =
                        Field
                          { name = "t"
                          ; kind = Required
                          ; conv =
                              (fun x__090_ ->
                                let _x__091_ = (int_of_sexp [@inlined never]) x__090_ in
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
                    let _x__195_ = (a_of_sexp [@inlined never]) x__194_ in
                    fun () -> _x__195_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__192_ ->
                          let _x__193_ = (b_of_sexp [@inlined never]) x__192_ in
                          fun () -> _x__193_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun x__190_ ->
                                let _x__191_ = (c_of_sexp [@inlined never]) x__190_ in
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
                    let _x__203_ = (a_of_sexp [@inlined never]) x__202_ in
                    fun () -> _x__203_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun x__200_ ->
                          let _x__201_ = (b_of_sexp [@inlined never]) x__200_ in
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
                      (fun [@inlined never] sexp__281_ ->
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
                            (fun [@inlined never] sexp__270_ ->
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
     fun x__312_ ->
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
                  (fun sexp__308_ ->
                    let _x__311_ =
                      let _x__310_ =
                        let _a__309_ =
                          Sexplib0.Sexp_conv_error.record_poly_field_value
                            error_source__296_
                        in
                        list_of_sexp _a__309_ sexp__308_
                      in
                      { a__297_ = _x__310_ }
                    in
                    fun () -> _x__311_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; conv =
                        (fun sexp__304_ ->
                          let _x__307_ =
                            let _x__306_ =
                              let _b__305_ =
                                Sexplib0.Sexp_conv_error.record_poly_field_value
                                  error_source__296_
                              in
                              option_of_sexp _b__305_ sexp__304_
                            in
                            { b__298_ = _x__306_ }
                          in
                          fun () -> _x__307_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; conv =
                              (fun sexp__300_ ->
                                let _x__303_ =
                                  let _x__302_ =
                                    let _c__301_ =
                                      Sexplib0.Sexp_conv_error.record_poly_field_value
                                        error_source__296_
                                    in
                                    _c__301_ sexp__300_
                                  in
                                  { c__299_ = _x__302_ }
                                in
                                fun () -> _x__303_)
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
         x__312_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__314_; b = b__317_; c = c__320_ } ->
       let bnds__313_ = ([] : _ Stdlib.List.t) in
       let bnds__313_ =
         let arg__321_ =
           let _of_c__322_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           _of_c__322_ c__320_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__321_ ] :: bnds__313_
          : _ Stdlib.List.t)
       in
       let bnds__313_ =
         let arg__318_ =
           let _of_b__319_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_option _of_b__319_ b__317_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__318_ ] :: bnds__313_
          : _ Stdlib.List.t)
       in
       let bnds__313_ =
         let arg__315_ =
           let _of_a__316_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_list _of_a__316_ a__314_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__315_ ] :: bnds__313_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__313_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_defaults = struct
  type t =
    { a : int [@default 0]
    ; b : int [@default 0] [@sexp_drop_default.compare.local]
    ; c : int [@default 0] [@sexp_drop_default.equal.local]
    ; d : int [@default 0] [@sexp_drop_default.sexp]
    ; e : int [@default 0] [@sexp_drop_default ( = )]
    ; f : int [@sexp_drop_if ( = ) 0]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let default__329_ : int = 0
     and default__328_ : int = 0
     and default__327_ : int = 0
     and default__326_ : int = 0
     and default__325_ : int = 0 in
     let error_source__324_ = "expansion.ml.Record_with_defaults.t" in
     fun x__342_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__324_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__325_)
              ; conv =
                  (fun x__340_ ->
                    let _x__341_ = (int_of_sexp [@inlined never]) x__340_ in
                    fun () -> _x__341_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__326_)
                    ; conv =
                        (fun x__338_ ->
                          let _x__339_ = (int_of_sexp [@inlined never]) x__338_ in
                          fun () -> _x__339_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__327_)
                          ; conv =
                              (fun x__336_ ->
                                let _x__337_ = (int_of_sexp [@inlined never]) x__336_ in
                                fun () -> _x__337_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__328_)
                                ; conv =
                                    (fun x__334_ ->
                                      let _x__335_ =
                                        (int_of_sexp [@inlined never]) x__334_
                                      in
                                      fun () -> _x__335_)
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__329_)
                                      ; conv =
                                          (fun x__332_ ->
                                            let _x__333_ =
                                              (int_of_sexp [@inlined never]) x__332_
                                            in
                                            fun () -> _x__333_)
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv =
                                                (fun x__330_ ->
                                                  let _x__331_ =
                                                    (int_of_sexp [@inlined never]) x__330_
                                                  in
                                                  fun () -> _x__331_)
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
         x__342_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__347_ : int = 0
     and default__352_ : int = 0
     and default__357_ : int = 0
     and default__363_ : int = 0
     and drop_default__362_ : int -> int -> Stdlib.Bool.t = ( = )
     and drop_if__368_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t = fun () -> ( = ) 0 in
     fun { a = a__344_; b = b__348_; c = c__353_; d = d__358_; e = e__364_; f = f__369_ } ->
       let bnds__343_ = ([] : _ Stdlib.List.t) in
       let bnds__343_ =
         if (drop_if__368_ ()) f__369_
         then bnds__343_
         else (
           let arg__371_ = sexp_of_int f__369_ in
           let bnd__370_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__371_ ] in
           (bnd__370_ :: bnds__343_ : _ Stdlib.List.t))
       in
       let bnds__343_ =
         if drop_default__362_ default__363_ e__364_
         then bnds__343_
         else (
           let arg__366_ = sexp_of_int e__364_ in
           let bnd__365_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__366_ ] in
           (bnd__365_ :: bnds__343_ : _ Stdlib.List.t))
       in
       let bnds__343_ =
         let arg__360_ = sexp_of_int d__358_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__357_) arg__360_
         then bnds__343_
         else (
           let bnd__359_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__360_ ] in
           (bnd__359_ :: bnds__343_ : _ Stdlib.List.t))
       in
       let bnds__343_ =
         if [%equal__local: int] default__352_ c__353_
         then bnds__343_
         else (
           let arg__355_ = sexp_of_int c__353_ in
           let bnd__354_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__355_ ] in
           (bnd__354_ :: bnds__343_ : _ Stdlib.List.t))
       in
       let bnds__343_ =
         if [%compare.equal__local: int] default__347_ b__348_
         then bnds__343_
         else (
           let arg__350_ = sexp_of_int b__348_ in
           let bnd__349_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__350_ ] in
           (bnd__349_ :: bnds__343_ : _ Stdlib.List.t))
       in
       let bnds__343_ =
         let arg__345_ = sexp_of_int a__344_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__345_ ] :: bnds__343_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__343_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_with_defaults_and_stackify = struct
  type t =
    { a : string [@default ""]
    ; b : string [@default ""] [@sexp_drop_default.compare.local]
    ; c : string [@default ""] [@sexp_drop_default.equal.local]
    ; d : string [@default ""] [@sexp_drop_default.sexp]
    ; e : string [@default ""] [@sexp_drop_default String.equal [@mode local]]
    ; f : string [@sexp_drop_if fun s -> (String.equal [@mode local]) "" s]
    }
  [@@deriving_inline sexp ~stackify]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let default__384_ : string = ""
     and default__383_ : string = ""
     and default__382_ : string = ""
     and default__381_ : string = ""
     and default__380_ : string = "" in
     let error_source__379_ = "expansion.ml.Record_with_defaults_and_stackify.t" in
     fun x__397_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__379_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__380_)
              ; conv =
                  (fun x__395_ ->
                    let _x__396_ = (string_of_sexp [@inlined never]) x__395_ in
                    fun () -> _x__396_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__381_)
                    ; conv =
                        (fun x__393_ ->
                          let _x__394_ = (string_of_sexp [@inlined never]) x__393_ in
                          fun () -> _x__394_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__382_)
                          ; conv =
                              (fun x__391_ ->
                                let _x__392_ =
                                  (string_of_sexp [@inlined never]) x__391_
                                in
                                fun () -> _x__392_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__383_)
                                ; conv =
                                    (fun x__389_ ->
                                      let _x__390_ =
                                        (string_of_sexp [@inlined never]) x__389_
                                      in
                                      fun () -> _x__390_)
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__384_)
                                      ; conv =
                                          (fun x__387_ ->
                                            let _x__388_ =
                                              (string_of_sexp [@inlined never]) x__387_
                                            in
                                            fun () -> _x__388_)
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv =
                                                (fun x__385_ ->
                                                  let _x__386_ =
                                                    (string_of_sexp [@inlined never])
                                                      x__385_
                                                  in
                                                  fun () -> _x__386_)
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
         x__397_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__402_ : string = ""
     and default__407_ : string = ""
     and default__412_ : string = ""
     and default__418_ : string = ""
     and drop_default__417_ : local_ string -> local_ string -> Stdlib.Bool.t =
       String.equal__local
     and drop_if__423_ : local_ string -> Stdlib.Bool.t =
       fun s -> String.equal__local "" s
     in
     fun { a = a__399_; b = b__403_; c = c__408_; d = d__413_; e = e__419_; f = f__424_ } ->
       let bnds__398_ = ([] : _ Stdlib.List.t) in
       let bnds__398_ =
         if drop_if__423_ f__424_
         then bnds__398_
         else (
           let arg__426_ = sexp_of_string f__424_ in
           let bnd__425_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__426_ ] in
           (bnd__425_ :: bnds__398_ : _ Stdlib.List.t))
       in
       let bnds__398_ =
         if drop_default__417_ default__418_ e__419_
         then bnds__398_
         else (
           let arg__421_ = sexp_of_string e__419_ in
           let bnd__420_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__421_ ] in
           (bnd__420_ :: bnds__398_ : _ Stdlib.List.t))
       in
       let bnds__398_ =
         let arg__415_ = sexp_of_string d__413_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_string default__412_) arg__415_
         then bnds__398_
         else (
           let bnd__414_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__415_ ] in
           (bnd__414_ :: bnds__398_ : _ Stdlib.List.t))
       in
       let bnds__398_ =
         if [%equal__local: string] default__407_ c__408_
         then bnds__398_
         else (
           let arg__410_ = sexp_of_string c__408_ in
           let bnd__409_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__410_ ] in
           (bnd__409_ :: bnds__398_ : _ Stdlib.List.t))
       in
       let bnds__398_ =
         if [%compare.equal__local: string] default__402_ b__403_
         then bnds__398_
         else (
           let arg__405_ = sexp_of_string b__403_ in
           let bnd__404_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__405_ ] in
           (bnd__404_ :: bnds__398_ : _ Stdlib.List.t))
       in
       let bnds__398_ =
         let arg__400_ = sexp_of_string a__399_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__400_ ] :: bnds__398_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__398_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (let default__431_ : string = ""
     and default__436_ : string = ""
     and default__441_ : string = ""
     and default__447_ : string = ""
     and drop_default__446_ : local_ string -> local_ string -> Stdlib.Bool.t =
       String.equal__local
     and drop_if__452_ : local_ string -> Stdlib.Bool.t =
       fun s -> String.equal__local "" s
     in
     fun { a = a__428_; b = b__432_; c = c__437_; d = d__442_; e = e__448_; f = f__453_ } -> exclave_
       let bnds__427_ = ([] : _ Stdlib.List.t) in
       let bnds__427_ =
         if drop_if__452_ f__453_
         then bnds__427_
         else (
           let arg__455_ = sexp_of_string__stack f__453_ in
           let bnd__454_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__455_ ] in
           (bnd__454_ :: bnds__427_ : _ Stdlib.List.t))
       in
       let bnds__427_ =
         if drop_default__446_ default__447_ e__448_
         then bnds__427_
         else (
           let arg__450_ = sexp_of_string__stack e__448_ in
           let bnd__449_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__450_ ] in
           (bnd__449_ :: bnds__427_ : _ Stdlib.List.t))
       in
       let bnds__427_ =
         let arg__444_ = sexp_of_string__stack d__442_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_string__stack default__441_) arg__444_
         then bnds__427_
         else (
           let bnd__443_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__444_ ] in
           (bnd__443_ :: bnds__427_ : _ Stdlib.List.t))
       in
       let bnds__427_ =
         if [%equal__local: string] default__436_ c__437_
         then bnds__427_
         else (
           let arg__439_ = sexp_of_string__stack c__437_ in
           let bnd__438_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__439_ ] in
           (bnd__438_ :: bnds__427_ : _ Stdlib.List.t))
       in
       let bnds__427_ =
         if [%compare.equal__local: string] default__431_ b__432_
         then bnds__427_
         else (
           let arg__434_ = sexp_of_string__stack b__432_ in
           let bnd__433_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__434_ ] in
           (bnd__433_ :: bnds__427_ : _ Stdlib.List.t))
       in
       let bnds__427_ =
         let arg__429_ = sexp_of_string__stack a__428_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__429_ ] :: bnds__427_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__427_
     : local_ t -> local_ Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t__stack

  [@@@end]
end

module Record_with_explicit_local_defaults = struct
  type t =
    { a : string [@default ""] [@sexp_drop_default.compare.local]
    ; b : string [@default ""] [@sexp_drop_default.equal.local]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let default__471_ : string = ""
     and default__470_ : string = "" in
     let error_source__469_ = "expansion.ml.Record_with_explicit_local_defaults.t" in
     fun x__476_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__469_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__470_)
              ; conv =
                  (fun x__474_ ->
                    let _x__475_ = (string_of_sexp [@inlined never]) x__474_ in
                    fun () -> _x__475_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__471_)
                    ; conv =
                        (fun x__472_ ->
                          let _x__473_ = (string_of_sexp [@inlined never]) x__472_ in
                          fun () -> _x__473_)
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
         x__476_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__479_ : string = ""
     and default__484_ : string = "" in
     fun { a = a__480_; b = b__485_ } ->
       let bnds__477_ = ([] : _ Stdlib.List.t) in
       let bnds__477_ =
         if [%equal__local: string] default__484_ b__485_
         then bnds__477_
         else (
           let arg__487_ = sexp_of_string b__485_ in
           let bnd__486_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__487_ ] in
           (bnd__486_ :: bnds__477_ : _ Stdlib.List.t))
       in
       let bnds__477_ =
         if [%compare.equal__local: string] default__479_ a__480_
         then bnds__477_
         else (
           let arg__482_ = sexp_of_string a__480_ in
           let bnd__481_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__482_ ] in
           (bnd__481_ :: bnds__477_ : _ Stdlib.List.t))
       in
       Sexplib0.Sexp.List bnds__477_
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
    ; e : int or_null [@sexp.or_null]
    }
  [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__495_ = "expansion.ml.Record_with_special_types.t" in
     fun x__496_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__495_
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
                                { name = "d"
                                ; kind = Sexp_bool
                                ; conv = ()
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Sexp_or_null
                                      ; conv = int_of_sexp
                                      ; rest = Empty
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
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, (c, (d, (e, ()))))) : t -> { a; b; c; d; e })
         x__496_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__498_; b = b__503_; c = c__507_; d = d__510_; e = e__512_ } ->
       let bnds__497_ = ([] : _ Stdlib.List.t) in
       let bnds__497_ =
         match e__512_ with
         | Ppx_sexp_conv_lib.Or_null.Null -> bnds__497_
         | Ppx_sexp_conv_lib.Or_null.This v__513_ ->
           let arg__515_ = sexp_of_int v__513_ in
           let bnd__514_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__515_ ] in
           (bnd__514_ :: bnds__497_ : _ Stdlib.List.t)
       in
       let bnds__497_ =
         if d__510_
         then (
           let bnd__511_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__511_ :: bnds__497_ : _ Stdlib.List.t))
         else bnds__497_
       in
       let bnds__497_ =
         if match c__507_ with
            | [||] -> true
            | _ -> false
         then bnds__497_
         else (
           let arg__509_ = (sexp_of_array sexp_of_int) c__507_ in
           let bnd__508_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__509_ ] in
           (bnd__508_ :: bnds__497_ : _ Stdlib.List.t))
       in
       let bnds__497_ =
         if match b__503_ with
            | [] -> true
            | _ -> false
         then bnds__497_
         else (
           let arg__505_ = (sexp_of_list sexp_of_int) b__503_ in
           let bnd__504_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__505_ ] in
           (bnd__504_ :: bnds__497_ : _ Stdlib.List.t))
       in
       let bnds__497_ =
         match a__498_ with
         | Stdlib.Option.None -> bnds__497_
         | Stdlib.Option.Some v__499_ ->
           let arg__501_ = sexp_of_int v__499_ in
           let bnd__500_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__501_ ] in
           (bnd__500_ :: bnds__497_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__497_
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
    (let error_source__517_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__526_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__517_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; conv =
                  (fun x__524_ ->
                    let _x__525_ =
                      (option_of_sexp int_of_sexp [@inlined never]) x__524_
                    in
                    fun () -> _x__525_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; conv =
                        (fun x__522_ ->
                          let _x__523_ =
                            (list_of_sexp int_of_sexp [@inlined never]) x__522_
                          in
                          fun () -> _x__523_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; conv =
                              (fun x__520_ ->
                                let _x__521_ = (unit_of_sexp [@inlined never]) x__520_ in
                                fun () -> _x__521_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; conv =
                                    (fun x__518_ ->
                                      let _x__519_ =
                                        (int_of_sexp [@inlined never]) x__518_
                                      in
                                      fun () -> _x__519_)
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
         x__526_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__528_; b = b__530_; c = c__532_; d = d__534_ } ->
       let bnds__527_ = ([] : _ Stdlib.List.t) in
       let bnds__527_ =
         match sexp_of_int d__534_ with
         | Sexplib0.Sexp.List [] -> bnds__527_
         | arg__535_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__535_ ] :: bnds__527_
            : _ Stdlib.List.t)
       in
       let bnds__527_ =
         match sexp_of_unit c__532_ with
         | Sexplib0.Sexp.List [] -> bnds__527_
         | arg__533_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__533_ ] :: bnds__527_
            : _ Stdlib.List.t)
       in
       let bnds__527_ =
         match sexp_of_list sexp_of_int b__530_ with
         | Sexplib0.Sexp.List [] -> bnds__527_
         | arg__531_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__531_ ] :: bnds__527_
            : _ Stdlib.List.t)
       in
       let bnds__527_ =
         match sexp_of_option sexp_of_int a__528_ with
         | Sexplib0.Sexp.List [] -> bnds__527_
         | arg__529_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__529_ ] :: bnds__527_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__527_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__538_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__541_) :: sexp_args__542_) as
       _sexp__540_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__542_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__539_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__538_ sexp__539_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__537_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__538_ sexp__537_
     | Sexplib0.Sexp.List [] as sexp__537_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__538_ sexp__537_
     | sexp__537_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__538_ [ "A" ] sexp__537_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__543_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__543_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__550_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__545_ as _sexp__547_ ->
       (match atom__545_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__550_ _sexp__547_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__545_ :: sexp_args__548_) as
       _sexp__547_ ->
       (match atom__545_ with
        | "A" as _tag__549_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__548_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__546_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__550_ sexp__546_
     | Sexplib0.Sexp.List [] as sexp__546_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__550_ sexp__546_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__552_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__551_ ->
       try __t_of_sexp__ sexp__551_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__552_ sexp__551_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__553_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__553_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__555_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__558_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__555_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__556_ ->
                    let _x__557_ = (int_of_sexp [@inlined never]) x__556_ in
                    fun () -> _x__557_)
              ; rest = Empty
              })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t ->
           let a = a () in
           { a })
         x__558_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__560_ } ->
       let bnds__559_ = ([] : _ Stdlib.List.t) in
       let bnds__559_ =
         let arg__561_ = sexp_of_int a__560_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__561_ ] :: bnds__559_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__559_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__563_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__563_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__564_ -> sexp_of_list (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) x__564_
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
      (let error_source__566_ = "expansion.ml.Portable.t" in
       fun x__571_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__566_
           ~fields:
             (Field
                { name = "u"
                ; kind = Required
                ; conv =
                    (fun x__569_ ->
                      let _x__570_ = (u_of_sexp int_of_sexp [@inlined never]) x__569_ in
                      fun () -> _x__570_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__567_ ->
                            let _x__568_ = (int_of_sexp [@inlined never]) x__567_ in
                            fun () -> _x__568_)
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
           x__571_
       : Sexplib0.Sexp.t -> t)

    and u_of_sexp : 'a. ((Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a u) @ portable =
      let error_source__574_ = "expansion.ml.Portable.u" in
      fun _of_a__572_ x__579_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__574_
          ~fields:
            (Field
               { name = "t"
               ; kind = Required
               ; conv =
                   (fun x__577_ ->
                     let _x__578_ = (t_of_sexp [@inlined never]) x__577_ in
                     fun () -> _x__578_)
               ; rest =
                   Field
                     { name = "a"
                     ; kind = Required
                     ; conv =
                         (fun x__575_ ->
                           let _x__576_ = (_of_a__572_ [@inlined never]) x__575_ in
                           fun () -> _x__576_)
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
          x__579_
    ;;

    let _ = t_of_sexp
    and _ = u_of_sexp
  end

  let _ @ portable = t_of_sexp
  and _ @ portable = u_of_sexp

  include struct
    let rec sexp_of_t @ portable =
      (fun { u = u__581_; b = b__583_ } ->
         let bnds__580_ = ([] : _ Stdlib.List.t) in
         let bnds__580_ =
           let arg__584_ = sexp_of_int b__583_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__584_ ] :: bnds__580_
            : _ Stdlib.List.t)
         in
         let bnds__580_ =
           let arg__582_ = sexp_of_u sexp_of_int u__581_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "u"; arg__582_ ] :: bnds__580_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__580_
       : t -> Sexplib0.Sexp.t)

    and sexp_of_u : 'a. (('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t) @ portable =
      fun _of_a__585_ { t = t__587_; a = a__589_ } ->
      let bnds__586_ = ([] : _ Stdlib.List.t) in
      let bnds__586_ =
        let arg__590_ = _of_a__585_ a__589_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__590_ ] :: bnds__586_
         : _ Stdlib.List.t)
      in
      let bnds__586_ =
        let arg__588_ = sexp_of_t t__587_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__588_ ] :: bnds__586_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__586_
    ;;

    let _ = sexp_of_t
    and _ = sexp_of_u
  end

  let _ @ portable = sexp_of_t
  and _ @ portable = sexp_of_u

  [@@@end]
end

module Local_inputs = struct
  let sexp_of_int (x : int @ local) = sexp_of_int x
  let x = (sexp_of_int : int -> Sexp.t)

  type t = int

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (sexp_of_int :> local_ t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t
  let sexp_of_t__stack = sexp_of_int__stack

  module Trivial : sig
    type t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : local_ t -> Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = int [@@deriving_inline sexp ~localize]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (sexp_of_int : local_ t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t

    [@@@end]
  end

  module Record : sig
    type t =
      { a : int
      ; b : Trivial.t
      }
    [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : local_ t -> Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t =
      { a : int
      ; b : Trivial.t
      }
    [@@deriving_inline sexp ~localize]

    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__593_ = "expansion.ml.Local_inputs.Record.t" in
       fun x__598_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__593_
           ~fields:
             (Field
                { name = "a"
                ; kind = Required
                ; conv =
                    (fun x__596_ ->
                      let _x__597_ = (int_of_sexp [@inlined never]) x__596_ in
                      fun () -> _x__597_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__594_ ->
                            let _x__595_ = (Trivial.t_of_sexp [@inlined never]) x__594_ in
                            fun () -> _x__595_)
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
           x__598_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { a = a__600_; b = b__602_ } ->
         let bnds__599_ = ([] : _ Stdlib.List.t) in
         let bnds__599_ =
           let arg__603_ = Trivial.sexp_of_t b__602_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__603_ ] :: bnds__599_
            : _ Stdlib.List.t)
         in
         let bnds__599_ =
           let arg__601_ = sexp_of_int a__600_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__601_ ] :: bnds__599_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__599_
       : local_ t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Variant : sig
    type t [@@deriving sexp ~localize]
  end = struct
    type t =
      | A of Trivial.t
      | B of
          { x : Trivial.t
          ; y : Trivial.t
          }
    [@@deriving_inline sexp ~localize]

    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__606_ = "expansion.ml.Local_inputs.Variant.t" in
       function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("a" | "A") as _tag__609_) :: sexp_args__610_) as
         _sexp__608_ ->
         (match sexp_args__610_ with
          | arg0__611_ :: [] ->
            let res0__612_ = Trivial.t_of_sexp arg0__611_ in
            A res0__612_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__606_
              _tag__609_
              _sexp__608_)
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("b" | "B") :: sexps__618_) as sexp__617_
         ->
         Sexplib0.Sexp_conv_record.record_of_sexps
           ~context:sexp__617_
           ~caller:error_source__606_
           ~fields:
             (Field
                { name = "x"
                ; kind = Required
                ; conv =
                    (fun x__615_ ->
                      let _x__616_ = (Trivial.t_of_sexp [@inlined never]) x__615_ in
                      fun () -> _x__616_)
                ; rest =
                    Field
                      { name = "y"
                      ; kind = Required
                      ; conv =
                          (fun x__613_ ->
                            let _x__614_ = (Trivial.t_of_sexp [@inlined never]) x__613_ in
                            fun () -> _x__614_)
                      ; rest = Empty
                      }
                })
           ~index_of_field:(function
             | "x" -> 0
             | "y" -> 1
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (x, (y, ())) : t ->
             let x = x () in
             let y = y () in
             B { x; y })
           sexps__618_
       | Sexplib0.Sexp.Atom ("a" | "A" | "b" | "B") as sexp__607_ ->
         Sexplib0.Sexp_conv_error.stag_takes_args error_source__606_ sexp__607_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__605_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__606_ sexp__605_
       | Sexplib0.Sexp.List [] as sexp__605_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__606_ sexp__605_
       | sexp__605_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__606_
           [ "A"; "B" ]
           sexp__605_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | A arg0__619_ ->
         let res0__620_ = Trivial.sexp_of_t arg0__619_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; res0__620_ ]
       | B { x = x__622_; y = y__624_ } ->
         let bnds__621_ = ([] : _ Stdlib.List.t) in
         let bnds__621_ =
           let arg__625_ = Trivial.sexp_of_t y__624_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "y"; arg__625_ ] :: bnds__621_
            : _ Stdlib.List.t)
         in
         let bnds__621_ =
           let arg__623_ = Trivial.sexp_of_t x__622_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__623_ ] :: bnds__621_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "B" :: bnds__621_)
       : local_ t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Parameterized : sig
    type 'a t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : (local_ 'a -> Sexplib0.Sexp.t) -> local_ 'a t -> Sexplib0.Sexp.t
      val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type 'a t = { a : 'a } [@@deriving_inline sexp ~localize]

    let _ = fun (_ : 'a t) -> ()

    let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      let error_source__628_ = "expansion.ml.Local_inputs.Parameterized.t" in
      fun _of_a__626_ x__631_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__628_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__629_ ->
                     let _x__630_ = (_of_a__626_ [@inlined never]) x__629_ in
                     fun () -> _x__630_)
               ; rest = Empty
               })
          ~index_of_field:(function
            | "a" -> 0
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, ()) : _ t ->
            let a = a () in
            { a })
          x__631_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. (local_ 'a -> Sexplib0.Sexp.t) -> local_ 'a t -> Sexplib0.Sexp.t =
      fun _of_a__632_ { a = a__634_ } ->
      let bnds__633_ = ([] : _ Stdlib.List.t) in
      let bnds__633_ =
        let arg__635_ = _of_a__632_ a__634_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__635_ ] :: bnds__633_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__633_
    ;;

    let _ = sexp_of_t

    [@@@end]
  end
end

module Local_inputs_and_stackify = struct
  module Trivial : sig
    type t [@@deriving_inline sexp ~localize ~stackify]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : local_ t -> Sexplib0.Sexp.t
      val sexp_of_t__stack : local_ t -> local_ Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = Local_inputs.t [@@deriving_inline sexp ~localize ~stackify]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (Local_inputs.t_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (Local_inputs.sexp_of_t : local_ t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t

    let sexp_of_t__stack =
      (Local_inputs.sexp_of_t__stack : local_ t -> local_ Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t__stack

    [@@@end]
  end

  module Record : sig
    type 'a t [@@deriving_inline sexp ~localize ~stackify]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : (local_ 'a -> Sexplib0.Sexp.t) -> local_ 'a t -> Sexplib0.Sexp.t

      val sexp_of_t__stack
        :  (local_ 'a -> local_ Sexplib0.Sexp.t)
        -> local_ 'a t
        -> local_ Sexplib0.Sexp.t

      val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type 'a t =
      { a : 'a
      ; x : int
      }
    [@@deriving_inline sexp ~localize ~stackify]

    let _ = fun (_ : 'a t) -> ()

    let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      let error_source__639_ = "expansion.ml.Local_inputs_and_stackify.Record.t" in
      fun _of_a__637_ x__644_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__639_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__642_ ->
                     let _x__643_ = (_of_a__637_ [@inlined never]) x__642_ in
                     fun () -> _x__643_)
               ; rest =
                   Field
                     { name = "x"
                     ; kind = Required
                     ; conv =
                         (fun x__640_ ->
                           let _x__641_ = (int_of_sexp [@inlined never]) x__640_ in
                           fun () -> _x__641_)
                     ; rest = Empty
                     }
               })
          ~index_of_field:(function
            | "a" -> 0
            | "x" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, (x, ())) : _ t ->
            let a = a () in
            let x = x () in
            { a; x })
          x__644_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. (local_ 'a -> Sexplib0.Sexp.t) -> local_ 'a t -> Sexplib0.Sexp.t =
      fun _of_a__645_ { a = a__647_; x = x__649_ } ->
      let bnds__646_ = ([] : _ Stdlib.List.t) in
      let bnds__646_ =
        let arg__650_ = sexp_of_int x__649_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__650_ ] :: bnds__646_
         : _ Stdlib.List.t)
      in
      let bnds__646_ =
        let arg__648_ = _of_a__645_ a__647_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__648_ ] :: bnds__646_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__646_
    ;;

    let _ = sexp_of_t

    let sexp_of_t__stack
      : 'a. (local_ 'a -> local_ Sexplib0.Sexp.t) -> local_ 'a t -> local_ Sexplib0.Sexp.t
      =
      fun _of_a__651_ { a = a__653_; x = x__655_ } -> exclave_
      let bnds__652_ = ([] : _ Stdlib.List.t) in
      let bnds__652_ =
        let arg__656_ = sexp_of_int__stack x__655_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__656_ ] :: bnds__652_
         : _ Stdlib.List.t)
      in
      let bnds__652_ =
        let arg__654_ = _of_a__651_ a__653_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__654_ ] :: bnds__652_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__652_
    ;;

    let _ = sexp_of_t__stack

    [@@@end]
  end

  module Use_these_inside_non_local_marked_types : sig
    type t [@@deriving sexp]
    type u [@@deriving sexp]
  end = struct
    type t = Trivial.t [@@deriving_inline sexp]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (Trivial.t_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (Trivial.sexp_of_t : t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t

    [@@@end]

    type u = Trivial.t List.t [@@deriving_inline sexp]

    let _ = fun (_ : u) -> ()

    let u_of_sexp =
      (fun x__659_ -> List.t_of_sexp Trivial.t_of_sexp x__659_ : Sexplib0.Sexp.t -> u)
    ;;

    let _ = u_of_sexp

    let sexp_of_u =
      (fun x__660_ -> List.sexp_of_t Trivial.sexp_of_t x__660_ : u -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_u

    [@@@end]
  end
end
