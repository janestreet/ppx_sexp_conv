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
  let sexp_of_t__stack = (fun _ -> assert false : t -> Sexplib0.Sexp.t)
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
    (fun (arg0__019_, arg1__020_, arg2__021_) ->
       let res0__022_ = sexp_of_int__stack arg0__019_
       and res1__023_ = sexp_of_int__stack arg1__020_
       and res2__024_ = sexp_of_int__stack arg2__021_ in
       Sexplib0.Sexp.List [ res0__022_; res1__023_; res2__024_ ]
     : t -> Sexplib0.Sexp.t)
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
    (fun { a = a__042_; b = b__044_; c = c__046_ } ->
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
     : t -> Sexplib0.Sexp.t)
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
    (fun { a = a__065_; b = b__067_; c = c__069_ } ->
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
     : t -> Sexplib0.Sexp.t)
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
     | A -> Sexplib0.Sexp.Atom "A"
     | B (arg0__116_, arg1__117_) ->
       let res0__118_ = sexp_of_int__stack arg0__116_
       and res1__119_ = sexp_of_int__stack arg1__117_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__118_; res1__119_ ]
     | C { a = a__121_; b = b__123_; d = d__125_ } ->
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
     : t -> Sexplib0.Sexp.t)
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
     | `A -> Sexplib0.Sexp.Atom "A"
     | `B v__146_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int__stack v__146_ ]
     : t -> Sexplib0.Sexp.t)
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
    ; b : int [@default 0] [@sexp_drop_default.compare.local]
    ; c : int [@default 0] [@sexp_drop_default.equal.local]
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
                    let _x__338_ = (int_of_sexp [@inlined never]) x__337_ in
                    fun () -> _x__338_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__323_)
                    ; conv =
                        (fun x__335_ ->
                          let _x__336_ = (int_of_sexp [@inlined never]) x__335_ in
                          fun () -> _x__336_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__324_)
                          ; conv =
                              (fun x__333_ ->
                                let _x__334_ = (int_of_sexp [@inlined never]) x__333_ in
                                fun () -> _x__334_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__325_)
                                ; conv =
                                    (fun x__331_ ->
                                      let _x__332_ =
                                        (int_of_sexp [@inlined never]) x__331_
                                      in
                                      fun () -> _x__332_)
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__326_)
                                      ; conv =
                                          (fun x__329_ ->
                                            let _x__330_ =
                                              (int_of_sexp [@inlined never]) x__329_
                                            in
                                            fun () -> _x__330_)
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv =
                                                (fun x__327_ ->
                                                  let _x__328_ =
                                                    (int_of_sexp [@inlined never]) x__327_
                                                  in
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
         if [%equal__local: int] default__349_ c__350_
         then bnds__340_
         else (
           let arg__352_ = sexp_of_int c__350_ in
           let bnd__351_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__352_ ] in
           (bnd__351_ :: bnds__340_ : _ Stdlib.List.t))
       in
       let bnds__340_ =
         if [%compare.equal__local: int] default__344_ b__345_
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
    (let default__381_ : string = ""
     and default__380_ : string = ""
     and default__379_ : string = ""
     and default__378_ : string = ""
     and default__377_ : string = "" in
     let error_source__376_ = "expansion.ml.Record_with_defaults_and_stackify.t" in
     fun x__394_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__376_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__377_)
              ; conv =
                  (fun x__392_ ->
                    let _x__393_ = (string_of_sexp [@inlined never]) x__392_ in
                    fun () -> _x__393_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__378_)
                    ; conv =
                        (fun x__390_ ->
                          let _x__391_ = (string_of_sexp [@inlined never]) x__390_ in
                          fun () -> _x__391_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__379_)
                          ; conv =
                              (fun x__388_ ->
                                let _x__389_ =
                                  (string_of_sexp [@inlined never]) x__388_
                                in
                                fun () -> _x__389_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__380_)
                                ; conv =
                                    (fun x__386_ ->
                                      let _x__387_ =
                                        (string_of_sexp [@inlined never]) x__386_
                                      in
                                      fun () -> _x__387_)
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__381_)
                                      ; conv =
                                          (fun x__384_ ->
                                            let _x__385_ =
                                              (string_of_sexp [@inlined never]) x__384_
                                            in
                                            fun () -> _x__385_)
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; conv =
                                                (fun x__382_ ->
                                                  let _x__383_ =
                                                    (string_of_sexp [@inlined never])
                                                      x__382_
                                                  in
                                                  fun () -> _x__383_)
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
         x__394_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__399_ : string = ""
     and default__404_ : string = ""
     and default__409_ : string = ""
     and default__415_ : string = ""
     and drop_default__414_ : string -> string -> Stdlib.Bool.t = String.equal__local
     and drop_if__420_ : string -> Stdlib.Bool.t = fun s -> String.equal__local "" s in
     fun { a = a__396_; b = b__400_; c = c__405_; d = d__410_; e = e__416_; f = f__421_ } ->
       let bnds__395_ = ([] : _ Stdlib.List.t) in
       let bnds__395_ =
         if drop_if__420_ f__421_
         then bnds__395_
         else (
           let arg__423_ = sexp_of_string f__421_ in
           let bnd__422_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__423_ ] in
           (bnd__422_ :: bnds__395_ : _ Stdlib.List.t))
       in
       let bnds__395_ =
         if drop_default__414_ default__415_ e__416_
         then bnds__395_
         else (
           let arg__418_ = sexp_of_string e__416_ in
           let bnd__417_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__418_ ] in
           (bnd__417_ :: bnds__395_ : _ Stdlib.List.t))
       in
       let bnds__395_ =
         let arg__412_ = sexp_of_string d__410_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_string default__409_) arg__412_
         then bnds__395_
         else (
           let bnd__411_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__412_ ] in
           (bnd__411_ :: bnds__395_ : _ Stdlib.List.t))
       in
       let bnds__395_ =
         if [%equal__local: string] default__404_ c__405_
         then bnds__395_
         else (
           let arg__407_ = sexp_of_string c__405_ in
           let bnd__406_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__407_ ] in
           (bnd__406_ :: bnds__395_ : _ Stdlib.List.t))
       in
       let bnds__395_ =
         if [%compare.equal__local: string] default__399_ b__400_
         then bnds__395_
         else (
           let arg__402_ = sexp_of_string b__400_ in
           let bnd__401_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__402_ ] in
           (bnd__401_ :: bnds__395_ : _ Stdlib.List.t))
       in
       let bnds__395_ =
         let arg__397_ = sexp_of_string a__396_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__397_ ] :: bnds__395_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__395_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (let default__428_ : string = ""
     and default__433_ : string = ""
     and default__438_ : string = ""
     and default__444_ : string = ""
     and drop_default__443_ : string -> string -> Stdlib.Bool.t = String.equal__local
     and drop_if__449_ : string -> Stdlib.Bool.t = fun s -> String.equal__local "" s in
     fun { a = a__425_; b = b__429_; c = c__434_; d = d__439_; e = e__445_; f = f__450_ } ->
       let bnds__424_ = ([] : _ Stdlib.List.t) in
       let bnds__424_ =
         if drop_if__449_ f__450_
         then bnds__424_
         else (
           let arg__452_ = sexp_of_string__stack f__450_ in
           let bnd__451_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__452_ ] in
           (bnd__451_ :: bnds__424_ : _ Stdlib.List.t))
       in
       let bnds__424_ =
         if drop_default__443_ default__444_ e__445_
         then bnds__424_
         else (
           let arg__447_ = sexp_of_string__stack e__445_ in
           let bnd__446_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__447_ ] in
           (bnd__446_ :: bnds__424_ : _ Stdlib.List.t))
       in
       let bnds__424_ =
         let arg__441_ = sexp_of_string__stack d__439_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_string__stack default__438_) arg__441_
         then bnds__424_
         else (
           let bnd__440_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__441_ ] in
           (bnd__440_ :: bnds__424_ : _ Stdlib.List.t))
       in
       let bnds__424_ =
         if [%equal__local: string] default__433_ c__434_
         then bnds__424_
         else (
           let arg__436_ = sexp_of_string__stack c__434_ in
           let bnd__435_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__436_ ] in
           (bnd__435_ :: bnds__424_ : _ Stdlib.List.t))
       in
       let bnds__424_ =
         if [%compare.equal__local: string] default__428_ b__429_
         then bnds__424_
         else (
           let arg__431_ = sexp_of_string__stack b__429_ in
           let bnd__430_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__431_ ] in
           (bnd__430_ :: bnds__424_ : _ Stdlib.List.t))
       in
       let bnds__424_ =
         let arg__426_ = sexp_of_string__stack a__425_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__426_ ] :: bnds__424_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__424_
     : t -> Sexplib0.Sexp.t)
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
    (let default__468_ : string = ""
     and default__467_ : string = "" in
     let error_source__466_ = "expansion.ml.Record_with_explicit_local_defaults.t" in
     fun x__473_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__466_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__467_)
              ; conv =
                  (fun x__471_ ->
                    let _x__472_ = (string_of_sexp [@inlined never]) x__471_ in
                    fun () -> _x__472_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__468_)
                    ; conv =
                        (fun x__469_ ->
                          let _x__470_ = (string_of_sexp [@inlined never]) x__469_ in
                          fun () -> _x__470_)
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
         x__473_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__476_ : string = ""
     and default__481_ : string = "" in
     fun { a = a__477_; b = b__482_ } ->
       let bnds__474_ = ([] : _ Stdlib.List.t) in
       let bnds__474_ =
         if [%equal__local: string] default__481_ b__482_
         then bnds__474_
         else (
           let arg__484_ = sexp_of_string b__482_ in
           let bnd__483_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__484_ ] in
           (bnd__483_ :: bnds__474_ : _ Stdlib.List.t))
       in
       let bnds__474_ =
         if [%compare.equal__local: string] default__476_ a__477_
         then bnds__474_
         else (
           let arg__479_ = sexp_of_string a__477_ in
           let bnd__478_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__479_ ] in
           (bnd__478_ :: bnds__474_ : _ Stdlib.List.t))
       in
       Sexplib0.Sexp.List bnds__474_
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
    (let error_source__492_ = "expansion.ml.Record_with_special_types.t" in
     fun x__493_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__492_
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
         x__493_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__495_; b = b__500_; c = c__504_; d = d__507_; e = e__509_ } ->
       let bnds__494_ = ([] : _ Stdlib.List.t) in
       let bnds__494_ =
         match e__509_ with
         | Ppx_sexp_conv_lib.Or_null.Null -> bnds__494_
         | Ppx_sexp_conv_lib.Or_null.This v__510_ ->
           let arg__512_ = sexp_of_int v__510_ in
           let bnd__511_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__512_ ] in
           (bnd__511_ :: bnds__494_ : _ Stdlib.List.t)
       in
       let bnds__494_ =
         if d__507_
         then (
           let bnd__508_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__508_ :: bnds__494_ : _ Stdlib.List.t))
         else bnds__494_
       in
       let bnds__494_ =
         if match c__504_ with
            | [||] -> true
            | _ -> false
         then bnds__494_
         else (
           let arg__506_ = (sexp_of_array sexp_of_int) c__504_ in
           let bnd__505_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__506_ ] in
           (bnd__505_ :: bnds__494_ : _ Stdlib.List.t))
       in
       let bnds__494_ =
         if match b__500_ with
            | [] -> true
            | _ -> false
         then bnds__494_
         else (
           let arg__502_ = (sexp_of_list sexp_of_int) b__500_ in
           let bnd__501_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__502_ ] in
           (bnd__501_ :: bnds__494_ : _ Stdlib.List.t))
       in
       let bnds__494_ =
         match a__495_ with
         | Stdlib.Option.None -> bnds__494_
         | Stdlib.Option.Some v__496_ ->
           let arg__498_ = sexp_of_int v__496_ in
           let bnd__497_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__498_ ] in
           (bnd__497_ :: bnds__494_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__494_
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
    (let error_source__514_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__523_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__514_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; conv =
                  (fun x__521_ ->
                    let _x__522_ =
                      (option_of_sexp int_of_sexp [@inlined never]) x__521_
                    in
                    fun () -> _x__522_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; conv =
                        (fun x__519_ ->
                          let _x__520_ =
                            (list_of_sexp int_of_sexp [@inlined never]) x__519_
                          in
                          fun () -> _x__520_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; conv =
                              (fun x__517_ ->
                                let _x__518_ = (unit_of_sexp [@inlined never]) x__517_ in
                                fun () -> _x__518_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; conv =
                                    (fun x__515_ ->
                                      let _x__516_ =
                                        (int_of_sexp [@inlined never]) x__515_
                                      in
                                      fun () -> _x__516_)
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
         x__523_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__525_; b = b__527_; c = c__529_; d = d__531_ } ->
       let bnds__524_ = ([] : _ Stdlib.List.t) in
       let bnds__524_ =
         match sexp_of_int d__531_ with
         | Sexplib0.Sexp.List [] -> bnds__524_
         | arg__532_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__532_ ] :: bnds__524_
            : _ Stdlib.List.t)
       in
       let bnds__524_ =
         match sexp_of_unit c__529_ with
         | Sexplib0.Sexp.List [] -> bnds__524_
         | arg__530_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__530_ ] :: bnds__524_
            : _ Stdlib.List.t)
       in
       let bnds__524_ =
         match sexp_of_list sexp_of_int b__527_ with
         | Sexplib0.Sexp.List [] -> bnds__524_
         | arg__528_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__528_ ] :: bnds__524_
            : _ Stdlib.List.t)
       in
       let bnds__524_ =
         match sexp_of_option sexp_of_int a__525_ with
         | Sexplib0.Sexp.List [] -> bnds__524_
         | arg__526_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__526_ ] :: bnds__524_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__524_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__535_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__538_) :: sexp_args__539_) as
       _sexp__537_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__539_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__536_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__535_ sexp__536_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__534_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__535_ sexp__534_
     | Sexplib0.Sexp.List [] as sexp__534_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__535_ sexp__534_
     | sexp__534_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__535_ [ "A" ] sexp__534_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__540_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__540_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__547_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__542_ as _sexp__544_ ->
       (match atom__542_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__547_ _sexp__544_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__542_ :: sexp_args__545_) as
       _sexp__544_ ->
       (match atom__542_ with
        | "A" as _tag__546_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__545_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__543_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__547_ sexp__543_
     | Sexplib0.Sexp.List [] as sexp__543_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__547_ sexp__543_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__549_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__548_ ->
       try __t_of_sexp__ sexp__548_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__549_ sexp__548_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__550_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__550_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__552_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__555_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__552_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__553_ ->
                    let _x__554_ = (int_of_sexp [@inlined never]) x__553_ in
                    fun () -> _x__554_)
              ; rest = Empty
              })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t ->
           let a = a () in
           { a })
         x__555_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__557_ } ->
       let bnds__556_ = ([] : _ Stdlib.List.t) in
       let bnds__556_ =
         let arg__558_ = sexp_of_int a__557_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__558_ ] :: bnds__556_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__556_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__560_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__560_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__561_ -> sexp_of_list (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) x__561_
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
    let rec t_of_sexp =
      (let error_source__563_ = "expansion.ml.Portable.t" in
       fun x__568_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__563_
           ~fields:
             (Field
                { name = "u"
                ; kind = Required
                ; conv =
                    (fun x__566_ ->
                      let _x__567_ = (u_of_sexp int_of_sexp [@inlined never]) x__566_ in
                      fun () -> _x__567_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__564_ ->
                            let _x__565_ = (int_of_sexp [@inlined never]) x__564_ in
                            fun () -> _x__565_)
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
           x__568_
       : Sexplib0.Sexp.t -> t)

    and u_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a u =
      let error_source__571_ = "expansion.ml.Portable.u" in
      fun _of_a__569_ x__576_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__571_
          ~fields:
            (Field
               { name = "t"
               ; kind = Required
               ; conv =
                   (fun x__574_ ->
                     let _x__575_ = (t_of_sexp [@inlined never]) x__574_ in
                     fun () -> _x__575_)
               ; rest =
                   Field
                     { name = "a"
                     ; kind = Required
                     ; conv =
                         (fun x__572_ ->
                           let _x__573_ = (_of_a__569_ [@inlined never]) x__572_ in
                           fun () -> _x__573_)
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
          x__576_
    ;;

    let _ = t_of_sexp
    and _ = u_of_sexp
  end

  let _ = t_of_sexp
  and _ = u_of_sexp

  include struct
    let rec sexp_of_t =
      (fun { u = u__578_; b = b__580_ } ->
         let bnds__577_ = ([] : _ Stdlib.List.t) in
         let bnds__577_ =
           let arg__581_ = sexp_of_int b__580_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__581_ ] :: bnds__577_
            : _ Stdlib.List.t)
         in
         let bnds__577_ =
           let arg__579_ = sexp_of_u sexp_of_int u__578_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "u"; arg__579_ ] :: bnds__577_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__577_
       : t -> Sexplib0.Sexp.t)

    and sexp_of_u : 'a. ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t =
      fun _of_a__582_ { t = t__584_; a = a__586_ } ->
      let bnds__583_ = ([] : _ Stdlib.List.t) in
      let bnds__583_ =
        let arg__587_ = _of_a__582_ a__586_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__587_ ] :: bnds__583_
         : _ Stdlib.List.t)
      in
      let bnds__583_ =
        let arg__585_ = sexp_of_t t__584_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__585_ ] :: bnds__583_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__583_
    ;;

    let _ = sexp_of_t
    and _ = sexp_of_u
  end

  let _ = sexp_of_t
  and _ = sexp_of_u

  [@@@end]
end

module Local_inputs = struct
  let sexp_of_int (x : int) = sexp_of_int x
  let x = (sexp_of_int : int -> Sexp.t)

  type t = int

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (sexp_of_int :> t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t
  let sexp_of_t__stack = sexp_of_int__stack

  module Trivial : sig
    type t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : t -> Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = int [@@deriving_inline sexp ~localize]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (sexp_of_int : t -> Sexplib0.Sexp.t)
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

      val sexp_of_t : t -> Sexplib0.Sexp.t
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
      (let error_source__590_ = "expansion.ml.Local_inputs.Record.t" in
       fun x__595_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__590_
           ~fields:
             (Field
                { name = "a"
                ; kind = Required
                ; conv =
                    (fun x__593_ ->
                      let _x__594_ = (int_of_sexp [@inlined never]) x__593_ in
                      fun () -> _x__594_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__591_ ->
                            let _x__592_ = (Trivial.t_of_sexp [@inlined never]) x__591_ in
                            fun () -> _x__592_)
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
           x__595_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { a = a__597_; b = b__599_ } ->
         let bnds__596_ = ([] : _ Stdlib.List.t) in
         let bnds__596_ =
           let arg__600_ = Trivial.sexp_of_t b__599_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__600_ ] :: bnds__596_
            : _ Stdlib.List.t)
         in
         let bnds__596_ =
           let arg__598_ = sexp_of_int a__597_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__598_ ] :: bnds__596_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__596_
       : t -> Sexplib0.Sexp.t)
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
      (let error_source__603_ = "expansion.ml.Local_inputs.Variant.t" in
       function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("a" | "A") as _tag__606_) :: sexp_args__607_) as
         _sexp__605_ ->
         (match sexp_args__607_ with
          | arg0__608_ :: [] ->
            let res0__609_ = Trivial.t_of_sexp arg0__608_ in
            A res0__609_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__603_
              _tag__606_
              _sexp__605_)
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("b" | "B") :: sexps__615_) as sexp__614_
         ->
         Sexplib0.Sexp_conv_record.record_of_sexps
           ~context:sexp__614_
           ~caller:error_source__603_
           ~fields:
             (Field
                { name = "x"
                ; kind = Required
                ; conv =
                    (fun x__612_ ->
                      let _x__613_ = (Trivial.t_of_sexp [@inlined never]) x__612_ in
                      fun () -> _x__613_)
                ; rest =
                    Field
                      { name = "y"
                      ; kind = Required
                      ; conv =
                          (fun x__610_ ->
                            let _x__611_ = (Trivial.t_of_sexp [@inlined never]) x__610_ in
                            fun () -> _x__611_)
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
           sexps__615_
       | Sexplib0.Sexp.Atom ("a" | "A" | "b" | "B") as sexp__604_ ->
         Sexplib0.Sexp_conv_error.stag_takes_args error_source__603_ sexp__604_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__602_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__603_ sexp__602_
       | Sexplib0.Sexp.List [] as sexp__602_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__603_ sexp__602_
       | sexp__602_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__603_
           [ "A"; "B" ]
           sexp__602_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | A arg0__616_ ->
         let res0__617_ = Trivial.sexp_of_t arg0__616_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; res0__617_ ]
       | B { x = x__619_; y = y__621_ } ->
         let bnds__618_ = ([] : _ Stdlib.List.t) in
         let bnds__618_ =
           let arg__622_ = Trivial.sexp_of_t y__621_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "y"; arg__622_ ] :: bnds__618_
            : _ Stdlib.List.t)
         in
         let bnds__618_ =
           let arg__620_ = Trivial.sexp_of_t x__619_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__620_ ] :: bnds__618_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "B" :: bnds__618_)
       : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Parameterized : sig
    type 'a t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
      val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type 'a t = { a : 'a } [@@deriving_inline sexp ~localize]

    let _ = fun (_ : 'a t) -> ()

    let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      let error_source__625_ = "expansion.ml.Local_inputs.Parameterized.t" in
      fun _of_a__623_ x__628_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__625_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__626_ ->
                     let _x__627_ = (_of_a__623_ [@inlined never]) x__626_ in
                     fun () -> _x__627_)
               ; rest = Empty
               })
          ~index_of_field:(function
            | "a" -> 0
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, ()) : _ t ->
            let a = a () in
            { a })
          x__628_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
      fun _of_a__629_ { a = a__631_ } ->
      let bnds__630_ = ([] : _ Stdlib.List.t) in
      let bnds__630_ =
        let arg__632_ = _of_a__629_ a__631_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__632_ ] :: bnds__630_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__630_
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

      val sexp_of_t : t -> Sexplib0.Sexp.t
      val sexp_of_t__stack : t -> Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = Local_inputs.t [@@deriving_inline sexp ~localize ~stackify]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (Local_inputs.t_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (Local_inputs.sexp_of_t : t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t
    let sexp_of_t__stack = (Local_inputs.sexp_of_t__stack : t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t__stack

    [@@@end]
  end

  module Record : sig
    type 'a t [@@deriving_inline sexp ~localize ~stackify]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
      val sexp_of_t__stack : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
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
      let error_source__636_ = "expansion.ml.Local_inputs_and_stackify.Record.t" in
      fun _of_a__634_ x__641_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__636_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__639_ ->
                     let _x__640_ = (_of_a__634_ [@inlined never]) x__639_ in
                     fun () -> _x__640_)
               ; rest =
                   Field
                     { name = "x"
                     ; kind = Required
                     ; conv =
                         (fun x__637_ ->
                           let _x__638_ = (int_of_sexp [@inlined never]) x__637_ in
                           fun () -> _x__638_)
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
          x__641_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
      fun _of_a__642_ { a = a__644_; x = x__646_ } ->
      let bnds__643_ = ([] : _ Stdlib.List.t) in
      let bnds__643_ =
        let arg__647_ = sexp_of_int x__646_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__647_ ] :: bnds__643_
         : _ Stdlib.List.t)
      in
      let bnds__643_ =
        let arg__645_ = _of_a__642_ a__644_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__645_ ] :: bnds__643_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__643_
    ;;

    let _ = sexp_of_t

    let sexp_of_t__stack : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
      fun _of_a__648_ { a = a__650_; x = x__652_ } ->
      let bnds__649_ = ([] : _ Stdlib.List.t) in
      let bnds__649_ =
        let arg__653_ = sexp_of_int__stack x__652_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__653_ ] :: bnds__649_
         : _ Stdlib.List.t)
      in
      let bnds__649_ =
        let arg__651_ = _of_a__648_ a__650_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__651_ ] :: bnds__649_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__649_
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
      (fun x__656_ -> List.t_of_sexp Trivial.t_of_sexp x__656_ : Sexplib0.Sexp.t -> u)
    ;;

    let _ = u_of_sexp

    let sexp_of_u =
      (fun x__657_ -> List.sexp_of_t Trivial.sexp_of_t x__657_ : u -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_u

    [@@@end]
  end
end
