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
    (fun _ -> exclave_ assert false : t @ local -> Sexplib0.Sexp.t @ local)
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
           let _of_c__322_ _ = Sexplib0.Sexp_conv.sexp_of_opaque () in
           _of_c__322_ c__320_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__321_ ] :: bnds__313_
          : _ Stdlib.List.t)
       in
       let bnds__313_ =
         let arg__318_ =
           let _of_b__319_ _ = Sexplib0.Sexp_conv.sexp_of_opaque () in
           sexp_of_option _of_b__319_ b__317_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__318_ ] :: bnds__313_
          : _ Stdlib.List.t)
       in
       let bnds__313_ =
         let arg__315_ =
           let _of_a__316_ _ = Sexplib0.Sexp_conv.sexp_of_opaque () in
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
    (let default__325_ : int = 0
     and default__326_ : int = 0
     and default__327_ : int = 0
     and default__328_ : int = 0
     and default__329_ : int = 0 in
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
    (let default__380_ : string = ""
     and default__381_ : string = ""
     and default__382_ : string = ""
     and default__383_ : string = ""
     and default__384_ : string = "" in
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
     and drop_default__417_ : string @ local -> string @ local -> Stdlib.Bool.t =
       String.equal__local
     and drop_if__423_ : string @ local -> Stdlib.Bool.t =
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
     and drop_default__446_ : string @ local -> string @ local -> Stdlib.Bool.t =
       String.equal__local
     and drop_if__452_ : string @ local -> Stdlib.Bool.t =
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
     : t @ local -> Sexplib0.Sexp.t @ local)
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
    (let default__470_ : string = ""
     and default__471_ : string = "" in
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
    (let _must_type_check__496_ : _ =
       fun (a : int option) ->
       let _ : _ =
         (a
          : int Stdlib.Option.t)
          [@error_message "ppx_sexp_conv: expected [_ option] or equivalent"]
       in
       ()
     and _must_type_check__497_ : _ =
       fun (b : int list) ->
       let _ : _ =
         (b
          : int Stdlib.List.t)
          [@error_message "ppx_sexp_conv: expected [_ list] or equivalent"]
       in
       ()
     and _must_type_check__498_ : _ =
       fun (c : int array) ->
       let _ : _ =
         (c
          : int Stdlib.Array.t)
          [@error_message "ppx_sexp_conv: expected [_ array] or equivalent"]
       in
       ()
     and _must_type_check__499_ : _ =
       fun (d : bool) ->
       let _ : _ =
         (d
          : Stdlib.Bool.t)
          [@error_message "ppx_sexp_conv: expected [bool] or equivalent"]
       in
       ()
     and _must_type_check__500_ : _ =
       fun (e : int or_null) ->
       let _ : _ =
         (e
          : int Basement.Or_null_shim.t)
          [@error_message "ppx_sexp_conv: expected [_ or_null] or equivalent"]
       in
       ()
     in
     let error_source__495_ = "expansion.ml.Record_with_special_types.t" in
     fun x__501_ ->
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
         x__501_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let _must_type_check__504_ : _ =
       fun (a : int option) ->
       let _ : _ =
         (a
          : int Stdlib.Option.t)
          [@error_message "ppx_sexp_conv: expected [_ option] or equivalent"]
       in
       ()
     and _must_type_check__509_ : _ =
       fun (b : int list) ->
       let _ : _ =
         (b
          : int Stdlib.List.t)
          [@error_message "ppx_sexp_conv: expected [_ list] or equivalent"]
       in
       ()
     and _must_type_check__514_ : _ =
       fun (c : int array) ->
       let _ : _ =
         (c
          : int Stdlib.Array.t)
          [@error_message "ppx_sexp_conv: expected [_ array] or equivalent"]
       in
       ()
     and _must_type_check__519_ : _ =
       fun (d : bool) ->
       let _ : _ =
         (d
          : Stdlib.Bool.t)
          [@error_message "ppx_sexp_conv: expected [bool] or equivalent"]
       in
       ()
     and _must_type_check__522_ : _ =
       fun (e : int or_null) ->
       let _ : _ =
         (e
          : int Basement.Or_null_shim.t)
          [@error_message "ppx_sexp_conv: expected [_ or_null] or equivalent"]
       in
       ()
     in
     fun { a = a__503_; b = b__510_; c = c__515_; d = d__518_; e = e__521_ } ->
       let bnds__502_ = ([] : _ Stdlib.List.t) in
       let bnds__502_ =
         match e__521_ with
         | Ppx_sexp_conv_lib.Or_null.Null -> bnds__502_
         | Ppx_sexp_conv_lib.Or_null.This v__523_ ->
           let arg__525_ = sexp_of_int v__523_ in
           let bnd__524_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__525_ ] in
           (bnd__524_ :: bnds__502_ : _ Stdlib.List.t)
       in
       let bnds__502_ =
         if d__518_
         then (
           let bnd__520_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__520_ :: bnds__502_ : _ Stdlib.List.t))
         else bnds__502_
       in
       let bnds__502_ =
         if match c__515_ with
            | [||] -> true
            | _ -> false
         then bnds__502_
         else (
           let arg__517_ = (sexp_of_array sexp_of_int) c__515_ in
           let bnd__516_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__517_ ] in
           (bnd__516_ :: bnds__502_ : _ Stdlib.List.t))
       in
       let bnds__502_ =
         if match b__510_ with
            | [] -> true
            | _ -> false
         then bnds__502_
         else (
           let arg__512_ = (sexp_of_list sexp_of_int) b__510_ in
           let bnd__511_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__512_ ] in
           (bnd__511_ :: bnds__502_ : _ Stdlib.List.t))
       in
       let bnds__502_ =
         match a__503_ with
         | Stdlib.Option.None -> bnds__502_
         | Stdlib.Option.Some v__505_ ->
           let arg__507_ = sexp_of_int v__505_ in
           let bnd__506_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__507_ ] in
           (bnd__506_ :: bnds__502_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__502_
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
    (let error_source__527_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__536_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__527_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; conv =
                  (fun x__534_ ->
                    let _x__535_ =
                      (option_of_sexp int_of_sexp [@inlined never]) x__534_
                    in
                    fun () -> _x__535_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; conv =
                        (fun x__532_ ->
                          let _x__533_ =
                            (list_of_sexp int_of_sexp [@inlined never]) x__532_
                          in
                          fun () -> _x__533_)
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; conv =
                              (fun x__530_ ->
                                let _x__531_ = (unit_of_sexp [@inlined never]) x__530_ in
                                fun () -> _x__531_)
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; conv =
                                    (fun x__528_ ->
                                      let _x__529_ =
                                        (int_of_sexp [@inlined never]) x__528_
                                      in
                                      fun () -> _x__529_)
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
         x__536_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__538_; b = b__540_; c = c__542_; d = d__544_ } ->
       let bnds__537_ = ([] : _ Stdlib.List.t) in
       let bnds__537_ =
         match sexp_of_int d__544_ with
         | Sexplib0.Sexp.List [] -> bnds__537_
         | arg__545_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__545_ ] :: bnds__537_
            : _ Stdlib.List.t)
       in
       let bnds__537_ =
         match sexp_of_unit c__542_ with
         | Sexplib0.Sexp.List [] -> bnds__537_
         | arg__543_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__543_ ] :: bnds__537_
            : _ Stdlib.List.t)
       in
       let bnds__537_ =
         match sexp_of_list sexp_of_int b__540_ with
         | Sexplib0.Sexp.List [] -> bnds__537_
         | arg__541_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__541_ ] :: bnds__537_
            : _ Stdlib.List.t)
       in
       let bnds__537_ =
         match sexp_of_option sexp_of_int a__538_ with
         | Sexplib0.Sexp.List [] -> bnds__537_
         | arg__539_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__539_ ] :: bnds__537_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__537_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__548_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__551_) :: sexp_args__552_) as
       _sexp__550_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__552_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__549_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__548_ sexp__549_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__547_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__548_ sexp__547_
     | Sexplib0.Sexp.List [] as sexp__547_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__548_ sexp__547_
     | sexp__547_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__548_ [ "A" ] sexp__547_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__553_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__553_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__560_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__555_ as _sexp__557_ ->
       (match atom__555_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__560_ _sexp__557_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__555_ :: sexp_args__558_) as
       _sexp__557_ ->
       (match atom__555_ with
        | "A" as _tag__559_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__558_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__556_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__560_ sexp__556_
     | Sexplib0.Sexp.List [] as sexp__556_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__560_ sexp__556_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__562_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__561_ ->
       try __t_of_sexp__ sexp__561_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__562_ sexp__561_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__563_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__563_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__565_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__568_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__565_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; conv =
                  (fun x__566_ ->
                    let _x__567_ = (int_of_sexp [@inlined never]) x__566_ in
                    fun () -> _x__567_)
              ; rest = Empty
              })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t ->
           let a = a () in
           { a })
         x__568_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__570_ } ->
       let bnds__569_ = ([] : _ Stdlib.List.t) in
       let bnds__569_ =
         let arg__571_ = sexp_of_int a__570_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__571_ ] :: bnds__569_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__569_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__573_ ->
       list_of_sexp
         (fun sexp ->
           match Sexplib0.Sexp_conv.opaque_of_sexp sexp with
           | (_ : Basement.nothing) -> .)
         x__573_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__574_ -> sexp_of_list (fun _ -> Sexplib0.Sexp_conv.sexp_of_opaque ()) x__574_
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
      (let error_source__576_ = "expansion.ml.Portable.t" in
       fun x__581_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__576_
           ~fields:
             (Field
                { name = "u"
                ; kind = Required
                ; conv =
                    (fun x__579_ ->
                      let _x__580_ = (u_of_sexp int_of_sexp [@inlined never]) x__579_ in
                      fun () -> _x__580_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__577_ ->
                            let _x__578_ = (int_of_sexp [@inlined never]) x__577_ in
                            fun () -> _x__578_)
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
           x__581_
       : Sexplib0.Sexp.t -> t)

    and u_of_sexp : 'a. ((Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a u) @ portable =
      let error_source__584_ = "expansion.ml.Portable.u" in
      fun _of_a__582_ x__589_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__584_
          ~fields:
            (Field
               { name = "t"
               ; kind = Required
               ; conv =
                   (fun x__587_ ->
                     let _x__588_ = (t_of_sexp [@inlined never]) x__587_ in
                     fun () -> _x__588_)
               ; rest =
                   Field
                     { name = "a"
                     ; kind = Required
                     ; conv =
                         (fun x__585_ ->
                           let _x__586_ = (_of_a__582_ [@inlined never]) x__585_ in
                           fun () -> _x__586_)
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
          x__589_
    ;;

    let _ = t_of_sexp
    and _ = u_of_sexp
  end

  let _ @ portable = t_of_sexp
  and _ @ portable = u_of_sexp

  include struct
    let rec sexp_of_t @ portable =
      (fun { u = u__591_; b = b__593_ } ->
         let bnds__590_ = ([] : _ Stdlib.List.t) in
         let bnds__590_ =
           let arg__594_ = sexp_of_int b__593_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__594_ ] :: bnds__590_
            : _ Stdlib.List.t)
         in
         let bnds__590_ =
           let arg__592_ = sexp_of_u sexp_of_int u__591_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "u"; arg__592_ ] :: bnds__590_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__590_
       : t -> Sexplib0.Sexp.t)

    and sexp_of_u : 'a. (('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t) @ portable =
      fun _of_a__595_ { t = t__597_; a = a__599_ } ->
      let bnds__596_ = ([] : _ Stdlib.List.t) in
      let bnds__596_ =
        let arg__600_ = _of_a__595_ a__599_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__600_ ] :: bnds__596_
         : _ Stdlib.List.t)
      in
      let bnds__596_ =
        let arg__598_ = sexp_of_t t__597_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__598_ ] :: bnds__596_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__596_
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
  let sexp_of_t = (sexp_of_int :> t @ local -> Sexplib0.Sexp.t)
  let _ = sexp_of_t
  let sexp_of_t__stack = sexp_of_int__stack

  module Trivial : sig
    type t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : t @ local -> Sexplib0.Sexp.t
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = int [@@deriving_inline sexp ~localize]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (int_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (sexp_of_int : t @ local -> Sexplib0.Sexp.t)
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

      val sexp_of_t : t @ local -> Sexplib0.Sexp.t
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
      (let error_source__603_ = "expansion.ml.Local_inputs.Record.t" in
       fun x__608_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__603_
           ~fields:
             (Field
                { name = "a"
                ; kind = Required
                ; conv =
                    (fun x__606_ ->
                      let _x__607_ = (int_of_sexp [@inlined never]) x__606_ in
                      fun () -> _x__607_)
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; conv =
                          (fun x__604_ ->
                            let _x__605_ = (Trivial.t_of_sexp [@inlined never]) x__604_ in
                            fun () -> _x__605_)
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
           x__608_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { a = a__610_; b = b__612_ } ->
         let bnds__609_ = ([] : _ Stdlib.List.t) in
         let bnds__609_ =
           let arg__613_ = Trivial.sexp_of_t b__612_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__613_ ] :: bnds__609_
            : _ Stdlib.List.t)
         in
         let bnds__609_ =
           let arg__611_ = sexp_of_int a__610_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__611_ ] :: bnds__609_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__609_
       : t @ local -> Sexplib0.Sexp.t)
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
      (let error_source__616_ = "expansion.ml.Local_inputs.Variant.t" in
       function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("a" | "A") as _tag__619_) :: sexp_args__620_) as
         _sexp__618_ ->
         (match sexp_args__620_ with
          | arg0__621_ :: [] ->
            let res0__622_ = Trivial.t_of_sexp arg0__621_ in
            A res0__622_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__616_
              _tag__619_
              _sexp__618_)
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("b" | "B") :: sexps__628_) as sexp__627_
         ->
         Sexplib0.Sexp_conv_record.record_of_sexps
           ~context:sexp__627_
           ~caller:error_source__616_
           ~fields:
             (Field
                { name = "x"
                ; kind = Required
                ; conv =
                    (fun x__625_ ->
                      let _x__626_ = (Trivial.t_of_sexp [@inlined never]) x__625_ in
                      fun () -> _x__626_)
                ; rest =
                    Field
                      { name = "y"
                      ; kind = Required
                      ; conv =
                          (fun x__623_ ->
                            let _x__624_ = (Trivial.t_of_sexp [@inlined never]) x__623_ in
                            fun () -> _x__624_)
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
           sexps__628_
       | Sexplib0.Sexp.Atom ("a" | "A" | "b" | "B") as sexp__617_ ->
         Sexplib0.Sexp_conv_error.stag_takes_args error_source__616_ sexp__617_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__615_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__616_ sexp__615_
       | Sexplib0.Sexp.List [] as sexp__615_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__616_ sexp__615_
       | sexp__615_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__616_
           [ "A"; "B" ]
           sexp__615_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | A arg0__629_ ->
         let res0__630_ = Trivial.sexp_of_t arg0__629_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; res0__630_ ]
       | B { x = x__632_; y = y__634_ } ->
         let bnds__631_ = ([] : _ Stdlib.List.t) in
         let bnds__631_ =
           let arg__635_ = Trivial.sexp_of_t y__634_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "y"; arg__635_ ] :: bnds__631_
            : _ Stdlib.List.t)
         in
         let bnds__631_ =
           let arg__633_ = Trivial.sexp_of_t x__632_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__633_ ] :: bnds__631_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "B" :: bnds__631_)
       : t @ local -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Parameterized : sig
    type 'a t [@@deriving_inline sexp ~localize]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : ('a @ local -> Sexplib0.Sexp.t) -> 'a t @ local -> Sexplib0.Sexp.t
      val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type 'a t = { a : 'a } [@@deriving_inline sexp ~localize]

    let _ = fun (_ : 'a t) -> ()

    let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      let error_source__638_ = "expansion.ml.Local_inputs.Parameterized.t" in
      fun _of_a__636_ x__641_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__638_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__639_ ->
                     let _x__640_ = (_of_a__636_ [@inlined never]) x__639_ in
                     fun () -> _x__640_)
               ; rest = Empty
               })
          ~index_of_field:(function
            | "a" -> 0
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, ()) : _ t ->
            let a = a () in
            { a })
          x__641_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. ('a @ local -> Sexplib0.Sexp.t) -> 'a t @ local -> Sexplib0.Sexp.t
      =
      fun _of_a__642_ { a = a__644_ } ->
      let bnds__643_ = ([] : _ Stdlib.List.t) in
      let bnds__643_ =
        let arg__645_ = _of_a__642_ a__644_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__645_ ] :: bnds__643_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__643_
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

      val sexp_of_t : t @ local -> Sexplib0.Sexp.t
      val sexp_of_t__stack : t @ local -> Sexplib0.Sexp.t @ local
      val t_of_sexp : Sexplib0.Sexp.t -> t
    end
    [@@ocaml.doc "@inline"]

    [@@@end]
  end = struct
    type t = Local_inputs.t [@@deriving_inline sexp ~localize ~stackify]

    let _ = fun (_ : t) -> ()
    let t_of_sexp = (Local_inputs.t_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (Local_inputs.sexp_of_t : t @ local -> Sexplib0.Sexp.t)
    let _ = sexp_of_t

    let sexp_of_t__stack =
      (Local_inputs.sexp_of_t__stack : t @ local -> Sexplib0.Sexp.t @ local)
    ;;

    let _ = sexp_of_t__stack

    [@@@end]
  end

  module Record : sig
    type 'a t [@@deriving_inline sexp ~localize ~stackify]

    include sig
      [@@@ocaml.warning "-32"]

      val sexp_of_t : ('a @ local -> Sexplib0.Sexp.t) -> 'a t @ local -> Sexplib0.Sexp.t

      val sexp_of_t__stack
        :  ('a @ local -> Sexplib0.Sexp.t @ local)
        -> 'a t @ local
        -> Sexplib0.Sexp.t @ local

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
      let error_source__649_ = "expansion.ml.Local_inputs_and_stackify.Record.t" in
      fun _of_a__647_ x__654_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__649_
          ~fields:
            (Field
               { name = "a"
               ; kind = Required
               ; conv =
                   (fun x__652_ ->
                     let _x__653_ = (_of_a__647_ [@inlined never]) x__652_ in
                     fun () -> _x__653_)
               ; rest =
                   Field
                     { name = "x"
                     ; kind = Required
                     ; conv =
                         (fun x__650_ ->
                           let _x__651_ = (int_of_sexp [@inlined never]) x__650_ in
                           fun () -> _x__651_)
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
          x__654_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. ('a @ local -> Sexplib0.Sexp.t) -> 'a t @ local -> Sexplib0.Sexp.t
      =
      fun _of_a__655_ { a = a__657_; x = x__659_ } ->
      let bnds__656_ = ([] : _ Stdlib.List.t) in
      let bnds__656_ =
        let arg__660_ = sexp_of_int x__659_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__660_ ] :: bnds__656_
         : _ Stdlib.List.t)
      in
      let bnds__656_ =
        let arg__658_ = _of_a__655_ a__657_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__658_ ] :: bnds__656_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__656_
    ;;

    let _ = sexp_of_t

    let sexp_of_t__stack
      : 'a.
      ('a @ local -> Sexplib0.Sexp.t @ local) -> 'a t @ local -> Sexplib0.Sexp.t @ local
      =
      fun _of_a__661_ { a = a__663_; x = x__665_ } -> exclave_
      let bnds__662_ = ([] : _ Stdlib.List.t) in
      let bnds__662_ =
        let arg__666_ = sexp_of_int__stack x__665_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "x"; arg__666_ ] :: bnds__662_
         : _ Stdlib.List.t)
      in
      let bnds__662_ =
        let arg__664_ = _of_a__661_ a__663_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__664_ ] :: bnds__662_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__662_
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
      (fun x__669_ -> List.t_of_sexp Trivial.t_of_sexp x__669_ : Sexplib0.Sexp.t -> u)
    ;;

    let _ = u_of_sexp

    let sexp_of_u =
      (fun x__670_ -> List.sexp_of_t Trivial.sexp_of_t x__670_ : u -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_u

    [@@@end]
  end
end

module Custom = struct
  module T = struct
    type t = int * int * int [@@deriving sexp]
  end

  type t = ((int * int * int)[@sexp.custom: T.t]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()
  let t_of_sexp = (fun x__687_ -> list_of_sexp T.t_of_sexp x__687_ : Sexplib0.Sexp.t -> t)
  let _ = t_of_sexp
  let sexp_of_t = (fun x__688_ -> sexp_of_list T.sexp_of_t x__688_ : t -> Sexplib0.Sexp.t)
  let _ = sexp_of_t

  [@@@end]

  module T1 = struct
    type 'a t = 'a * 'a [@@deriving sexp]
  end

  type 'a t1 = (('a * 'a)[@sexp.custom: 'a T1.t]) list [@@deriving_inline sexp]

  let _ = fun (_ : 'a t1) -> ()

  let t1_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t1 =
    fun _of_a__702_ x__704_ -> list_of_sexp (T1.t_of_sexp _of_a__702_) x__704_
  ;;

  let _ = t1_of_sexp

  let sexp_of_t1 : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t1 -> Sexplib0.Sexp.t =
    fun _of_a__705_ x__706_ -> sexp_of_list (T1.sexp_of_t _of_a__705_) x__706_
  ;;

  let _ = sexp_of_t1

  [@@@end]
end
