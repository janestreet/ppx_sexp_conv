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
     fun x__027_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__026_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; layout = Value
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
         x__027_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__029_; b = b__031_; c = c__033_ } ->
       let bnds__028_ = ([] : _ Stdlib.List.t) in
       let bnds__028_ =
         let arg__034_ = sexp_of_int c__033_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__034_ ] :: bnds__028_
          : _ Stdlib.List.t)
       in
       let bnds__028_ =
         let arg__032_ = sexp_of_int b__031_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__032_ ] :: bnds__028_
          : _ Stdlib.List.t)
       in
       let bnds__028_ =
         let arg__030_ = sexp_of_int a__029_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__030_ ] :: bnds__028_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__028_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (fun { a = a__036_; b = b__038_; c = c__040_ } ->
       let bnds__035_ = ([] : _ Stdlib.List.t) in
       let bnds__035_ =
         let arg__041_ = sexp_of_int__stack c__040_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__041_ ] :: bnds__035_
          : _ Stdlib.List.t)
       in
       let bnds__035_ =
         let arg__039_ = sexp_of_int__stack b__038_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__039_ ] :: bnds__035_
          : _ Stdlib.List.t)
       in
       let bnds__035_ =
         let arg__037_ = sexp_of_int__stack a__036_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__037_ ] :: bnds__035_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__035_
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
    (let error_source__043_ = "expansion.ml.Mutable_record.t" in
     fun x__044_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__043_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; layout = Value
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
         x__044_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__046_; b = b__048_; c = c__050_ } ->
       let bnds__045_ = ([] : _ Stdlib.List.t) in
       let bnds__045_ =
         let arg__051_ = sexp_of_int c__050_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__051_ ] :: bnds__045_
          : _ Stdlib.List.t)
       in
       let bnds__045_ =
         let arg__049_ = sexp_of_int b__048_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__049_ ] :: bnds__045_
          : _ Stdlib.List.t)
       in
       let bnds__045_ =
         let arg__047_ = sexp_of_int a__046_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__047_ ] :: bnds__045_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__045_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (fun { a = a__053_; b = b__055_; c = c__057_ } ->
       let bnds__052_ = ([] : _ Stdlib.List.t) in
       let bnds__052_ =
         let arg__058_ = sexp_of_int__stack c__057_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__058_ ] :: bnds__052_
          : _ Stdlib.List.t)
       in
       let bnds__052_ =
         let arg__056_ = sexp_of_int__stack b__055_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__056_ ] :: bnds__052_
          : _ Stdlib.List.t)
       in
       let bnds__052_ =
         let arg__054_ = sexp_of_int__stack a__053_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__054_ ] :: bnds__052_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__052_
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
    (let error_source__061_ = "expansion.ml.Variant.t" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__064_) :: sexp_args__065_) as
       _sexp__063_ ->
       (match sexp_args__065_ with
        | [ arg0__066_; arg1__067_ ] ->
          let res0__068_ = int_of_sexp arg0__066_
          and res1__069_ = int_of_sexp arg1__067_ in
          B (res0__068_, res1__069_)
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__061_
            _tag__064_
            _sexp__063_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__071_) as sexp__070_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__070_
         ~caller:error_source__061_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "d"
                          ; kind = Required
                          ; layout = Value
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
         sexps__071_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("d" | "D") :: sexps__073_) as sexp__072_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__072_
         ~caller:error_source__061_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "t"
                          ; kind = Required
                          ; layout = Value
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
         sexps__073_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__062_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__061_ sexp__062_
     | Sexplib0.Sexp.Atom ("b" | "B" | "c" | "C" | "d" | "D") as sexp__062_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__061_ sexp__062_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__060_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__061_ sexp__060_
     | Sexplib0.Sexp.List [] as sexp__060_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__061_ sexp__060_
     | sexp__060_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__061_
         [ "A"; "B"; "C"; "D" ]
         sexp__060_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B (arg0__074_, arg1__075_) ->
       let res0__076_ = sexp_of_int arg0__074_
       and res1__077_ = sexp_of_int arg1__075_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__076_; res1__077_ ]
     | C { a = a__079_; b = b__081_; d = d__083_ } ->
       let bnds__078_ = ([] : _ Stdlib.List.t) in
       let bnds__078_ =
         let arg__084_ = sexp_of_int d__083_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__084_ ] :: bnds__078_
          : _ Stdlib.List.t)
       in
       let bnds__078_ =
         let arg__082_ = sexp_of_int b__081_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__082_ ] :: bnds__078_
          : _ Stdlib.List.t)
       in
       let bnds__078_ =
         let arg__080_ = sexp_of_int a__079_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__080_ ] :: bnds__078_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__078_)
     | D { a = a__086_; b = b__088_; t = t__090_ } ->
       let bnds__085_ = ([] : _ Stdlib.List.t) in
       let bnds__085_ =
         let arg__091_ = sexp_of_int t__090_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__091_ ] :: bnds__085_
          : _ Stdlib.List.t)
       in
       let bnds__085_ =
         let arg__089_ = sexp_of_int b__088_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__089_ ] :: bnds__085_
          : _ Stdlib.List.t)
       in
       let bnds__085_ =
         let arg__087_ = sexp_of_int a__086_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__087_ ] :: bnds__085_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__085_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B (arg0__092_, arg1__093_) ->
       let res0__094_ = sexp_of_int__stack arg0__092_
       and res1__095_ = sexp_of_int__stack arg1__093_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__094_; res1__095_ ]
     | C { a = a__097_; b = b__099_; d = d__101_ } ->
       let bnds__096_ = ([] : _ Stdlib.List.t) in
       let bnds__096_ =
         let arg__102_ = sexp_of_int__stack d__101_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__102_ ] :: bnds__096_
          : _ Stdlib.List.t)
       in
       let bnds__096_ =
         let arg__100_ = sexp_of_int__stack b__099_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__100_ ] :: bnds__096_
          : _ Stdlib.List.t)
       in
       let bnds__096_ =
         let arg__098_ = sexp_of_int__stack a__097_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__098_ ] :: bnds__096_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__096_)
     | D { a = a__104_; b = b__106_; t = t__108_ } ->
       let bnds__103_ = ([] : _ Stdlib.List.t) in
       let bnds__103_ =
         let arg__109_ = sexp_of_int__stack t__108_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__109_ ] :: bnds__103_
          : _ Stdlib.List.t)
       in
       let bnds__103_ =
         let arg__107_ = sexp_of_int__stack b__106_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__107_ ] :: bnds__103_
          : _ Stdlib.List.t)
       in
       let bnds__103_ =
         let arg__105_ = sexp_of_int__stack a__104_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__105_ ] :: bnds__103_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "D" :: bnds__103_)
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
    (let error_source__115_ = "expansion.ml.Poly_variant.t" in
     function
     | Sexplib0.Sexp.Atom atom__111_ as _sexp__113_ ->
       (match atom__111_ with
        | "A" -> `A
        | "B" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__115_ _sexp__113_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__111_ :: sexp_args__114_) as
       _sexp__113_ ->
       (match atom__111_ with
        | "B" as _tag__116_ ->
          (match sexp_args__114_ with
           | arg0__117_ :: [] ->
             let res0__118_ = int_of_sexp arg0__117_ in
             `B res0__118_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__115_
               _tag__116_
               _sexp__113_)
        | "A" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__115_ _sexp__113_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__112_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__115_ sexp__112_
     | Sexplib0.Sexp.List [] as sexp__112_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__115_ sexp__112_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__120_ = "expansion.ml.Poly_variant.t" in
     fun sexp__119_ ->
       try __t_of_sexp__ sexp__119_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__120_ sexp__119_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | `A -> Sexplib0.Sexp.Atom "A"
     | `B v__121_ -> Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int v__121_ ]
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  let sexp_of_t__stack =
    (function
     | `A -> Sexplib0.Sexp.Atom "A"
     | `B v__122_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; sexp_of_int__stack v__122_ ]
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
    (let error_source__134_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__123_ ->
       try (Poly_variant.__t_of_sexp__ sexp__123_ :> t) with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         (match sexp__123_ with
          | Sexplib0.Sexp.Atom atom__124_ as _sexp__126_ ->
            (match atom__124_ with
             | "C" ->
               Sexplib0.Sexp_conv_error.ptag_takes_args error_source__134_ _sexp__126_
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__124_ :: sexp_args__127_) as
            _sexp__126_ ->
            (match atom__124_ with
             | "C" as _tag__128_ ->
               (match sexp_args__127_ with
                | arg0__135_ :: [] ->
                  let res0__136_ =
                    match arg0__135_ with
                    | Sexplib0.Sexp.List [ arg0__129_; arg1__130_ ] ->
                      let res0__131_ = int_of_sexp arg0__129_
                      and res1__132_ = int_of_sexp arg1__130_ in
                      res0__131_, res1__132_
                    | sexp__133_ ->
                      Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                        error_source__134_
                        2
                        sexp__133_
                  in
                  `C res0__136_
                | _ ->
                  Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                    error_source__134_
                    _tag__128_
                    _sexp__126_)
             | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
          | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__125_ ->
            Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
              error_source__134_
              sexp__125_
          | Sexplib0.Sexp.List [] as sexp__125_ ->
            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
              error_source__134_
              sexp__125_)
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__138_ = "expansion.ml.Inline_poly_variant.t" in
     fun sexp__137_ ->
       try __t_of_sexp__ sexp__137_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__138_ sexp__137_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (function
     | #Poly_variant.t as v__139_ -> Poly_variant.sexp_of_t v__139_
     | `C v__140_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "C"
         ; (let arg0__141_, arg1__142_ = v__140_ in
            let res0__143_ = sexp_of_int arg0__141_
            and res1__144_ = sexp_of_int arg1__142_ in
            Sexplib0.Sexp.List [ res0__143_; res1__144_ ])
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
    (let error_source__147_ = "expansion.ml.Recursive.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__150_) :: sexp_args__151_) as
       _sexp__149_ ->
       (match sexp_args__151_ with
        | arg0__152_ :: [] ->
          let res0__153_ = t_of_sexp arg0__152_ in
          Banana res0__153_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__147_
            _tag__150_
            _sexp__149_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__148_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__147_ sexp__148_
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__148_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__147_ sexp__148_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__146_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__147_ sexp__146_
     | Sexplib0.Sexp.List [] as sexp__146_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__147_ sexp__146_
     | sexp__146_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__147_
         [ "Banana"; "Orange" ]
         sexp__146_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__154_ ->
       let res0__155_ = sexp_of_t arg0__154_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__155_ ]
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
    (let error_source__159_ = "expansion.ml.Mutually_recursive.a" in
     function
     | Sexplib0.Sexp.Atom ("a" | "A") -> A
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("b" | "B") as _tag__162_) :: sexp_args__163_) as
       _sexp__161_ ->
       (match sexp_args__163_ with
        | arg0__164_ :: [] ->
          let res0__165_ = b_of_sexp arg0__164_ in
          B res0__165_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__159_
            _tag__162_
            _sexp__161_)
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("c" | "C") :: sexps__167_) as sexp__166_ ->
       Sexplib0.Sexp_conv_record.record_of_sexps
         ~context:sexp__166_
         ~caller:error_source__159_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = a_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = b_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; layout = Value
                          ; conv = c_of_sexp
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
         ~create:(fun (a, (b, (c, ()))) : a -> C { a; b; c })
         sexps__167_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("a" | "A") :: _) as sexp__160_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__159_ sexp__160_
     | Sexplib0.Sexp.Atom ("b" | "B" | "c" | "C") as sexp__160_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__159_ sexp__160_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__158_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__159_ sexp__158_
     | Sexplib0.Sexp.List [] as sexp__158_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__159_ sexp__158_
     | sexp__158_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__159_
         [ "A"; "B"; "C" ]
         sexp__158_
     : Sexplib0.Sexp.t -> a)

  and b_of_sexp =
    (let error_source__169_ = "expansion.ml.Mutually_recursive.b" in
     fun x__170_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__169_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = a_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv = b_of_sexp
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : b -> { a; b })
         x__170_
     : Sexplib0.Sexp.t -> b)

  and c_of_sexp = (fun x__172_ -> a_of_sexp x__172_ : Sexplib0.Sexp.t -> c)

  let _ = a_of_sexp
  and _ = b_of_sexp
  and _ = c_of_sexp

  let rec sexp_of_a =
    (function
     | A -> Sexplib0.Sexp.Atom "A"
     | B arg0__173_ ->
       let res0__174_ = sexp_of_b arg0__173_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__174_ ]
     | C { a = a__176_; b = b__178_; c = c__180_ } ->
       let bnds__175_ = ([] : _ Stdlib.List.t) in
       let bnds__175_ =
         let arg__181_ = sexp_of_c c__180_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__181_ ] :: bnds__175_
          : _ Stdlib.List.t)
       in
       let bnds__175_ =
         let arg__179_ = sexp_of_b b__178_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__179_ ] :: bnds__175_
          : _ Stdlib.List.t)
       in
       let bnds__175_ =
         let arg__177_ = sexp_of_a a__176_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__177_ ] :: bnds__175_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "C" :: bnds__175_)
     : a -> Sexplib0.Sexp.t)

  and sexp_of_b =
    (fun { a = a__183_; b = b__185_ } ->
       let bnds__182_ = ([] : _ Stdlib.List.t) in
       let bnds__182_ =
         let arg__186_ = sexp_of_b b__185_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__186_ ] :: bnds__182_
          : _ Stdlib.List.t)
       in
       let bnds__182_ =
         let arg__184_ = sexp_of_a a__183_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__184_ ] :: bnds__182_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__182_
     : b -> Sexplib0.Sexp.t)

  and sexp_of_c = (fun x__187_ -> sexp_of_a x__187_ : c -> Sexplib0.Sexp.t)

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
    (let error_source__191_ = "expansion.ml.Re_export.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("banana" | "Banana") as _tag__194_) :: sexp_args__195_) as
       _sexp__193_ ->
       (match sexp_args__195_ with
        | arg0__196_ :: [] ->
          let res0__197_ = t_of_sexp arg0__196_ in
          Banana res0__197_
        | _ ->
          Sexplib0.Sexp_conv_error.stag_incorrect_n_args
            error_source__191_
            _tag__194_
            _sexp__193_)
     | Sexplib0.Sexp.Atom ("orange" | "Orange") -> Orange
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("orange" | "Orange") :: _) as sexp__192_ ->
       Sexplib0.Sexp_conv_error.stag_no_args error_source__191_ sexp__192_
     | Sexplib0.Sexp.Atom ("banana" | "Banana") as sexp__192_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__191_ sexp__192_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__190_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__191_ sexp__190_
     | Sexplib0.Sexp.List [] as sexp__190_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__191_ sexp__190_
     | sexp__190_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag
         error_source__191_
         [ "Banana"; "Orange" ]
         sexp__190_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (function
     | Banana arg0__198_ ->
       let res0__199_ = sexp_of_t arg0__198_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Banana"; res0__199_ ]
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
    fun _of_a__200_ x__202_ -> option_of_sexp (list_of_sexp _of_a__200_) x__202_
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__203_ x__204_ -> sexp_of_option (sexp_of_list _of_a__203_) x__204_
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
    fun _of_a__212_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun _of_a__214_ _ -> Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore
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
    fun _of_a__215_ _of_b__216_ -> Sexplib0.Sexp_conv.fun_of_sexp
  ;;

  let _ = t_of_sexp

  let sexp_of_t
    : 'a 'b.
    ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
    =
    fun _of_a__218_ _of_b__219_ _ ->
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

  let sexp_of_t : 'a__221_. ('a__221_ -> Sexplib0.Sexp.t) -> 'a__221_ t -> Sexplib0.Sexp.t
    =
    fun (type a__227_) : ((a__227_ -> Sexplib0.Sexp.t) -> a__227_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__222_ -> function
    | A -> Sexplib0.Sexp.Atom "A"
    | B arg0__223_ ->
      let res0__224_ = sexp_of_int arg0__223_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "B"; res0__224_ ]
    | C arg0__225_ ->
      let res0__226_ = sexp_of_list (fun _ -> Sexplib0.Sexp.Atom "_") arg0__225_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "C"; res0__226_ ]
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
    (let default__230_ : [ `B ] = `B in
     let error_source__229_ = "expansion.ml.Recursive_record_containing_variant.t" in
     fun x__246_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__229_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv =
                  (fun sexp__245_ ->
                    try
                      match sexp__245_ with
                      | Sexplib0.Sexp.Atom atom__238_ as _sexp__240_ ->
                        (match atom__238_ with
                         | "A" ->
                           Sexplib0.Sexp_conv_error.ptag_takes_args
                             error_source__229_
                             _sexp__240_
                         | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                      | Sexplib0.Sexp.List
                          (Sexplib0.Sexp.Atom atom__238_ :: sexp_args__241_) as
                        _sexp__240_ ->
                        (match atom__238_ with
                         | "A" as _tag__242_ ->
                           (match sexp_args__241_ with
                            | arg0__243_ :: [] ->
                              let res0__244_ = t_of_sexp arg0__243_ in
                              `A res0__244_
                            | _ ->
                              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                error_source__229_
                                _tag__242_
                                _sexp__240_)
                         | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__239_ ->
                        Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                          error_source__229_
                          sexp__239_
                      | Sexplib0.Sexp.List [] as sexp__239_ ->
                        Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                          error_source__229_
                          sexp__239_
                    with
                    | Sexplib0.Sexp_conv_error.No_variant_match ->
                      Sexplib0.Sexp_conv_error.no_matching_variant_found
                        error_source__229_
                        sexp__245_)
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__230_)
                    ; layout = Value
                    ; conv =
                        (fun sexp__236_ ->
                          try
                            match sexp__236_ with
                            | Sexplib0.Sexp.Atom atom__232_ as _sexp__234_ ->
                              (match atom__232_ with
                               | "B" -> `B
                               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                            | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__232_ :: _) as
                              _sexp__234_ ->
                              (match atom__232_ with
                               | "B" ->
                                 Sexplib0.Sexp_conv_error.ptag_no_args
                                   error_source__229_
                                   _sexp__234_
                               | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                            | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__233_
                              ->
                              Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                                error_source__229_
                                sexp__233_
                            | Sexplib0.Sexp.List [] as sexp__233_ ->
                              Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                                error_source__229_
                                sexp__233_
                          with
                          | Sexplib0.Sexp_conv_error.No_variant_match ->
                            Sexplib0.Sexp_conv_error.no_matching_variant_found
                              error_source__229_
                              sexp__236_)
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "a" -> 0
           | "b" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (a, (b, ())) : t -> { a; b })
         x__246_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let rec sexp_of_t =
    (let default__253_ : [ `B ] = `B
     and drop_default__252_ : [ `B ] -> [ `B ] -> Stdlib.Bool.t = Poly.equal in
     fun { a = a__248_; b = b__254_ } ->
       let bnds__247_ = ([] : _ Stdlib.List.t) in
       let bnds__247_ =
         if drop_default__252_ default__253_ b__254_
         then bnds__247_
         else (
           let arg__256_ = (fun `B -> Sexplib0.Sexp.Atom "B") b__254_ in
           let bnd__255_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__256_ ] in
           (bnd__255_ :: bnds__247_ : _ Stdlib.List.t))
       in
       let bnds__247_ =
         let arg__249_ =
           let (`A v__250_) = a__248_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "A"; sexp_of_t v__250_ ]
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__249_ ] :: bnds__247_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__247_
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
    (let error_source__258_ = "expansion.ml.Poly_record.t" in
     fun x__268_ ->
       let open struct
         type a__259_ = { a__259_ : 'a. 'a list } [@@unboxed]
         type b__260_ = { b__260_ : 'b. 'b option } [@@unboxed]
         type c__261_ = { c__261_ : 'c. 'c } [@@unboxed]
       end in
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__258_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv =
                  (fun sexp__266_ ->
                    { a__259_ =
                        (let _a__267_ =
                           Sexplib0.Sexp_conv_error.record_poly_field_value
                             error_source__258_
                         in
                         list_of_sexp _a__267_ sexp__266_)
                    })
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Required
                    ; layout = Value
                    ; conv =
                        (fun sexp__264_ ->
                          { b__260_ =
                              (let _b__265_ =
                                 Sexplib0.Sexp_conv_error.record_poly_field_value
                                   error_source__258_
                               in
                               option_of_sexp _b__265_ sexp__264_)
                          })
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Required
                          ; layout = Value
                          ; conv =
                              (fun sexp__262_ ->
                                { c__261_ =
                                    (let _c__263_ =
                                       Sexplib0.Sexp_conv_error.record_poly_field_value
                                         error_source__258_
                                     in
                                     _c__263_ sexp__262_)
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
         ~create:(fun ({ a__259_ = a }, ({ b__260_ = b }, ({ c__261_ = c }, ()))) : t ->
           { a; b; c })
         x__268_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__270_; b = b__273_; c = c__276_ } ->
       let bnds__269_ = ([] : _ Stdlib.List.t) in
       let bnds__269_ =
         let arg__277_ =
           let _of_c__278_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           _of_c__278_ c__276_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__277_ ] :: bnds__269_
          : _ Stdlib.List.t)
       in
       let bnds__269_ =
         let arg__274_ =
           let _of_b__275_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_option _of_b__275_ b__273_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__274_ ] :: bnds__269_
          : _ Stdlib.List.t)
       in
       let bnds__269_ =
         let arg__271_ =
           let _of_a__272_ = (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) in
           sexp_of_list _of_a__272_ a__270_
         in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__271_ ] :: bnds__269_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__269_
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
    (let default__281_ : int = 0
     and default__282_ : int = 0
     and default__283_ : int = 0
     and default__284_ : int = 0
     and default__285_ : int = 0 in
     let error_source__280_ = "expansion.ml.Record_with_defaults.t" in
     fun x__286_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__280_
         ~fields:
           (Field
              { name = "a"
              ; kind = Default (fun () -> default__285_)
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Default (fun () -> default__284_)
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Default (fun () -> default__283_)
                          ; layout = Value
                          ; conv = int_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Default (fun () -> default__282_)
                                ; layout = Value
                                ; conv = int_of_sexp
                                ; rest =
                                    Field
                                      { name = "e"
                                      ; kind = Default (fun () -> default__281_)
                                      ; layout = Value
                                      ; conv = int_of_sexp
                                      ; rest =
                                          Field
                                            { name = "f"
                                            ; kind = Required
                                            ; layout = Value
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
         x__286_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (let default__291_ : int = 0
     and default__296_ : int = 0
     and default__301_ : int = 0
     and default__307_ : int = 0
     and drop_default__306_ : int -> int -> Stdlib.Bool.t = ( = )
     and drop_if__312_ : Stdlib.Unit.t -> int -> Stdlib.Bool.t = fun () -> ( = ) 0 in
     fun { a = a__288_; b = b__292_; c = c__297_; d = d__302_; e = e__308_; f = f__313_ } ->
       let bnds__287_ = ([] : _ Stdlib.List.t) in
       let bnds__287_ =
         if (drop_if__312_ ()) f__313_
         then bnds__287_
         else (
           let arg__315_ = sexp_of_int f__313_ in
           let bnd__314_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "f"; arg__315_ ] in
           (bnd__314_ :: bnds__287_ : _ Stdlib.List.t))
       in
       let bnds__287_ =
         if drop_default__306_ default__307_ e__308_
         then bnds__287_
         else (
           let arg__310_ = sexp_of_int e__308_ in
           let bnd__309_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "e"; arg__310_ ] in
           (bnd__309_ :: bnds__287_ : _ Stdlib.List.t))
       in
       let bnds__287_ =
         let arg__304_ = sexp_of_int d__302_ in
         if Sexplib0.Sexp_conv.( = ) (sexp_of_int default__301_) arg__304_
         then bnds__287_
         else (
           let bnd__303_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__304_ ] in
           (bnd__303_ :: bnds__287_ : _ Stdlib.List.t))
       in
       let bnds__287_ =
         if [%equal: int] default__296_ c__297_
         then bnds__287_
         else (
           let arg__299_ = sexp_of_int c__297_ in
           let bnd__298_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__299_ ] in
           (bnd__298_ :: bnds__287_ : _ Stdlib.List.t))
       in
       let bnds__287_ =
         if [%compare.equal: int] default__291_ b__292_
         then bnds__287_
         else (
           let arg__294_ = sexp_of_int b__292_ in
           let bnd__293_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__294_ ] in
           (bnd__293_ :: bnds__287_ : _ Stdlib.List.t))
       in
       let bnds__287_ =
         let arg__289_ = sexp_of_int a__288_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__289_ ] :: bnds__287_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__287_
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
    (let error_source__323_ = "expansion.ml.Record_with_special_types.t" in
     fun x__324_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__323_
         ~fields:
           (Field
              { name = "a"
              ; kind = Sexp_option
              ; layout = Value
              ; conv = int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Sexp_list
                    ; layout = Value
                    ; conv = int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Sexp_array
                          ; layout = Value
                          ; conv = int_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Sexp_bool
                                ; layout = Value
                                ; conv = ()
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
         x__324_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__326_; b = b__331_; c = c__335_; d = d__338_ } ->
       let bnds__325_ = ([] : _ Stdlib.List.t) in
       let bnds__325_ =
         if d__338_
         then (
           let bnd__339_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d" ] in
           (bnd__339_ :: bnds__325_ : _ Stdlib.List.t))
         else bnds__325_
       in
       let bnds__325_ =
         if match c__335_ with
            | [||] -> true
            | _ -> false
         then bnds__325_
         else (
           let arg__337_ = (sexp_of_array sexp_of_int) c__335_ in
           let bnd__336_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__337_ ] in
           (bnd__336_ :: bnds__325_ : _ Stdlib.List.t))
       in
       let bnds__325_ =
         if match b__331_ with
            | [] -> true
            | _ -> false
         then bnds__325_
         else (
           let arg__333_ = (sexp_of_list sexp_of_int) b__331_ in
           let bnd__332_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__333_ ] in
           (bnd__332_ :: bnds__325_ : _ Stdlib.List.t))
       in
       let bnds__325_ =
         match a__326_ with
         | Stdlib.Option.None -> bnds__325_
         | Stdlib.Option.Some v__327_ ->
           let arg__329_ = sexp_of_int v__327_ in
           let bnd__328_ = Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__329_ ] in
           (bnd__328_ :: bnds__325_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__325_
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
    (let error_source__341_ = "expansion.ml.Record_with_omit_nil.t" in
     fun x__342_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__341_
         ~fields:
           (Field
              { name = "a"
              ; kind = Omit_nil
              ; layout = Value
              ; conv = option_of_sexp int_of_sexp
              ; rest =
                  Field
                    { name = "b"
                    ; kind = Omit_nil
                    ; layout = Value
                    ; conv = list_of_sexp int_of_sexp
                    ; rest =
                        Field
                          { name = "c"
                          ; kind = Omit_nil
                          ; layout = Value
                          ; conv = unit_of_sexp
                          ; rest =
                              Field
                                { name = "d"
                                ; kind = Omit_nil
                                ; layout = Value
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
         x__342_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__344_; b = b__346_; c = c__348_; d = d__350_ } ->
       let bnds__343_ = ([] : _ Stdlib.List.t) in
       let bnds__343_ =
         match sexp_of_int d__350_ with
         | Sexplib0.Sexp.List [] -> bnds__343_
         | arg__351_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "d"; arg__351_ ] :: bnds__343_
            : _ Stdlib.List.t)
       in
       let bnds__343_ =
         match sexp_of_unit c__348_ with
         | Sexplib0.Sexp.List [] -> bnds__343_
         | arg__349_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "c"; arg__349_ ] :: bnds__343_
            : _ Stdlib.List.t)
       in
       let bnds__343_ =
         match sexp_of_list sexp_of_int b__346_ with
         | Sexplib0.Sexp.List [] -> bnds__343_
         | arg__347_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__347_ ] :: bnds__343_
            : _ Stdlib.List.t)
       in
       let bnds__343_ =
         match sexp_of_option sexp_of_int a__344_ with
         | Sexplib0.Sexp.List [] -> bnds__343_
         | arg__345_ ->
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__345_ ] :: bnds__343_
            : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__343_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Variant_with_sexp_list = struct
  type t = A of int list [@sexp.list] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__354_ = "expansion.ml.Variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom (("a" | "A") as _tag__357_) :: sexp_args__358_) as
       _sexp__356_ -> A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__358_)
     | Sexplib0.Sexp.Atom ("a" | "A") as sexp__355_ ->
       Sexplib0.Sexp_conv_error.stag_takes_args error_source__354_ sexp__355_
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__353_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__354_ sexp__353_
     | Sexplib0.Sexp.List [] as sexp__353_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__354_ sexp__353_
     | sexp__353_ ->
       Sexplib0.Sexp_conv_error.unexpected_stag error_source__354_ [ "A" ] sexp__353_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (A l__359_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__359_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Poly_variant_with_sexp_list = struct
  type t = [ `A of int list [@sexp.list] ] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    (let error_source__366_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     function
     | Sexplib0.Sexp.Atom atom__361_ as _sexp__363_ ->
       (match atom__361_ with
        | "A" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__366_ _sexp__363_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__361_ :: sexp_args__364_) as
       _sexp__363_ ->
       (match atom__361_ with
        | "A" as _tag__365_ ->
          `A (Sexplib0.Sexp_conv.list_map int_of_sexp sexp_args__364_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__362_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__366_ sexp__362_
     | Sexplib0.Sexp.List [] as sexp__362_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__366_ sexp__362_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = __t_of_sexp__

  let t_of_sexp =
    (let error_source__368_ = "expansion.ml.Poly_variant_with_sexp_list.t" in
     fun sexp__367_ ->
       try __t_of_sexp__ sexp__367_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__368_ sexp__367_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun (`A l__369_) ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "A" :: Sexplib0.Sexp_conv.list_map sexp_of_int l__369_)
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Record_allowing_extra_fields = struct
  type t = { a : int } [@@allow_extra_fields] [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (let error_source__371_ = "expansion.ml.Record_allowing_extra_fields.t" in
     fun x__372_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__371_
         ~fields:
           (Field
              { name = "a"
              ; kind = Required
              ; layout = Value
              ; conv = int_of_sexp
              ; rest = Empty
              })
         ~index_of_field:(function
           | "a" -> 0
           | _ -> -1)
         ~allow_extra_fields:true
         ~create:(fun (a, ()) : t -> { a })
         x__372_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun { a = a__374_ } ->
       let bnds__373_ = ([] : _ Stdlib.List.t) in
       let bnds__373_ =
         let arg__375_ = sexp_of_int a__374_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__375_ ] :: bnds__373_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__373_
     : t -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_t

  [@@@end]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving_inline sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    (fun x__377_ -> list_of_sexp Sexplib0.Sexp_conv.opaque_of_sexp x__377_
     : Sexplib0.Sexp.t -> t)
  ;;

  let _ = t_of_sexp

  let sexp_of_t =
    (fun x__378_ -> sexp_of_list (Sexplib0.Sexp_conv.sexp_of_opaque : _ -> _) x__378_
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
      (let error_source__380_ = "expansion.ml.Portable.t" in
       fun x__381_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__380_
           ~fields:
             (Field
                { name = "u"
                ; kind = Required
                ; layout = Value
                ; conv = u_of_sexp int_of_sexp
                ; rest =
                    Field
                      { name = "b"
                      ; kind = Required
                      ; layout = Value
                      ; conv = int_of_sexp
                      ; rest = Empty
                      }
                })
           ~index_of_field:(function
             | "u" -> 0
             | "b" -> 1
             | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (u, (b, ())) : t -> { u; b })
           x__381_
       : Sexplib0.Sexp.t -> t)

    and u_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a u =
      let error_source__384_ = "expansion.ml.Portable.u" in
      fun _of_a__382_ x__385_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp
          ~caller:error_source__384_
          ~fields:
            (Field
               { name = "t"
               ; kind = Required
               ; layout = Value
               ; conv = t_of_sexp
               ; rest =
                   Field
                     { name = "a"
                     ; kind = Required
                     ; layout = Value
                     ; conv = _of_a__382_
                     ; rest = Empty
                     }
               })
          ~index_of_field:(function
            | "t" -> 0
            | "a" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (t, (a, ())) : _ u -> { t; a })
          x__385_
    ;;

    let _ = t_of_sexp
    and _ = u_of_sexp
  end

  let _ = t_of_sexp
  and _ = u_of_sexp

  include struct
    let rec sexp_of_t =
      (fun { u = u__387_; b = b__389_ } ->
         let bnds__386_ = ([] : _ Stdlib.List.t) in
         let bnds__386_ =
           let arg__390_ = sexp_of_int b__389_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "b"; arg__390_ ] :: bnds__386_
            : _ Stdlib.List.t)
         in
         let bnds__386_ =
           let arg__388_ = sexp_of_u sexp_of_int u__387_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "u"; arg__388_ ] :: bnds__386_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__386_
       : t -> Sexplib0.Sexp.t)

    and sexp_of_u : 'a. ('a -> Sexplib0.Sexp.t) -> 'a u -> Sexplib0.Sexp.t =
      fun _of_a__391_ { t = t__393_; a = a__395_ } ->
      let bnds__392_ = ([] : _ Stdlib.List.t) in
      let bnds__392_ =
        let arg__396_ = _of_a__391_ a__395_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "a"; arg__396_ ] :: bnds__392_
         : _ Stdlib.List.t)
      in
      let bnds__392_ =
        let arg__394_ = sexp_of_t t__393_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "t"; arg__394_ ] :: bnds__392_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List bnds__392_
    ;;

    let _ = sexp_of_t
    and _ = sexp_of_u
  end

  let _ = sexp_of_t
  and _ = sexp_of_u

  [@@@end]
end
