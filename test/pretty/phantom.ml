[@@@warning "-32-37-60"]

open Ppx_sexp_conv_lib.Conv

module M : sig
  type ('a[@phantom] : any, 'b) t = 'b [@@deriving_inline sexp]

  val sexp_of_t
    : ('a : any) 'b.
    ('b -> Sexplib0.Sexp.t) -> ('a : any, 'b) t -> Sexplib0.Sexp.t

  val t_of_sexp
    : ('a : any) 'b.
    (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a : any, 'b) t

  [@@@end]

  type ('a[@phantom] : any, 'b) alias = ('a, 'b) t [@@deriving_inline sexp]

  val sexp_of_alias
    : ('a : any) 'b.
    ('b -> Sexplib0.Sexp.t) -> ('a : any, 'b) alias -> Sexplib0.Sexp.t

  val alias_of_sexp
    : ('a : any) 'b.
    (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a : any, 'b) alias

  [@@@end]
end = struct
  type ('a[@phantom] : any, 'b) t = 'b [@@deriving_inline sexp]

  let t_of_sexp
    : 'b ('a : any). (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a : any, 'b) t
    =
    fun _of_b__002_ -> _of_b__002_
  ;;

  let sexp_of_t
    : 'b ('a : any). ('b -> Sexplib0.Sexp.t) -> ('a : any, 'b) t -> Sexplib0.Sexp.t
    =
    fun _of_b__005_ -> _of_b__005_
  ;;

  [@@@end]

  type ('a[@phantom] : any, 'b) alias = (('a[@phantom]), 'b) t [@@deriving_inline sexp]

  let alias_of_sexp
    : 'b ('a : any). (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a : any, 'b) alias
    =
    t_of_sexp
  ;;

  let sexp_of_alias
    : 'b ('a : any). ('b -> Sexplib0.Sexp.t) -> ('a : any, 'b) alias -> Sexplib0.Sexp.t
    =
    sexp_of_t
  ;;

  [@@@end]
end

module Phantom_gadt = struct
  type 'a[@phantom] t = T : int -> 'a t [@@deriving_inline sexp]

  let t_of_sexp : 'a. Sexplib0.Sexp.t -> 'a t =
    fun (type a__023_) : (Sexplib0.Sexp.t -> a__023_ t) ->
    let error_source__016_ = "phantom.ml.Phantom_gadt.t" in
    function
    | Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom (("t" | "T") as _tag__019_) :: sexp_args__020_) as _sexp__018_
      ->
      (match sexp_args__020_ with
       | arg0__021_ :: [] ->
         let res0__022_ = int_of_sexp arg0__021_ in
         T res0__022_
       | _ ->
         Sexplib0.Sexp_conv_error.stag_incorrect_n_args
           error_source__016_
           _tag__019_
           _sexp__018_)
    | Sexplib0.Sexp.Atom ("t" | "T") as sexp__017_ ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__016_ sexp__017_
    | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__015_ ->
      Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__016_ sexp__015_
    | Sexplib0.Sexp.List [] as sexp__015_ ->
      Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__016_ sexp__015_
    | sexp__015_ ->
      Sexplib0.Sexp_conv_error.unexpected_stag error_source__016_ [ "T" ] sexp__015_
  ;;

  let sexp_of_t : 'a. 'a t -> Sexplib0.Sexp.t =
    fun (type a__027_) : (a__027_ t -> Sexplib0.Sexp.t) ->
    fun (T arg0__025_) ->
    let res0__026_ = sexp_of_int arg0__025_ in
    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "T"; res0__026_ ]
  ;;

  [@@@end]
end

module Phantom_gadt_packed = struct
  (* Note that [of_sexp] isn't supported for GADTs such as this, as there is no sensible
     implementation. Trying to derive it gives an unbound type variable error, with or
     without the [@phantom]. *)
  type 'a[@phantom] t = T : 'b -> 'a t [@@deriving_inline sexp_of]

  let sexp_of_t : 'a. 'a t -> Sexplib0.Sexp.t =
    fun (type a__031_) : (a__031_ t -> Sexplib0.Sexp.t) ->
    fun (T arg0__029_) ->
    let res0__030_ =
      let _ = arg0__029_ in
      Sexplib0.Sexp.Atom "_"
    in
    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "T"; res0__030_ ]
  ;;

  [@@@end]
end

(* Test [@@phantom: 'a] syntax (alternative to [@phantom] on type parameter) *)
module Phantom_td_syntax : sig
  type ('a, 'b) t = 'b [@@phantom: 'a] [@@deriving_inline sexp]

  val sexp_of_t : ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
  val t_of_sexp : (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t

  [@@@end]

  type ('a, 'b) u = string [@@phantom: 'a * 'b] [@@deriving_inline sexp]

  val sexp_of_u : ('a, 'b) u -> Sexplib0.Sexp.t
  val u_of_sexp : Sexplib0.Sexp.t -> ('a, 'b) u

  [@@@end]
end = struct
  type ('a, 'b) t = 'b [@@phantom: 'a] [@@deriving_inline sexp]

  let t_of_sexp : 'b 'a. (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t =
    fun _of_b__033_ -> _of_b__033_
  ;;

  let sexp_of_t : 'b 'a. ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t =
    fun _of_b__036_ -> _of_b__036_
  ;;

  [@@@end]

  type ('a, 'b) u = string [@@phantom: 'a * 'b] [@@deriving_inline sexp]

  let u_of_sexp : 'a 'b. Sexplib0.Sexp.t -> ('a, 'b) u = string_of_sexp
  let sexp_of_u : 'a 'b. ('a, 'b) u -> Sexplib0.Sexp.t = sexp_of_string

  [@@@end]
end

module Phantom_td_syntax' : sig
  type ('a, 'b) t = ('a, 'b) Phantom_td_syntax.t [@@phantom: 'a] [@@deriving_inline sexp]

  val sexp_of_t : ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
  val t_of_sexp : (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t

  [@@@end]

  type ('a, 'b) u = ('a, 'b) Phantom_td_syntax.u
  [@@phantom: 'a * 'b] [@@deriving_inline sexp]

  val sexp_of_u : ('a, 'b) u -> Sexplib0.Sexp.t
  val u_of_sexp : Sexplib0.Sexp.t -> ('a, 'b) u

  [@@@end]
end = struct
  type ('a, 'b) t = (('a[@phantom]), 'b) Phantom_td_syntax.t
  [@@phantom: 'a] [@@deriving_inline sexp]

  let t_of_sexp : 'b 'a. (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> ('a, 'b) t =
    Phantom_td_syntax.t_of_sexp
  ;;

  let sexp_of_t : 'b 'a. ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t =
    Phantom_td_syntax.sexp_of_t
  ;;

  [@@@end]

  type ('a, 'b) u = (('a[@phantom]), ('b[@phantom])) Phantom_td_syntax.u
  [@@phantom: 'a * 'b] [@@deriving_inline sexp]

  let u_of_sexp : 'a 'b. Sexplib0.Sexp.t -> ('a, 'b) u = Phantom_td_syntax.u_of_sexp
  let sexp_of_u : 'a 'b. ('a, 'b) u -> Sexplib0.Sexp.t = Phantom_td_syntax.sexp_of_u

  [@@@end]
end
