[@@@ocamlformat "disable"]
[@@@warning "-32-60"]
module M : sig
  type ('a[@phantom] : any, 'b) t = 'b
  [@@deriving_inline sexp]
  val sexp_of_t :
    ('a : any) 'b .
      ('b -> Sexplib0.Sexp.t) -> (('a : any), 'b) t -> Sexplib0.Sexp.t
  val t_of_sexp :
    ('a : any) 'b .
      (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> (('a : any), 'b) t
  [@@@end]

  type ('a [@phantom] : any, 'b) alias = ('a, 'b) t
  [@@deriving_inline sexp]
  val sexp_of_alias :
    ('a : any) 'b .
      ('b -> Sexplib0.Sexp.t) -> (('a : any), 'b) alias -> Sexplib0.Sexp.t
  val alias_of_sexp :
    ('a : any) 'b .
      (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> (('a : any), 'b) alias
  [@@@end]
end = struct
  type ('a[@phantom] : any, 'b) t = 'b
  [@@deriving_inline sexp]
  let t_of_sexp :
    'b ('a : any) .
      (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> (('a : any), 'b) t
    = fun _of_b__002_ -> _of_b__002_
  let sexp_of_t :
    'b ('a : any) .
      ('b -> Sexplib0.Sexp.t) -> (('a : any), 'b) t -> Sexplib0.Sexp.t
    = fun _of_b__005_ -> _of_b__005_
  [@@@end]

  type ('a [@phantom] : any, 'b) alias = ('a[@phantom], 'b) t
  [@@deriving_inline sexp]
  let alias_of_sexp :
    'b ('a : any) .
      (Sexplib0.Sexp.t -> 'b) -> Sexplib0.Sexp.t -> (('a : any), 'b) alias
    = t_of_sexp
  let sexp_of_alias :
    'b ('a : any) .
      ('b -> Sexplib0.Sexp.t) -> (('a : any), 'b) alias -> Sexplib0.Sexp.t
    = sexp_of_t
  [@@@end]
end
