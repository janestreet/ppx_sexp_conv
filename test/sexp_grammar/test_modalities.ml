module type Sexp_of = sig
  type t [@@deriving_inline sexp_of ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Sexp_of_local = sig
  type t [@@deriving_inline sexp_of ~stackify ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
    val sexp_of_t__stack : t -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Of_sexp = sig
  type t [@@deriving_inline of_sexp ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val t_of_sexp : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Of_sexp_poly = sig
  type t [@@deriving_inline of_sexp_poly ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Sexp = sig
  type t [@@deriving_inline sexp ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Sexp_local = sig
  type t [@@deriving_inline sexp ~stackify ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S__stack with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Sexp_poly = sig
  type t [@@deriving_inline sexp_poly ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type Sexp_poly_local = sig
  type t [@@deriving_inline sexp_poly ~stackify ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val __t_of_sexp__ : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val sexp_of_t__stack : t -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end
