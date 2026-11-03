type unit = U [@@deriving sexp]

type a = { a : unit }
and b = { b : unit } [@@deriving sexp]

type c = [ `C of unit ]
and d = [ `D of unit ] [@@deriving sexp]

type 'a t = { a : 'a } [@@deriving sexp]
