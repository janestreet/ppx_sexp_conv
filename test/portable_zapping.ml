open! Base

[@@@disable_unused_warnings]

module Sexp_of_t = struct
  type t = A [@@deriving sexp_of ~portable]
end

module Sexp_of_t_portable : sig
  include module type of struct
    include Sexp_of_t
  end
end = struct
  include Sexp_of_t
end

module Sexp_of_not_t = struct
  type not_t = A [@@deriving sexp_of ~portable]
end

module Sexp_of_not_t_portable : sig
  include module type of struct
    include Sexp_of_not_t
  end
end = struct
  include Sexp_of_not_t
end

module Of_sexp_t = struct
  type t = A [@@deriving of_sexp ~portable]
end

module Of_sexp_t_portable : sig
  include module type of struct
    include Of_sexp_t
  end
end = struct
  include Of_sexp_t
end

module Of_sexp_not_t = struct
  type not_t = A [@@deriving of_sexp ~portable]
end

module Of_sexp_not_t_portable : sig
  include module type of struct
    include Of_sexp_not_t
  end
end = struct
  include Of_sexp_not_t
end

module Sexp_t = struct
  type t = A [@@deriving sexp ~portable]
end

module Sexp_t_portable : sig
  include module type of struct
    include Sexp_t
  end
end = struct
  include Sexp_t
end

module Sexp_not_t = struct
  type not_t = A [@@deriving sexp ~portable]
end

module Sexp_not_t_portable : sig
  include module type of struct
    include Sexp_not_t
  end
end = struct
  include Sexp_not_t
end

module Sexp_t1 = struct
  type 'a t = A of 'a [@@deriving sexp ~portable]
end

module Sexp_t1_portable : sig
  include module type of struct
    include Sexp_t1
  end
end = struct
  include Sexp_t1
end
