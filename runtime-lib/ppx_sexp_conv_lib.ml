module Conv = Sexplib0.Sexp_conv
module Conv_error = Sexplib0.Sexp_conv_error
module Or_null = Basement.Or_null_shim
module Sexp_grammar = Sexplib0.Sexp_grammar

module Sexp = struct
  include Sexplib0.Sexp

  let t_sexp_grammar = Conv.sexp_t_sexp_grammar
end

module Sexpable = Sexplib0.Sexpable

type converter = exn -> Sexp.t

external magic_portable_exn_converter
  :  converter
  -> converter @ portable
  @@ portable
  = "%identity"
