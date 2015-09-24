open Sexplib
open Sexp
open Conv

let debug = ref false
type ('a, 'b) variant2 = 'a [@@deriving of_sexp]
type variant3 = [ `B | ([ `C ], int) variant2 ] [@@deriving of_sexp]

let () =
  let not_deserializable = Atom "C" in
  try ignore ([%of_sexp: variant3] not_deserializable);
      failwith "Expected an exception about a silly type"
  with Conv.Of_sexp_error (exn, sexp) ->
    if !debug then (
      let sexp1 = Conv.sexp_of_exn exn in
      Printf.printf "Conv_error.Of_sexp_error (%s, %s)\n%!"
        (Sexp.to_string sexp1)
        (Sexp.to_string sexp)
    )

(* this one would trigger a warning in 4.0 about unused rec if type_conv
   says that this definition is recursive *)
type r = { r : int } [@@deriving sexp]

module Arity_of_constructors = struct
  type poly =
    [ `No_arg
    | `One_arg of int
    | `One_tuple of (int * string)
    | `Two_args of int * string
    ]
  [@@deriving sexp]

  let () =
    List.iter (fun (value, sexp) ->
      assert (sexp_of_poly value = sexp);
      assert (poly_of_sexp sexp = value);
    ) [
      `No_arg,             Sexp.Atom "No_arg";
      `One_arg 1,          Sexp.(List [Atom "One_arg"; Atom "1"]);
      `One_tuple (1, "a"), Sexp.(List [Atom "One_tuple"; List [Atom "1"; Atom "a"]]);
      `Two_args (1, "a"),  Sexp.(List [Atom "Two_args";  List [Atom "1"; Atom "a"]]);
    ]

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving sexp]

  let () =
    List.iter (fun (value, sexp) ->
      assert (sexp_of_nominal value = sexp);
      assert (nominal_of_sexp sexp = value);
    ) [
      No_arg,             Sexp.Atom "No_arg";
      One_arg 1,          Sexp.(List [Atom "One_arg"; Atom "1"]);
      One_tuple (1, "a"), Sexp.(List [Atom "One_tuple"; List [Atom "1"; Atom "a"]]);
      Two_args (1, "a"),  Sexp.(List [Atom "Two_args"; Atom "1"; Atom "a"]);
    ]
end

module Field_name_should_not_be_rewritten = struct
  type nonrec r = { r : r }
  let _ = fun (r : r) -> r.r
end

module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving sexp]
  end = struct
    type t = [ `A ] [@@deriving sexp]
  end
  module Empty = struct
  end
  module No_warning2(X : sig type t [@@deriving sexp] end) = struct
  end
  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with sexp end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving sexp]
  end
  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving sexp]
  end
  module No_warning5 : S = ((struct
    type t = [ `A ] [@@deriving sexp]
  end : S) : S)

  module Nested_functors
    (M1 : sig type t [@@deriving sexp] end)
    (M2 : sig type t [@@deriving sexp] end) = struct
  end

  let () =
    let module M : sig
      type t [@@deriving sexp]
    end = struct
      type t [@@deriving sexp]
    end in
    ()

  module Include = struct
    include (struct
      type t = int [@@deriving sexp]
    end : sig
      type t [@@deriving sexp]
    end with type t := int)
  end
end

module Default = struct
  type t = {
    a : int [@default 2];
  } [@@deriving sexp]
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List [List [Atom "a"; Atom "2"]]) = sexp_of_t { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the sexp_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = ([`A] as 'a) constraint 'a = a
    [@@deriving sexp]
  end = struct
    type a = [ `A ] [@@deriving sexp]
    type t = [ `A ] [@@deriving sexp]
  end
  let () =
    assert (Sexp.to_string (B.sexp_of_t `A) = "A");
    assert (`A = B.t_of_sexp (Sexp.of_string "A"));
    ()

  module B2 = struct
    type t = [ B.t | `B ] [@@deriving sexp]
  end

  module C : sig
    type t = (int as 'a)
    [@@deriving sexp]
  end = struct
    type t = int
    [@@deriving sexp]
  end

  module D : sig
    type t = 'a constraint 'a = int
    [@@deriving sexp]
  end = struct
    type t = int
    [@@deriving sexp]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving sexp]
  type 'a u = [ t | `b of 'a ] * int [@@deriving sexp]
end

module Drop_default = struct
  type t = {
    a : int [@default 2] [@sexp_drop_default];
  } [@@deriving sexp]
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })
end

module Drop_if = struct
  type t = {
    a : int [@default 2] [@sexp_drop_if fun x -> x mod 2 = 0]
  } [@@deriving sexp]
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 2 })
  let () = assert (Sexp.(List [List [Atom "a"; Atom "3"]]) = sexp_of_t { a = 3 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 4 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "3"]])) = { a = 3 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "4"]])) = { a = 4 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })

  type u = {
    a : int [@sexp_drop_if fun x ->
      (* pa_type_conv used to drop parens altogether, causing type errors in the
         following code *)
      let pair = (x, 2) in
      match Some pair with
      | None -> true
      | Some (x, y) -> x = y
    ]
  } [@@deriving sexp]
end

module No_unused_rec_warning = struct
  type r = { field : r -> unit }
  [@@deriving sexp_of]
end

module True_and_false = struct
  type t =
  | True
  | False
  [@@deriving sexp]

  let () = assert (Sexp.to_string (sexp_of_t True) = "True")
  let () = assert (Sexp.to_string (sexp_of_t False) = "False")
  let () = assert (True = t_of_sexp (Sexp.of_string "True"))
  let () = assert (False = t_of_sexp (Sexp.of_string "False"))
  let () = assert (True = t_of_sexp (Sexp.of_string "true"))
  let () = assert (False = t_of_sexp (Sexp.of_string "false"))

  type u =
  | True of int
  | False of int
  [@@deriving sexp]

  let () = assert (Sexp.to_string (sexp_of_u (True 1)) = "(True 1)")
  let () = assert (Sexp.to_string (sexp_of_u (False 2)) = "(False 2)")
  let () = assert (True 1 = u_of_sexp (Sexp.of_string "(True 1)"))
  let () = assert (False 2 = u_of_sexp (Sexp.of_string "(False 2)"))
  let () = assert (True 1 = u_of_sexp (Sexp.of_string "(true 1)"))
  let () = assert (False 2 = u_of_sexp (Sexp.of_string "(false 2)"))

  exception True [@@deriving sexp]
  let () = assert ("pa_sexp_test.ml.True_and_false.True" = Sexp.to_string (sexp_of_exn True))

  exception False of int [@@deriving sexp]
  let () = assert ("(pa_sexp_test.ml.True_and_false.False 1)" = Sexp.to_string (sexp_of_exn (False 1)))

end

module Gadt = struct
  let is_eq sexp str =
    let sexp2 = Sexp.of_string str in
    if sexp <> sexp2 then begin
      Printf.printf "%S vs %S\n%!" (Sexp.to_string sexp) str;
      assert false
    end

  (* plain type without argument *)
  type 'a s = Packed : 'a s [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: int s] Packed) "Packed"

  (* two kind of existential variables *)
  type 'a t = Packed : 'a * _ * 'b sexp_opaque -> 'a t [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: int t] (Packed (2, "asd", 1.))) "(Packed 2 _ <opaque>)"

  (* plain type with argument *)
  type 'a u = A : 'a -> 'a u [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: int u] (A 2)) "(A 2)"

  (* recursive *)
  type v = A : v option -> v [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: v] (A (Some (A None)))) "(A((A())))"

  (* implicit existential variable *)
  type w = A : 'a * int * ('a -> string) -> w [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: w] (A (1., 2, string_of_float))) "(A _ 2 <fun>)"

  (* tricky variable naming *)
  type 'a x = A : 'a -> 'b x [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: int x] (A 1.)) "(A _)"

  (* unused but colliding variables *)
  type (_, _) y = A : ('a, 'a) y [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: (int, int) y] A) "A"

  (* making sure we're not reversing parameters *)
  type (_, _) z = A : ('a * 'b) -> ('a, 'b) z [@@deriving sexp_of]
  let () = is_eq ([%sexp_of: (int, string) z] (A (1, "a"))) "(A (1 a))"

end

module Anonymous_variable = struct
  type _ t = int [@@deriving sexp]
  let () = assert (Sexp.to_string ([%sexp_of: _ t] 2) = "2")
  let () = assert ([%of_sexp: _ t] (Sexp.of_string "2") = 2)

  (* making sure we don't generate signatures like (_ -> Sexp.t) -> _ t -> Sexp.t which
     are too general *)
  module M : sig
    type _ t [@@deriving sexp]
  end = struct
    type 'a t = 'a [@@deriving sexp]
  end
end

module Record_field_disambiguation = struct

  type a = { fl: float; b : b }
  and b = { fl: int }
  [@@deriving sexp]

end

module Private = struct
  type t = private int [@@deriving sexp_of]

  type ('a, 'b) u = private t [@@deriving sexp_of]

  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving sexp_of]
end
