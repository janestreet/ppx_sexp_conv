open Base
open Ppx_sexp_conv_lib.Conv

type 'a[@phantom] t = int [@@deriving sexp]
type ('a[@phantom], 'b) u = 'b list [@@deriving sexp]
type ('a[@phantom], 'b[@phantom]) v = string [@@deriving sexp]
type ('a, 'b[@phantom]) w = 'a option [@@deriving sexp]

(* Alternative syntax using [@@phantom: 'a] on the type declaration *)
type 'a t2 = int [@@phantom: 'a] [@@deriving sexp]
type ('a, 'b) u2 = 'b list [@@phantom: 'a] [@@deriving sexp]
type ('a, 'b) v2 = string [@@phantom: 'a * 'b] [@@deriving sexp]
type ('a, 'b) w2 = 'a option [@@phantom: 'b] [@@deriving sexp]

(* Test that [@@phantom] only applies to the type it's attached to in mutually recursive
   definitions *)
type 'a mut1 = int [@@phantom: 'a]
and 'a mut2 = 'a list [@@deriving sexp]

let print_s sexp = Stdio.print_endline (Sexp.to_string_hum sexp)

let%expect_test "phantom type parameters work correctly" =
  let x : int t = 42 in
  let sexp = sexp_of_t x in
  print_s sexp;
  [%expect {| 42 |}];
  let y : string t = t_of_sexp sexp in
  Expect_test_helpers_base.require_equal (module Int) x y;
  [%expect {| |}];
  let x : (int, string) u = [ "hello"; "world" ] in
  let sexp = sexp_of_u sexp_of_string x in
  print_s sexp;
  [%expect {| (hello world) |}];
  let y : (bool, string) u = u_of_sexp string_of_sexp sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = string list [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}];
  let x : (int, string) v = "test" in
  let sexp = sexp_of_v x in
  print_s sexp;
  [%expect {| test |}];
  let y : (bool, float) v = v_of_sexp sexp in
  Expect_test_helpers_base.require_equal (module String) x y;
  [%expect {| |}];
  let x : (int, string) w = Some 42 in
  let sexp = sexp_of_w sexp_of_int x in
  print_s sexp;
  [%expect {| (42) |}];
  let y : (int, bool) w = w_of_sexp int_of_sexp sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = int option [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}]
;;

let%expect_test "[@@phantom: 'a] only applies to specific type in mutually recursive defs"
  =
  (* mut1 has phantom 'a, mut2 does not *)
  let x : int mut1 = 42 in
  let sexp = sexp_of_mut1 x in
  print_s sexp;
  [%expect {| 42 |}];
  let x : int mut2 = [ 1; 2; 3 ] in
  let sexp = sexp_of_mut2 sexp_of_int x in
  print_s sexp;
  [%expect {| (1 2 3) |}]
;;

let%expect_test "phantom type parameters using [@@phantom: 'a] syntax" =
  (* Test single phantom parameter: [@@phantom: 'a] *)
  let x : int t2 = 42 in
  let sexp = sexp_of_t2 x in
  print_s sexp;
  [%expect {| 42 |}];
  let y : string t2 = t2_of_sexp sexp in
  Expect_test_helpers_base.require_equal (module Int) x y;
  [%expect {| |}];
  (* Test one of multiple parameters is phantom: [@@phantom: 'a] *)
  let x : (int, string) u2 = [ "hello"; "world" ] in
  let sexp = sexp_of_u2 sexp_of_string x in
  print_s sexp;
  [%expect {| (hello world) |}];
  let y : (bool, string) u2 = u2_of_sexp string_of_sexp sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = string list [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}];
  (* Test multiple phantom parameters: [@@phantom: 'a * 'b] *)
  let x : (int, string) v2 = "test" in
  let sexp = sexp_of_v2 x in
  print_s sexp;
  [%expect {| test |}];
  let y : (bool, float) v2 = v2_of_sexp sexp in
  Expect_test_helpers_base.require_equal (module String) x y;
  [%expect {| |}];
  (* Test second parameter is phantom: [@@phantom: 'b] *)
  let x : (int, string) w2 = Some 42 in
  let sexp = sexp_of_w2 sexp_of_int x in
  print_s sexp;
  [%expect {| (42) |}];
  let y : (int, bool) w2 = w2_of_sexp int_of_sexp sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = int option [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}]
;;

let%expect_test "also in extensions" =
  let open struct
    (* versions of types with no sexps derived *)
    type foo_int = int
    type foo_string = string
    type foo_bool = bool
    type foo_float = float
  end in
  let x : foo_int t = 42 in
  let sexp = [%sexp_of: (foo_int[@phantom]) t] x in
  print_s sexp;
  [%expect {| 42 |}];
  let y : foo_string t = [%of_sexp: (foo_string[@phantom]) t] sexp in
  Expect_test_helpers_base.require_equal (module Int) x y;
  [%expect {| |}];
  let x : (foo_int, string) u = [ "hello"; "world" ] in
  let sexp = [%sexp_of: ((foo_int[@phantom]), string) u] x in
  print_s sexp;
  [%expect {| (hello world) |}];
  let y : (foo_bool, string) u = [%of_sexp: ((foo_bool[@phantom]), string) u] sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = string list [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}];
  let x : (foo_int, foo_string) v = "test" in
  let sexp = [%sexp_of: ((foo_int[@phantom]), (foo_string[@phantom])) v] x in
  print_s sexp;
  [%expect {| test |}];
  let y : (foo_bool, foo_float) v =
    [%of_sexp: ((foo_bool[@phantom]), (foo_float[@phantom])) v] sexp
  in
  Expect_test_helpers_base.require_equal (module String) x y;
  [%expect {| |}];
  let x : (int, foo_string) w = Some 42 in
  let sexp = [%sexp_of: (int, (foo_string[@phantom])) w] x in
  print_s sexp;
  [%expect {| (42) |}];
  let y : (int, foo_bool) w = [%of_sexp: (int, (foo_bool[@phantom])) w] sexp in
  Expect_test_helpers_base.require_equal
    (module struct
      type t = int option [@@deriving equal ~localize, sexp_of]
    end)
    x
    y;
  [%expect {| |}]
;;

[@@@ocamlformat "disable"]

type 'a[@phantom] variant =
  | X
  | Y
  | Z
[@@deriving_inline sexp]

let _ = fun (_ : 'a variant) -> ()

let variant_of_sexp : 'a . Sexplib0.Sexp.t -> 'a variant =
  fun (type a__092_) ->
    (let error_source__090_ = "phantom.ml.variant" in
     function
     | Sexplib0.Sexp.Atom ("x" | "X") -> X
     | Sexplib0.Sexp.Atom ("y" | "Y") -> Y
     | Sexplib0.Sexp.Atom ("z" | "Z") -> Z
     | Sexplib0.Sexp.List ((Sexplib0.Sexp.Atom
         ("x" | "X" | "y" | "Y" | "z" | "Z"))::_) as sexp__091_ ->
         Sexplib0.Sexp_conv_error.stag_no_args error_source__090_ sexp__091_
     | Sexplib0.Sexp.List ((Sexplib0.Sexp.List _)::_) as sexp__089_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__090_
           sexp__089_
     | Sexplib0.Sexp.List [] as sexp__089_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__090_
           sexp__089_
     | sexp__089_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag error_source__090_
           ["X"; "Y"; "Z"] sexp__089_ : Sexplib0.Sexp.t -> a__092_ variant)
;;

let _ = variant_of_sexp

let sexp_of_variant : 'a . 'a variant -> Sexplib0.Sexp.t =
  fun (type a__094_) ->
    (function
     | X -> Sexplib0.Sexp.Atom "X"
     | Y -> Sexplib0.Sexp.Atom "Y"
     | Z -> Sexplib0.Sexp.Atom "Z" : a__094_ variant -> Sexplib0.Sexp.t)
;;

let _ = sexp_of_variant

[@@@end]
[@@@ocamlformat "enable"]
