open Base

[@@@warning "-37"]

module Nullary = struct
  type 'a t =
    | A
    | B
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let (t_sexp_grammar :
         'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a t Ppx_sexp_conv_lib.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { name_kind = Capitalized
          ; clauses =
              [ { name = "A"; clause_kind = Atom_clause }
              ; { name = "B"; clause_kind = Atom_clause }
              ]
          }
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module With_arguments = struct
  module With_sexp = struct
    type t =
      | A of int * int
      | B of string
    [@@deriving sexp_of]
  end

  type t = With_sexp.t =
    | A of int * int
    | B of string
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { name_kind = Capitalized
               ; clauses =
                   [ { name = "A"
                     ; clause_kind =
                         List_clause
                           { args =
                               Cons
                                 ( int_sexp_grammar.untyped
                                 , Cons (int_sexp_grammar.untyped, Empty) )
                           }
                     }
                   ; { name = "B"
                     ; clause_kind =
                         List_clause { args = Cons (string_sexp_grammar.untyped, Empty) }
                     }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  open Expect_test_helpers_base

  let%expect_test _ =
    print_s (With_sexp.sexp_of_t (A (1, 2)));
    print_s (With_sexp.sexp_of_t (B "foo"));
    [%expect {|
      (A 1 2)
      (B foo) |}]
  ;;
end

module Sexp_list = struct
  module With_sexp = struct
    type t =
      | Int of int
      | List of int list
      | Sexp_dot_list of int list [@sexp.list]
    [@@deriving sexp]
  end

  type t = With_sexp.t =
    | Int of int
    | List of int list
    | Sexp_dot_list of int list [@sexp.list]
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Ppx_sexp_conv_lib.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { name_kind = Capitalized
               ; clauses =
                   [ { name = "Int"
                     ; clause_kind =
                         List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                     }
                   ; { name = "List"
                     ; clause_kind =
                         List_clause
                           { args =
                               Cons ((list_sexp_grammar int_sexp_grammar).untyped, Empty)
                           }
                     }
                   ; { name = "Sexp_dot_list"
                     ; clause_kind = List_clause { args = Many int_sexp_grammar.untyped }
                     }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let (T : (With_sexp.t, t) Type_equal.t) = T

  open Expect_test_helpers_base

  let%expect_test _ =
    print_s (With_sexp.sexp_of_t (Int 1));
    List.iter [ []; [ 1 ]; [ 1; 2 ] ] ~f:(fun l ->
      print_s (With_sexp.sexp_of_t (List l));
      print_s (With_sexp.sexp_of_t (Sexp_dot_list l)));
    [%expect
      {|
      (Int 1)
      (List ())
      (Sexp_dot_list)
      (List (1))
      (Sexp_dot_list 1)
      (List (1 2))
      (Sexp_dot_list 1 2) |}]
  ;;
end
