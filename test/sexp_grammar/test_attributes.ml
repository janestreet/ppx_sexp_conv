open! Base

module type S = sig
  type t [@@deriving sexp_grammar]
end

let show_grammar (module M : S) =
  Expect_test_helpers_base.print_s ([%sexp_of: _ Sexp_grammar.t] [%sexp_grammar: M.t])
;;

module Grammarless = struct
  type t =
    [ `A
    | `B of string
    ]
end

let the_grammar = [%sexp_grammar: [ `A | `B of string ]]

let%expect_test "[@sexp_grammar.custom] in [@@deriving]" =
  show_grammar
    (module struct
      type t = (Grammarless.t[@sexp_grammar.custom the_grammar]) * int
      [@@deriving_inline sexp_grammar]

      let _ = fun (_ : t) -> ()

      let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
        { untyped =
            Lazy
              (lazy
                (List
                   (Cons
                      ( (the_grammar : Grammarless.t Sexplib0.Sexp_grammar.t).untyped
                      , Cons (int_sexp_grammar.untyped, Empty) ))))
        }
      ;;

      let _ = t_sexp_grammar

      [@@@end]
    end);
  [%expect
    {|
    (List (
      Cons
      (Variant (
        (case_sensitivity Case_sensitive)
        (clauses (
          (No_tag (
            (name        A)
            (clause_kind Atom_clause)))
          (No_tag (
            (name B) (clause_kind (List_clause (args (Cons String Empty))))))))))
      (Cons Integer Empty)))
    |}]
;;

let%expect_test "[@sexp_grammar.custom] in [%sexp_grammar]" =
  show_grammar
    (module struct
      type t = Grammarless.t * int

      let t_sexp_grammar =
        [%sexp_grammar: (Grammarless.t[@sexp_grammar.custom the_grammar]) * int]
      ;;
    end);
  [%expect
    {|
    (List (
      Cons
      (Variant (
        (case_sensitivity Case_sensitive)
        (clauses (
          (No_tag (
            (name        A)
            (clause_kind Atom_clause)))
          (No_tag (
            (name B) (clause_kind (List_clause (args (Cons String Empty))))))))))
      (Cons Integer Empty)))
    |}]
;;

let%expect_test "[@sexp_grammar.any] in [@@deriving]" =
  show_grammar
    (module struct
      type t =
        (Grammarless.t[@sexp_grammar.any "GRAMMARLESS"])
        * (Grammarless.t[@sexp_grammar.any])
      [@@deriving_inline sexp_grammar]

      let _ = fun (_ : t) -> ()

      let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
        { untyped = List (Cons (Any "GRAMMARLESS", Cons (Any "ANY", Empty))) }
      ;;

      let _ = t_sexp_grammar

      [@@@end]
    end);
  [%expect {| (List (Cons (Any GRAMMARLESS) (Cons (Any ANY) Empty))) |}]
;;

let%expect_test "[@sexp_grammar.any] in [%sexp_grammar]" =
  show_grammar
    (module struct
      type t = Grammarless.t * Grammarless.t

      let t_sexp_grammar =
        [%sexp_grammar:
          (Grammarless.t[@sexp_grammar.any "GRAMMARLESS"])
          * (Grammarless.t[@sexp_grammar.any])]
      ;;
    end);
  [%expect {| (List (Cons (Any GRAMMARLESS) (Cons (Any ANY) Empty))) |}]
;;
