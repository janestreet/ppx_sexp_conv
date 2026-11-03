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

      let t_sexp_grammar : t Sexplib0.Sexp_grammar.t =
        { untyped =
            List
              (Cons
                 ( (the_grammar : Grammarless.t Sexplib0.Sexp_grammar.t).untyped
                 , Cons (int_sexp_grammar.untyped, Empty) ))
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

module Custom_int : sig
  type t = int [@@deriving sexp_grammar]
end = struct
  type t = int

  let t_sexp_grammar : t Sexplib0.Sexp_grammar.t = { untyped = Any "CUSTOM_INT" }
end

let%expect_test "[@sexp.opaque] composes with [@sexp_grammar.custom]" =
  (* The converters are opaque while the grammar comes from the attribute. *)
  show_grammar
    (module struct
      type t = (Grammarless.t[@sexp.opaque] [@sexp_grammar.custom the_grammar])
      [@@deriving sexp_grammar, sexp_of]
    end);
  [%expect
    {|
    (Variant (
      (case_sensitivity Case_sensitive)
      (clauses (
        (No_tag (
          (name        A)
          (clause_kind Atom_clause)))
        (No_tag ((name B) (clause_kind (List_clause (args (Cons String Empty))))))))))
    |}]
;;

let%expect_test "[@sexp.custom] in [@@deriving]" =
  show_grammar
    (module struct
      type t = (int[@sexp.custom: Custom_int.t]) * int [@@deriving_inline sexp_grammar]

      let _ = fun (_ : t) -> ()

      let t_sexp_grammar : t Sexplib0.Sexp_grammar.t =
        { untyped =
            List
              (Cons
                 ( Custom_int.t_sexp_grammar.untyped
                 , Cons (int_sexp_grammar.untyped, Empty) ))
        }
      ;;

      let _ = t_sexp_grammar

      [@@@end]
    end);
  [%expect {| (List (Cons (Any CUSTOM_INT) (Cons Integer Empty))) |}]
;;

let%expect_test "[@sexp.custom] in [%sexp_grammar]" =
  show_grammar
    (module struct
      type t = int * int

      let t_sexp_grammar = [%sexp_grammar: (int[@sexp.custom: Custom_int.t]) * int]
    end);
  [%expect {| (List (Cons (Any CUSTOM_INT) (Cons Integer Empty))) |}]
;;

let%expect_test "[@sexp_grammar.any] in [@@deriving]" =
  show_grammar
    (module struct
      type t =
        (Grammarless.t[@sexp_grammar.any "GRAMMARLESS"])
        * (Grammarless.t[@sexp_grammar.any])
      [@@deriving_inline sexp_grammar]

      let _ = fun (_ : t) -> ()

      let t_sexp_grammar : t Sexplib0.Sexp_grammar.t =
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

let%expect_test "[@sexp.{array,bool,list,or_null,option}] in [@@deriving]" =
  show_grammar
    (module struct
      type t =
        { first : bool [@sexp.bool]
        ; second : string option [@sexp.option]
        ; third : string or_null [@sexp.or_null]
        ; fourth : int array [@sexp.array]
        ; fifth : int list [@sexp.list]
        }
      [@@deriving_inline sexp_grammar]

      let _ = fun (_ : t) -> ()

      let t_sexp_grammar : t Sexplib0.Sexp_grammar.t =
        let _must_type_check__002_ : _ =
          fun (first : bool) ->
          let _ : _ =
            (first
             : Stdlib.Bool.t)
             [@error_message "ppx_sexp_conv: expected [bool] or equivalent"]
          in
          ()
        and _must_type_check__003_ : _ =
          fun (second : string option) ->
          let _ : _ =
            (second
             : string Stdlib.Option.t)
             [@error_message "ppx_sexp_conv: expected [_ option] or equivalent"]
          in
          ()
        and _must_type_check__004_ : _ =
          fun (third : string or_null) ->
          let _ : _ =
            (third
             : string Basement.Or_null_shim.t)
             [@error_message "ppx_sexp_conv: expected [_ or_null] or equivalent"]
          in
          ()
        and _must_type_check__005_ : _ =
          fun (fourth : int array) ->
          let _ : _ =
            (fourth
             : int Stdlib.Array.t)
             [@error_message "ppx_sexp_conv: expected [_ array] or equivalent"]
          in
          ()
        and _must_type_check__006_ : _ =
          fun (fifth : int list) ->
          let _ : _ =
            (fifth
             : int Stdlib.List.t)
             [@error_message "ppx_sexp_conv: expected [_ list] or equivalent"]
          in
          ()
        in
        { untyped =
            List
              (Fields
                 { allow_extra_fields = false
                 ; fields =
                     [ No_tag { name = "first"; required = false; args = Empty }
                     ; No_tag
                         { name = "second"
                         ; required = false
                         ; args = Cons (string_sexp_grammar.untyped, Empty)
                         }
                     ; No_tag
                         { name = "third"
                         ; required = false
                         ; args = Cons (string_sexp_grammar.untyped, Empty)
                         }
                     ; No_tag
                         { name = "fourth"
                         ; required = false
                         ; args = Cons (List (Many int_sexp_grammar.untyped), Empty)
                         }
                     ; No_tag
                         { name = "fifth"
                         ; required = false
                         ; args = Cons (List (Many int_sexp_grammar.untyped), Empty)
                         }
                     ]
                 })
        }
      ;;

      let _ = t_sexp_grammar

      [@@@end]
    end);
  [%expect
    {|
    (List (
      Fields (
        (allow_extra_fields false)
        (fields (
          (No_tag (
            (name     first)
            (required false)
            (args     Empty)))
          (No_tag (
            (name     second)
            (required false)
            (args (Cons String Empty))))
          (No_tag (
            (name     third)
            (required false)
            (args (Cons String Empty))))
          (No_tag (
            (name     fourth)
            (required false)
            (args (Cons (List (Many Integer)) Empty))))
          (No_tag (
            (name     fifth)
            (required false)
            (args (Cons (List (Many Integer)) Empty)))))))))
    |}]
;;
