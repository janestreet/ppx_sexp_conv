open! Base

(* Printing the raw grammar should be a last resort when there is no better way to test
   the ppx (e.g., [@@deriving_inline _]). The output is illegible and fragile. *)

let test raw_grammar =
  Sexp_grammar_validation.Raw_grammar.sexp_of_t raw_grammar |> Stdio.print_s
;;

let%expect_test "polymorphic" =
  test [%sexp_grammar: < for_all : 'k 'v. ('k * 'v) list > ];
  [%expect
    {|
    ((generic_groups
      ((6a84293b8771489e87de480107049eda
        ((tycon_names (list))
         (types
          ((t
            (Tyvar_parameterize (a)
             (Tyvar_instantiate (Tycon_index 0) ((Tyvar_index 0)))))))))
       (90163c5a0ec60eaf19da04c7fc7e1f3d
        ((tycon_names (List.t))
         (types
          ((list
            (Tyvar_parameterize (a)
             (Tyvar_instantiate (Tycon_index 0) ((Tyvar_index 0)))))))))
       (c235a370b5135a2c190baecfd2bd77e7
        ((tycon_names (list))
         (types
          ((dummy_type_name_from_sexp_grammar
            (Tyvar_parameterize (k v)
             (Tyvar_instantiate (Tycon_index 0)
              ((List ((One (Tyvar_index 0)) (One (Tyvar_index 1))))))))))))))
     (groups
      ((0
        ((generic_group 6a84293b8771489e87de480107049eda) (origin list.ml.T)
         (instantiate_tycons
          ((Inline (Tyvar_parameterize ('a) (List ((Many (Tyvar_index 0))))))))))
       (1
        ((generic_group 90163c5a0ec60eaf19da04c7fc7e1f3d) (origin base.ml.Export)
         (instantiate_tycons ((Ref t 0)))))
       (2
        ((generic_group c235a370b5135a2c190baecfd2bd77e7)
         (origin test_percent_sexp_grammar.ml)
         (instantiate_tycons ((Ref list 1)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 2))) |}]
;;

let%expect_test "primitive" =
  test [%sexp_grammar: int];
  [%expect
    {|
    ((generic_groups
      ((926517f9eb65458b638457c38981eb19
        ((tycon_names (int)) (types ((t (Tycon_index 0))))))
       (9f9fc55ea55deca5e5a55238a9e14814
        ((tycon_names (Int.t)) (types ((int (Tycon_index 0))))))
       (fbc7a39b4d9d260c4fa39e5250052c77
        ((tycon_names (int))
         (types ((dummy_type_name_from_sexp_grammar (Tycon_index 0))))))))
     (groups
      ((3
        ((generic_group 926517f9eb65458b638457c38981eb19) (origin int.ml.T)
         (instantiate_tycons ((Inline (Atom Int))))))
       (4
        ((generic_group 9f9fc55ea55deca5e5a55238a9e14814) (origin base.ml.Export)
         (instantiate_tycons ((Ref t 3)))))
       (5
        ((generic_group fbc7a39b4d9d260c4fa39e5250052c77)
         (origin test_percent_sexp_grammar.ml)
         (instantiate_tycons ((Ref int 4)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 5))) |}]
;;

let%expect_test "application of polymorphic type constructor" =
  test [%sexp_grammar: int list];
  [%expect
    {|
    ((generic_groups
      ((6a84293b8771489e87de480107049eda
        ((tycon_names (list))
         (types
          ((t
            (Tyvar_parameterize (a)
             (Tyvar_instantiate (Tycon_index 0) ((Tyvar_index 0)))))))))
       (90163c5a0ec60eaf19da04c7fc7e1f3d
        ((tycon_names (List.t))
         (types
          ((list
            (Tyvar_parameterize (a)
             (Tyvar_instantiate (Tycon_index 0) ((Tyvar_index 0)))))))))
       (926517f9eb65458b638457c38981eb19
        ((tycon_names (int)) (types ((t (Tycon_index 0))))))
       (9f9fc55ea55deca5e5a55238a9e14814
        ((tycon_names (Int.t)) (types ((int (Tycon_index 0))))))
       (b796e27d2ccfcadf2fd17396632c6ef8
        ((tycon_names (int list))
         (types
          ((dummy_type_name_from_sexp_grammar
            (Tyvar_instantiate (Tycon_index 1) ((Tycon_index 0))))))))))
     (groups
      ((0
        ((generic_group 6a84293b8771489e87de480107049eda) (origin list.ml.T)
         (instantiate_tycons
          ((Inline (Tyvar_parameterize ('a) (List ((Many (Tyvar_index 0))))))))))
       (1
        ((generic_group 90163c5a0ec60eaf19da04c7fc7e1f3d) (origin base.ml.Export)
         (instantiate_tycons ((Ref t 0)))))
       (3
        ((generic_group 926517f9eb65458b638457c38981eb19) (origin int.ml.T)
         (instantiate_tycons ((Inline (Atom Int))))))
       (4
        ((generic_group 9f9fc55ea55deca5e5a55238a9e14814) (origin base.ml.Export)
         (instantiate_tycons ((Ref t 3)))))
       (6
        ((generic_group b796e27d2ccfcadf2fd17396632c6ef8)
         (origin test_percent_sexp_grammar.ml)
         (instantiate_tycons ((Ref int 4) (Ref list 1)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 6))) |}]
;;

let%expect_test "arrow type / original polymorphic type syntax" =
  test [%sexp_grammar: 'k -> 'v -> ('k * 'v) list];
  [%expect
    {|
    ((generic_groups
      ((ddc522722eb44f32c55594e2d02c83b6
        ((tycon_names ())
         (types
          ((dummy_type_name_from_sexp_grammar (Grammar (Inline (Union ()))))))))))
     (groups
      ((7
        ((generic_group ddc522722eb44f32c55594e2d02c83b6)
         (origin test_percent_sexp_grammar.ml) (instantiate_tycons ())))))
     (start (Ref dummy_type_name_from_sexp_grammar 7))) |}]
;;
