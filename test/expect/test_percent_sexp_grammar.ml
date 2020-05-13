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
      (("j\132);\135qH\158\135\222H\001\007\004\158\218"
        ((implicit_vars (list))
         (types
          ((t (Explicit_bind (a) (Apply (Implicit_var 0) ((Explicit_var 0)))))))))
       ("\144\022<Z\014\198\014\175\025\218\004\199\252~\031="
        ((implicit_vars (List.t))
         (types
          ((list (Explicit_bind (a) (Apply (Implicit_var 0) ((Explicit_var 0)))))))))
       ("\1945\163p\181\019Z,\025\011\174\207\210\189w\231"
        ((implicit_vars (list))
         (types
          ((dummy_type_name_from_sexp_grammar
            (Explicit_bind (k v)
             (Apply (Implicit_var 0)
              ((List ((One (Explicit_var 0)) (One (Explicit_var 1))))))))))))))
     (groups
      ((0
        ((generic_group "j\132);\135qH\158\135\222H\001\007\004\158\218")
         (origin list.ml.T)
         (apply_implicit
          ((Inline (Explicit_bind ('a) (List ((Many (Explicit_var 0))))))))))
       (1
        ((generic_group "\144\022<Z\014\198\014\175\025\218\004\199\252~\031=")
         (origin base.ml.Export) (apply_implicit ((Ref t 0)))))
       (2
        ((generic_group "\1945\163p\181\019Z,\025\011\174\207\210\189w\231")
         (origin test_percent_sexp_grammar.ml) (apply_implicit ((Ref list 1)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 2))) |}]
;;

let%expect_test "primitive" =
  test [%sexp_grammar: int];
  [%expect
    {|
    ((generic_groups
      (("\146e\023\249\235eE\139c\132W\195\137\129\235\025"
        ((implicit_vars (int)) (types ((t (Implicit_var 0))))))
       ("\159\159\197^\165]\236\165\229\165R8\169\225H\020"
        ((implicit_vars (Int.t)) (types ((int (Implicit_var 0))))))
       ("\251\199\163\155M\157&\012O\163\158RP\005,w"
        ((implicit_vars (int))
         (types ((dummy_type_name_from_sexp_grammar (Implicit_var 0))))))))
     (groups
      ((3
        ((generic_group "\146e\023\249\235eE\139c\132W\195\137\129\235\025")
         (origin int.ml.T) (apply_implicit ((Inline (Atom Int))))))
       (4
        ((generic_group "\159\159\197^\165]\236\165\229\165R8\169\225H\020")
         (origin base.ml.Export) (apply_implicit ((Ref t 3)))))
       (5
        ((generic_group "\251\199\163\155M\157&\012O\163\158RP\005,w")
         (origin test_percent_sexp_grammar.ml) (apply_implicit ((Ref int 4)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 5))) |}]
;;

let%expect_test "application of polymorphic type constructor" =
  test [%sexp_grammar: int list];
  [%expect
    {|
    ((generic_groups
      (("j\132);\135qH\158\135\222H\001\007\004\158\218"
        ((implicit_vars (list))
         (types
          ((t (Explicit_bind (a) (Apply (Implicit_var 0) ((Explicit_var 0)))))))))
       ("\144\022<Z\014\198\014\175\025\218\004\199\252~\031="
        ((implicit_vars (List.t))
         (types
          ((list (Explicit_bind (a) (Apply (Implicit_var 0) ((Explicit_var 0)))))))))
       ("\146e\023\249\235eE\139c\132W\195\137\129\235\025"
        ((implicit_vars (int)) (types ((t (Implicit_var 0))))))
       ("\159\159\197^\165]\236\165\229\165R8\169\225H\020"
        ((implicit_vars (Int.t)) (types ((int (Implicit_var 0))))))
       ("\183\150\226},\207\202\223/\209s\150c,n\248"
        ((implicit_vars (int list))
         (types
          ((dummy_type_name_from_sexp_grammar
            (Apply (Implicit_var 1) ((Implicit_var 0))))))))))
     (groups
      ((0
        ((generic_group "j\132);\135qH\158\135\222H\001\007\004\158\218")
         (origin list.ml.T)
         (apply_implicit
          ((Inline (Explicit_bind ('a) (List ((Many (Explicit_var 0))))))))))
       (1
        ((generic_group "\144\022<Z\014\198\014\175\025\218\004\199\252~\031=")
         (origin base.ml.Export) (apply_implicit ((Ref t 0)))))
       (3
        ((generic_group "\146e\023\249\235eE\139c\132W\195\137\129\235\025")
         (origin int.ml.T) (apply_implicit ((Inline (Atom Int))))))
       (4
        ((generic_group "\159\159\197^\165]\236\165\229\165R8\169\225H\020")
         (origin base.ml.Export) (apply_implicit ((Ref t 3)))))
       (6
        ((generic_group "\183\150\226},\207\202\223/\209s\150c,n\248")
         (origin test_percent_sexp_grammar.ml)
         (apply_implicit ((Ref int 4) (Ref list 1)))))))
     (start (Ref dummy_type_name_from_sexp_grammar 6))) |}]
;;

let%expect_test "arrow type / original polymorphic type syntax" =
  test [%sexp_grammar: 'k -> 'v -> ('k * 'v) list];
  [%expect
    {|
    ((generic_groups
      (("\221\197\"r.\180O2\197U\148\226\208,\131\182"
        ((implicit_vars ())
         (types
          ((dummy_type_name_from_sexp_grammar (Grammar (Inline (Union ()))))))))))
     (groups
      ((7
        ((generic_group "\221\197\"r.\180O2\197U\148\226\208,\131\182")
         (origin test_percent_sexp_grammar.ml) (apply_implicit ())))))
     (start (Ref dummy_type_name_from_sexp_grammar 7))) |}]
;;
