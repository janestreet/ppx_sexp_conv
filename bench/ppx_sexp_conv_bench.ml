open! Base

let bench_sexp_of_t ~sexp_of_t t =
  let t = Sys.opaque_identity t in
  fun () -> sexp_of_t t
;;

let bench_t_of_sexp ~t_of_sexp string =
  let sexp = Sys.opaque_identity (Parsexp.Single.parse_string_exn string) in
  fun () -> t_of_sexp sexp
;;

let%bench_module "Record" =
  (module struct
    type t =
      { a : int
      ; b : int option [@omit_nil]
      ; c : bool [@sexp.bool]
      ; d : int array [@sexp.array]
      ; e : int list [@sexp.list]
      ; f : int option [@sexp.option]
      ; g : int [@default 0] [@sexp_drop_default ( = )]
      ; h : int [@default 0] [@sexp_drop_default.compare]
      ; i : int [@default 0] [@sexp_drop_default.equal]
      ; j : int [@default 0] [@sexp_drop_default.sexp]
      ; k : 'a. 'a list
      }
    [@@deriving sexp]

    let%bench_fun "sexp_of_t, full" =
      bench_sexp_of_t
        ~sexp_of_t
        { a = 1
        ; b = Some 2
        ; c = true
        ; d = [| 3; 4 |]
        ; e = [ 5; 6 ]
        ; f = Some 7
        ; g = 8
        ; h = 9
        ; i = 10
        ; j = 11
        ; k = []
        }
    ;;

    let%bench_fun "sexp_of_t, empty" =
      bench_sexp_of_t
        ~sexp_of_t
        { a = 0
        ; b = None
        ; c = false
        ; d = [||]
        ; e = []
        ; f = None
        ; g = 0
        ; h = 0
        ; i = 0
        ; j = 0
        ; k = []
        }
    ;;

    let%bench_fun "t_of_sexp, full, in order" =
      bench_t_of_sexp
        ~t_of_sexp
        "((a 1) (b (2)) (c) (d (3 4)) (e (5 6)) (f 7) (g 8) (h 9) (i 10) (j 11) (k ()))"
    ;;

    let%bench_fun "t_of_sexp, full, reverse order" =
      bench_t_of_sexp
        ~t_of_sexp
        "((k ()) (j 11) (i 10) (h 9) (g 8) (f 7) (e (5 6)) (d (3 4)) (c) (b (2)) (a 1))"
    ;;

    let%bench_fun "t_of_sexp, empty" = bench_t_of_sexp ~t_of_sexp "((a 0) (k ()))"
  end)
;;

let%bench_module "Variant" =
  (module struct
    type t =
      | Atomic
      | Tuple of int * string
      | List of int list [@sexp.list]
      | Record of
          { a : int
          ; b : int option [@omit_nil]
          ; c : bool [@sexp.bool]
          ; d : int array [@sexp.array]
          ; e : int list [@sexp.list]
          ; f : int option [@sexp.option]
          ; g : int [@default 0] [@sexp_drop_default ( = )]
          ; h : int [@default 0] [@sexp_drop_default.compare]
          ; i : int [@default 0] [@sexp_drop_default.equal]
          ; j : int [@default 0] [@sexp_drop_default.sexp]
          ; k : 'a. 'a list
          }
    [@@deriving sexp]

    let%bench_fun "sexp_of_t, atomic" = bench_sexp_of_t ~sexp_of_t Atomic
    let%bench_fun "sexp_of_t, tuple" = bench_sexp_of_t ~sexp_of_t (Tuple (1, "hello"))
    let%bench_fun "sexp_of_t, list, full" = bench_sexp_of_t ~sexp_of_t (List [ 1; 2 ])
    let%bench_fun "sexp_of_t, list, empty" = bench_sexp_of_t ~sexp_of_t (List [])

    let%bench_fun "sexp_of_t, record, full" =
      bench_sexp_of_t
        ~sexp_of_t
        (Record
           { a = 1
           ; b = Some 2
           ; c = true
           ; d = [| 3; 4 |]
           ; e = [ 5; 6 ]
           ; f = Some 7
           ; g = 8
           ; h = 9
           ; i = 10
           ; j = 11
           ; k = []
           })
    ;;

    let%bench_fun "sexp_of_t, record, empty" =
      bench_sexp_of_t
        ~sexp_of_t
        (Record
           { a = 0
           ; b = None
           ; c = false
           ; d = [||]
           ; e = []
           ; f = None
           ; g = 0
           ; h = 0
           ; i = 0
           ; j = 0
           ; k = []
           })
    ;;

    let%bench_fun "t_of_sexp, atomic" = bench_t_of_sexp ~t_of_sexp "Atomic"
    let%bench_fun "t_of_sexp, tuple" = bench_t_of_sexp ~t_of_sexp "(Tuple 1 hello)"
    let%bench_fun "t_of_sexp, list, full" = bench_t_of_sexp ~t_of_sexp "(List 1 2)"
    let%bench_fun "t_of_sexp, list, empty" = bench_t_of_sexp ~t_of_sexp "(List)"

    let%bench_fun "t_of_sexp, record, full, in order" =
      bench_t_of_sexp
        ~t_of_sexp
        "(Record (a 1) (b (2)) (c) (d (3 4)) (e (5 6)) (f 7) (g 8) (h 9) (i 10) (j 11) \
         (k ()))"
    ;;

    let%bench_fun "t_of_sexp, record, full, reverse order" =
      bench_t_of_sexp
        ~t_of_sexp
        "(Record (k ()) (j 11) (i 10) (h 9) (g 8) (f 7) (e (5 6)) (d (3 4)) (c) (b (2)) \
         (a 1))"
    ;;

    let%bench_fun "t_of_sexp, record, empty" =
      bench_t_of_sexp ~t_of_sexp "(Record (a 0) (k ()))"
    ;;
  end)
;;

let%bench_module "Tag" =
  (module struct
    type t =
      [ `Atomic
      | `Tuple of int * string
      | `List of int list [@sexp.list]
      ]
    [@@deriving sexp]

    let%bench_fun "sexp_of_t, atomic" = bench_sexp_of_t ~sexp_of_t `Atomic
    let%bench_fun "sexp_of_t, tuple" = bench_sexp_of_t ~sexp_of_t (`Tuple (1, "hello"))
    let%bench_fun "sexp_of_t, list, full" = bench_sexp_of_t ~sexp_of_t (`List [ 1; 2 ])
    let%bench_fun "sexp_of_t, list, empty" = bench_sexp_of_t ~sexp_of_t (`List [])
    let%bench_fun "t_of_sexp, atomic" = bench_t_of_sexp ~t_of_sexp "Atomic"
    let%bench_fun "t_of_sexp, tuple" = bench_t_of_sexp ~t_of_sexp "(Tuple (1 hello))"
    let%bench_fun "t_of_sexp, list, full" = bench_t_of_sexp ~t_of_sexp "(List 1 2)"
    let%bench_fun "t_of_sexp, list, empty" = bench_t_of_sexp ~t_of_sexp "(List)"
  end)
;;

let%bench_module "Inherit" =
  (module struct
    type atomic = [ `Atomic ] [@@deriving sexp]
    type tuple = [ `Tuple of int * string ] [@@deriving sexp]
    type listed = [ `List of int list [@sexp.list] ] [@@deriving sexp]

    type t =
      [ atomic
      | tuple
      | listed
      ]
    [@@deriving sexp]

    let%bench_fun "sexp_of_t, atomic" = bench_sexp_of_t ~sexp_of_t `Atomic
    let%bench_fun "sexp_of_t, tuple" = bench_sexp_of_t ~sexp_of_t (`Tuple (1, "hello"))
    let%bench_fun "sexp_of_t, list, full" = bench_sexp_of_t ~sexp_of_t (`List [ 1; 2 ])
    let%bench_fun "sexp_of_t, list, empty" = bench_sexp_of_t ~sexp_of_t (`List [])
    let%bench_fun "t_of_sexp, atomic" = bench_t_of_sexp ~t_of_sexp "Atomic"
    let%bench_fun "t_of_sexp, tuple" = bench_t_of_sexp ~t_of_sexp "(Tuple (1 hello))"
    let%bench_fun "t_of_sexp, list, full" = bench_t_of_sexp ~t_of_sexp "(List 1 2)"
    let%bench_fun "t_of_sexp, list, empty" = bench_t_of_sexp ~t_of_sexp "(List)"
  end)
;;
