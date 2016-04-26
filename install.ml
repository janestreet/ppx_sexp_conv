#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_sexp_conv"
  [ oasis_lib "ppx_sexp_conv"
  ; oasis_lib "ppx_sexp_conv_expander"
  ; file "META" ~section:"lib"
  ]
