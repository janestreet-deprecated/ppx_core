#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_core"
  [ oasis_lib "ppx_core"
  ; file "META" ~section:"lib"
  ]
