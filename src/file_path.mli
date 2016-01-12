open Parsetree

(** Return the path used as root in a file *)

(** For instance, inside a sub-module [M] in a file "foo.ml", the path will be
    "foo.ml.M" *)

val get_default_path     : Location.t -> string
val get_default_path_str : structure  -> string
val get_default_path_sig : signature  -> string
