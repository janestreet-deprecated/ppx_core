(** AST traversal classes *)

(** To use these classes, inherit from them and override the methods corresponding to the
    types from [Parsetree] you want to process. For instance to collect all the string
    constants in a structure:

    {[
      let string_constants_of = object
        inherit [string list] Ast_traverse.fold as super

        method! expression e acc =
          let acc = super#expression e acc in
          match e.pexp_desc with
          | Pexp_constant (Const_string (s, _)) -> s :: acc
          | _ -> acc

        method! pattern p acc =
          let acc = super#pattern p acc in
          match p.ppat_desc with
          | Ppat_constant (Const_string (s, _)) -> s :: acc
          | _ -> acc
      end

      let string_constants_of_structure = string_constants_of#structure
    ]}
*)

class map                     : Ast_traverse_map.t
class iter                    : Ast_traverse_iter.t
class ['acc] fold             : ['acc] Ast_traverse_fold.t
class ['acc] fold_map         : ['acc] Ast_traverse_fold_map.t
class ['ctx] map_with_context : ['ctx] Ast_traverse_map_with_context.t
class map_with_path           : [string] map_with_context

(** Turn a [map] class into an [Ast_mapper.mapper] record.

    The resulting mapper is "closed", i.e. functions ignore their first argument. *)
val ast_mapper_of_map : #map -> Ast_mapper.mapper
