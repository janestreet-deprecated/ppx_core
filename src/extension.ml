open StdLabels
open MoreLabels

module String_map = Map.Make(String)

module Context = struct
  open Parsetree

  type 'a t =
    | Class_expr       : class_expr       t
    | Class_field      : class_field      t
    | Class_type       : class_type       t
    | Class_type_field : class_type_field t
    | Core_type        : core_type        t
    | Expression       : expression       t
    | Module_expr      : module_expr      t
    | Module_type      : module_type      t
    | Pattern          : pattern          t
    | Signature_item   : signature_item   t
    | Structure_item   : structure_item   t

  type packed = T : _ t -> packed

  let class_expr       = Class_expr
  let class_field      = Class_field
  let class_type       = Class_type
  let class_type_field = Class_type_field
  let core_type        = Core_type
  let expression       = Expression
  let module_expr      = Module_expr
  let module_type      = Module_type
  let pattern          = Pattern
  let signature_item   = Signature_item
  let structure_item   = Structure_item

  let desc : type a. a t -> string = function
    | Class_expr       -> "class expression"
    | Class_field      -> "class field"
    | Class_type       -> "class type"
    | Class_type_field -> "class type field"
    | Core_type        -> "core type"
    | Expression       -> "expression"
    | Module_expr      -> "module expression"
    | Module_type      -> "module type"
    | Pattern          -> "pattern"
    | Signature_item   -> "signature item"
    | Structure_item   -> "structure item"
end

type ('context, 'payload) t =
  { name    : string
  ; context : 'context Context.t
  ; payload : (Parsetree.payload, 'payload) Ast_pattern.Packed.t
  }

let convert t ext =
  Ast_pattern.Packed.parse t.payload (Common.loc_of_payload ext) (snd ext)
;;

let registrar =
  Name.Registrar.create
    ~kind:"extension"
    ~current_file:__FILE__
    ~string_of_context:(fun (Context.T ctx) -> Some (Context.desc ctx))
;;

let declare name context pattern k =
  Name.Registrar.register registrar (Context.T context) name;
  { name
  ; context
  ; payload = Ast_pattern.Packed.create pattern k
  }
;;

let convert ts (ext : Parsetree.extension) =
  let name = fst ext in
  match List.filter ts ~f:(fun t -> Name.matches ~pattern:t.name name.txt) with
  | [] -> None
  | [t] -> Some (convert t ext)
  | l ->
    Location.raise_errorf ~loc:name.loc
      "Multiple match for extensions: %s"
      (String.concat ~sep:", " (List.map l ~f:(fun t -> t.name)))
;;

let fail ctx (name, _) =
  Name.Registrar.raise_errorf registrar (Context.T ctx)
    "Extension `%s' was not translated" name
;;

let check_unused = object
  inherit Ast_traverse.iter as super

  method! extension (name, _) =
    Location.raise_errorf ~loc:name.loc
      "extension not expected here, Ppx_core.Std.Extension needs updating!"

  method! core_type_desc = function
    | Ptyp_extension ext -> fail Context.Core_type ext
    | x -> super#core_type_desc x

  method! pattern_desc = function
    | Ppat_extension ext -> fail Context.Pattern ext
    | x -> super#pattern_desc x

  method! expression_desc = function
    | Pexp_extension ext -> fail Context.Expression ext
    | x -> super#expression_desc x

  method! class_type_desc = function
    | Pcty_extension ext -> fail Context.Class_type ext
    | x -> super#class_type_desc x

  method! class_type_field_desc = function
    | Pctf_extension ext -> fail Context.Class_type_field ext
    | x -> super#class_type_field_desc x

  method! class_expr_desc = function
    | Pcl_extension ext -> fail Context.Class_expr ext
    | x -> super#class_expr_desc x

  method! class_field_desc = function
    | Pcf_extension ext -> fail Context.Class_field ext
    | x -> super#class_field_desc x

  method! module_type_desc = function
    | Pmty_extension ext -> fail Context.Module_type ext
    | x -> super#module_type_desc x

  method! signature_item_desc = function
    | Psig_extension (ext, _) -> fail Context.Signature_item ext
    | x -> super#signature_item_desc x

  method! module_expr_desc = function
    | Pmod_extension ext -> fail Context.Module_expr ext
    | x -> super#module_expr_desc x

  method! structure_item_desc = function
    | Pstr_extension (ext, _) -> fail Context.Structure_item ext
    | x -> super#structure_item_desc x
end
