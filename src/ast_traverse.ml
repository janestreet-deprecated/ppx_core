class map = Ast_traverse_map.t
class iter = Ast_traverse_iter.t
class ['acc] fold = ['acc] Ast_traverse_fold.t
class ['acc] fold_map = ['acc] Ast_traverse_fold_map.t
class ['ctx] map_with_context = ['ctx] Ast_traverse_map_with_context.t

let enter name path = if path = "" then name else path ^ "." ^ name

class map_with_path = object
  inherit [string] map_with_context as super

  method! structure_item_desc path x =
    match x with
    | Pstr_module mb -> super#structure_item_desc (enter mb.pmb_name.txt path) x
    | _ -> super#structure_item_desc path x

  method! module_declaration path md =
    super#module_declaration (enter md.pmd_name.txt path) md

  method! module_type_declaration path mtd =
    super#module_type_declaration (enter mtd.pmtd_name.txt path) mtd
end

let ast_mapper_of_map (map : #map) : Ast_mapper.mapper =
  let open Ast_mapper in
  let mk f = fun (_ : Ast_mapper.mapper) -> f in
  { attribute               = mk map#attribute
  ; attributes              = mk map#attributes
  ; case                    = mk map#case
  ; cases                   = mk (map#list map#case)
  ; class_declaration       = mk map#class_declaration
  ; class_description       = mk map#class_description
  ; class_expr              = mk map#class_expr
  ; class_field             = mk map#class_field
  ; class_signature         = mk map#class_signature
  ; class_structure         = mk map#class_structure
  ; class_type              = mk map#class_type
  ; class_type_declaration  = mk map#class_type_declaration
  ; class_type_field        = mk map#class_type_field
  ; constructor_declaration = mk map#constructor_declaration
  ; expr                    = mk map#expression
  ; extension               = mk map#extension
  ; extension_constructor   = mk map#extension_constructor
  ; include_declaration     = mk map#include_declaration
  ; include_description     = mk map#include_description
  ; label_declaration       = mk map#label_declaration
  ; location                = mk map#location
  ; module_binding          = mk map#module_binding
  ; module_declaration      = mk map#module_declaration
  ; module_expr             = mk map#module_expr
  ; module_type             = mk map#module_type
  ; module_type_declaration = mk map#module_type_declaration
  ; open_description        = mk map#open_description
  ; pat                     = mk map#pattern
  ; payload                 = mk map#payload
  ; signature               = mk map#signature
  ; signature_item          = mk map#signature_item
  ; structure               = mk map#structure
  ; structure_item          = mk map#structure_item
  ; typ                     = mk map#core_type
  ; type_declaration        = mk map#type_declaration
  ; type_extension          = mk map#type_extension
  ; type_kind               = mk map#type_kind
  ; value_binding           = mk map#value_binding
  ; value_description       = mk map#value_description
  ; with_constraint         = mk map#with_constraint
  }
