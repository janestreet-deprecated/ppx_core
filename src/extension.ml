open StdLabels
open MoreLabels
open Common

module String_map = Map.Make(String)

type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

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

  let eq : type a b. a t -> b t -> (a, b) equality = fun a b ->
    match a, b with
    | Class_expr       , Class_expr       -> Eq
    | Class_field      , Class_field      -> Eq
    | Class_type       , Class_type       -> Eq
    | Class_type_field , Class_type_field -> Eq
    | Core_type        , Core_type        -> Eq
    | Expression       , Expression       -> Eq
    | Module_expr      , Module_expr      -> Eq
    | Module_type      , Module_type      -> Eq
    | Pattern          , Pattern          -> Eq
    | Signature_item   , Signature_item   -> Eq
    | Structure_item   , Structure_item   -> Eq
    | _ -> assert (T a <> T b); Ne

  let get_extension : type a. a t -> a -> (extension * attributes) option = fun t x ->
    match t, x with
    | Class_expr       , {pcl_desc =Pcl_extension  e; pcl_attributes =a;_} -> Some (e, a)
    | Class_field      , {pcf_desc =Pcf_extension  e; pcf_attributes =a;_} -> Some (e, a)
    | Class_type       , {pcty_desc=Pcty_extension e; pcty_attributes=a;_} -> Some (e, a)
    | Class_type_field , {pctf_desc=Pctf_extension e; pctf_attributes=a;_} -> Some (e, a)
    | Core_type        , {ptyp_desc=Ptyp_extension e; ptyp_attributes=a;_} -> Some (e, a)
    | Expression       , {pexp_desc=Pexp_extension e; pexp_attributes=a;_} -> Some (e, a)
    | Module_expr      , {pmod_desc=Pmod_extension e; pmod_attributes=a;_} -> Some (e, a)
    | Module_type      , {pmty_desc=Pmty_extension e; pmty_attributes=a;_} -> Some (e, a)
    | Pattern          , {ppat_desc=Ppat_extension e; ppat_attributes=a;_} -> Some (e, a)
    | Signature_item   , {psig_desc=Psig_extension(e, a)               ;_} -> Some (e, a)
    | Structure_item   , {pstr_desc=Pstr_extension(e, a)               ;_} -> Some (e, a)
    | _ -> None

  let merge_attributes : type a. a t -> a -> attributes -> a = fun t x attrs ->
    match t with
    | Class_expr       -> { x with pcl_attributes  = x.pcl_attributes  @ attrs }
    | Class_field      -> { x with pcf_attributes  = x.pcf_attributes  @ attrs }
    | Class_type       -> { x with pcty_attributes = x.pcty_attributes @ attrs }
    | Class_type_field -> { x with pctf_attributes = x.pctf_attributes @ attrs }
    | Core_type        -> { x with ptyp_attributes = x.ptyp_attributes @ attrs }
    | Expression       -> { x with pexp_attributes = x.pexp_attributes @ attrs }
    | Module_expr      -> { x with pmod_attributes = x.pmod_attributes @ attrs }
    | Module_type      -> { x with pmty_attributes = x.pmty_attributes @ attrs }
    | Pattern          -> { x with ppat_attributes = x.ppat_attributes @ attrs }
    | Signature_item   -> assert_no_attributes attrs; x
    | Structure_item   -> assert_no_attributes attrs; x
end

let registrar =
  Name.Registrar.create
    ~kind:"extension"
    ~current_file:__FILE__
    ~string_of_context:(fun (Context.T ctx) -> Some (Context.desc ctx))
;;

module Make(Callback : sig type 'a t end) = struct

  type ('a, 'b) payload_parser =
      Payload_parser : ('a, 'b, 'c) Ast_pattern.t * 'b Callback.t
      -> ('a, 'c) payload_parser

  type ('context, 'payload) t =
    { name     : string
    ; context  : 'context Context.t
    ; payload  : (Parsetree.payload, 'payload) payload_parser
    }

  let declare name context pattern k =
    Name.Registrar.register registrar (Context.T context) name;
    { name
    ; context
    ; payload = Payload_parser (pattern, k)
    }
  ;;

  let find ts (ext : Parsetree.extension) =
    let name = fst ext in
    match List.filter ts ~f:(fun t -> Name.matches ~pattern:t.name name.txt) with
    | [] -> None
    | [t] -> Some t
    | l ->
      Location.raise_errorf ~loc:name.loc
        "Multiple match for extensions: %s"
        (String.concat ~sep:", " (List.map l ~f:(fun t -> t.name)))
  ;;
end

module Expert = struct
  include Make(struct type 'a t = 'a end)

  let convert ts ~loc ext =
    match find ts ext with
    | None -> None
    | Some { payload = Payload_parser (pattern, f); _ } ->
      Some (Ast_pattern.parse pattern loc (snd ext) f)
end
include Expert
let convert ts ext = convert ts ~loc:(Common.loc_of_payload ext) ext

module V2 = struct
  module M = Make(struct type 'a t = loc:Location.t -> path:string -> 'a end)

  type 'a result =
    | Simple of 'a
    | Inline of 'a list

  type 'a unpacked = ('a, 'a result) M.t

  type t = T : _ unpacked -> t

  let declare name context pattern k =
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Simple x) in
    T (M.declare name context pattern k)
  ;;

  let check_context_for_inline : type a. a Context.t -> unit = function
    | Context.Class_field      -> ()
    | Context.Class_type_field -> ()
    | Context.Signature_item   -> ()
    | Context.Structure_item   -> ()
    | context ->
      Printf.ksprintf invalid_arg "Extension.V2.declare_inline: %s can't be inlined"
        (Context.desc context)
  ;;

  let declare_inline name context pattern k =
    check_context_for_inline context;
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Inline x) in
    T (M.declare name context pattern k)
  ;;

  let rec filter_by_context
    : type a. a Context.t -> t list -> a unpacked list =
    fun context expanders ->
      match expanders with
      | [] -> []
      | T t :: rest ->
        match Context.eq context t.context with
        | Eq -> t :: filter_by_context context rest
        | Ne ->      filter_by_context context rest
  ;;

  let convert_no_inline ts ~loc ~path ext =
    match M.find ts ext with
    | None -> None
    | Some { payload = M.Payload_parser (pattern, f); _  } ->
      match Ast_pattern.parse pattern loc (snd ext) (f ~loc ~path) with
      | Simple x -> Some x
      | Inline _ -> failwith "Extension.V2.convert_no_inline"
  ;;

  let rec map_node context ts super_call loc path x =
    match Context.get_extension context x with
    | None -> super_call path x
    | Some (ext, attrs) ->
      match convert_no_inline ts ~loc ~path ext with
      | None -> super_call path x
      | Some x ->
        map_node context ts super_call loc path (Context.merge_attributes context x attrs)
  ;;

  let rec map_nodes context ts super_call get_loc path l =
    match l with
    | [] -> []
    | x :: l ->
      match Context.get_extension context x with
      | None ->
        (* These two lets force the evaluation order, so that errors are reported in the
           same order as they appear in the source file. *)
        let x = super_call path x in
        let l = map_nodes context ts super_call get_loc path l in
        x :: l
      | Some (ext, attrs) ->
        match M.find ts ext with
        | None ->
          let x = super_call path x in
          let l = map_nodes context ts super_call get_loc path l in
          x :: l
        | Some { payload = M.Payload_parser (pattern, f); _ } ->
          assert_no_attributes attrs;
          let loc = get_loc x in
          let result = Ast_pattern.parse pattern loc (snd ext) (f ~loc ~path) in
          map_nodes context ts super_call get_loc path
            (match result with
             | Simple x -> x :: l
             | Inline x -> x @ l)
  ;;

  class map_top_down expanders =
    let class_expr       = filter_by_context Class_expr       expanders
    and class_field      = filter_by_context Class_field      expanders
    and class_type       = filter_by_context Class_type       expanders
    and class_type_field = filter_by_context Class_type_field expanders
    and core_type        = filter_by_context Core_type        expanders
    and expression       = filter_by_context Expression       expanders
    and module_expr      = filter_by_context Module_expr      expanders
    and module_type      = filter_by_context Module_type      expanders
    and pattern          = filter_by_context Pattern          expanders
    and signature_item   = filter_by_context Signature_item   expanders
    and structure_item   = filter_by_context Structure_item   expanders
    in
    object(self)
      inherit Ast_traverse.map_with_path as super

      method! core_type path x =
        map_node Core_type core_type super#core_type x.ptyp_loc path x

      method! pattern path x =
        map_node Pattern pattern super#pattern x.ppat_loc path x

      method! expression path x =
        map_node Expression expression super#expression x.pexp_loc path x

      method! class_type path x =
        map_node Class_type class_type super#class_type x.pcty_loc path x

      method! class_type_field path x =
        map_node Class_type_field class_type_field super#class_type_field
          x.pctf_loc path x

      method! class_expr path x =
        map_node Class_expr class_expr super#class_expr x.pcl_loc path x

      method! class_field path x =
        map_node Class_field class_field super#class_field x.pcf_loc path x

      method! module_type path x =
        map_node Module_type module_type super#module_type x.pmty_loc path x

      method! module_expr path x =
        map_node Module_expr module_expr super#module_expr x.pmod_loc path x

      method! structure_item path x =
        map_node Structure_item structure_item super#structure_item x.pstr_loc path x

      method! signature_item path x =
        map_node Signature_item signature_item super#signature_item x.psig_loc path x

      method! class_structure path { pcstr_self; pcstr_fields } =
        let pcstr_self = self#pattern path pcstr_self in
        let pcstr_fields =
          map_nodes Class_field class_field super#class_field
            (fun x -> x.pcf_loc) path pcstr_fields
        in
        { pcstr_self; pcstr_fields }

      method! class_signature path { pcsig_self; pcsig_fields } =
        let pcsig_self = self#core_type path pcsig_self in
        let pcsig_fields =
          map_nodes Class_type_field class_type_field super#class_type_field
            (fun x -> x.pctf_loc) path pcsig_fields
        in
        { pcsig_self; pcsig_fields }

      method! structure path x =
        map_nodes Structure_item structure_item super#structure_item
          (fun x -> x.pstr_loc) path x

      method! signature path x =
        map_nodes Signature_item signature_item super#signature_item
          (fun x -> x.psig_loc) path x
    end
end

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
    | Ptyp_extension ext -> fail Core_type ext
    | x -> super#core_type_desc x

  method! pattern_desc = function
    | Ppat_extension ext -> fail Pattern ext
    | x -> super#pattern_desc x

  method! expression_desc = function
    | Pexp_extension ext -> fail Expression ext
    | x -> super#expression_desc x

  method! class_type_desc = function
    | Pcty_extension ext -> fail Class_type ext
    | x -> super#class_type_desc x

  method! class_type_field_desc = function
    | Pctf_extension ext -> fail Class_type_field ext
    | x -> super#class_type_field_desc x

  method! class_expr_desc = function
    | Pcl_extension ext -> fail Class_expr ext
    | x -> super#class_expr_desc x

  method! class_field_desc = function
    | Pcf_extension ext -> fail Class_field ext
    | x -> super#class_field_desc x

  method! module_type_desc = function
    | Pmty_extension ext -> fail Module_type ext
    | x -> super#module_type_desc x

  method! signature_item_desc = function
    | Psig_extension (ext, _) -> fail Signature_item ext
    | x -> super#signature_item_desc x

  method! module_expr_desc = function
    | Pmod_extension ext -> fail Module_expr ext
    | x -> super#module_expr_desc x

  method! structure_item_desc = function
    | Pstr_extension (ext, _) -> fail Structure_item ext
    | x -> super#structure_item_desc x
end
