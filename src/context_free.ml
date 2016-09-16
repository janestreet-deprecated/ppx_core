open Parsetree
open Common

module E  = Extension
module EC = Extension.Context
module A  = Attribute
module AC = Attribute.Context

module Rule = struct
  module Attr_group_inline = struct
    type ('a, 'b, 'c) unpacked =
      { attribute : ('b, 'c) Attribute.t
      ; expand    : (loc:Location.t
                     -> path:string
                     -> Asttypes.rec_flag
                     -> 'b list
                     -> 'c option list
                     -> 'a list)
      }

    type ('a, 'b) t = T : ('a, 'b, _) unpacked -> ('a, 'b) t

    let attr_name (T t) = Attribute.name t.attribute
  end

  module Attr_inline = struct
    type ('a, 'b, 'c) unpacked =
      { attribute : ('b, 'c) Attribute.t
      ; expand    : (loc:Location.t
                     -> path:string
                     -> 'b
                     -> 'c
                     -> 'a list)
      }

    type ('a, 'b) t = T : ('a, 'b, _) unpacked -> ('a, 'b) t
    let attr_name (T t) = Attribute.name t.attribute
  end

  module Special_function = struct
    type t =
      { name   : string
      ; ident  : Longident.t
      ; expand : Parsetree.expression -> Parsetree.expression option
      }
  end

  module Field = struct
    type 'a t =
      | Extension          : Extension.t                                            t
      | Special_function   : Special_function.t                                     t
      | Attr_str_type_decl : (structure_item, type_declaration) Attr_group_inline.t t
      | Attr_sig_type_decl : (signature_item, type_declaration) Attr_group_inline.t t
      | Attr_str_type_ext  : (structure_item, type_extension) Attr_inline.t         t
      | Attr_sig_type_ext  : (signature_item, type_extension) Attr_inline.t         t
      | Attr_str_exception : (structure_item, extension_constructor) Attr_inline.t  t
      | Attr_sig_exception : (signature_item, extension_constructor) Attr_inline.t  t

    type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

    let eq : type a b. a t -> b t -> (a, b) equality = fun a b ->
      match a, b with
      | Extension          , Extension          -> Eq
      | Special_function   , Special_function   -> Eq
      | Attr_str_type_decl , Attr_str_type_decl -> Eq
      | Attr_sig_type_decl , Attr_sig_type_decl -> Eq
      | Attr_str_type_ext  , Attr_str_type_ext  -> Eq
      | Attr_sig_type_ext  , Attr_sig_type_ext  -> Eq
      | Attr_str_exception , Attr_str_exception -> Eq
      | Attr_sig_exception , Attr_sig_exception -> Eq
      | _ -> Ne
  end

  type t = T : 'a Field.t * 'a -> t

  type ('a, 'b, 'c) attr_group_inline =
    ('b, 'c) Attribute.t
    -> (loc:Location.t
        -> path:string
        -> Asttypes.rec_flag
        -> 'b list
        -> 'c option list
        -> 'a list)
    -> t

  type ('a, 'b, 'c) attr_inline =
    ('b, 'c) Attribute.t
    -> (loc:Location.t
        -> path:string
        -> 'b
        -> 'c
        -> 'a list)
    -> t

  let rec filter : type a. a Field.t -> t list -> a list = fun field l ->
    match l with
    | [] -> []
    | (T (field', x)) :: l ->
      match Field.eq field field' with
      | Field.Eq -> x :: filter field l
      | Field.Ne ->      filter field l
  ;;

  let extension ext = T (Extension, ext)

  let special_function id f =
    T (Special_function, { name   = id
                         ; ident  = Longident.parse id
                         ; expand = f
                         })
  ;;

  let attr_str_type_decl attribute expand =
    T (Attr_str_type_decl, T { attribute; expand })
  ;;

  let attr_sig_type_decl attribute expand =
    T (Attr_sig_type_decl, T { attribute; expand })
  ;;

  let attr_str_type_ext attribute expand =
    T (Attr_str_type_ext, T { attribute; expand })
  ;;

  let attr_sig_type_ext attribute expand =
    T (Attr_sig_type_ext, T { attribute; expand })
  ;;

  let attr_str_exception attribute expand =
    T (Attr_str_exception, T { attribute; expand })
  ;;

  let attr_sig_exception attribute expand =
    T (Attr_sig_exception, T { attribute; expand })
  ;;
end

let rec map_node context ts super_call loc path x =
  match EC.get_extension context x with
  | None -> super_call path x
  | Some (ext, attrs) ->
    match E.For_context.convert ts ~loc ~path ext with
    | None -> super_call path x
    | Some x ->
      map_node context ts super_call loc path (EC.merge_attributes context x attrs)
;;

let rec map_nodes context ts super_call get_loc path l =
  match l with
  | [] -> []
  | x :: l ->
    match EC.get_extension context x with
    | None ->
      (* These two lets force the evaluation order, so that errors are reported in the
         same order as they appear in the source file. *)
      let x = super_call path x in
      let l = map_nodes context ts super_call get_loc path l in
      x :: l
    | Some (ext, attrs) ->
      let loc = get_loc x in
      match E.For_context.convert_inline ts ~loc ~path ext with
      | None ->
        let x = super_call path x in
        let l = map_nodes context ts super_call get_loc path l in
        x :: l
      | Some x ->
        assert_no_attributes attrs;
        map_nodes context ts super_call get_loc path (x @ l)
;;

let table_of_special_functions special_functions =
  (* We expect the lookup to fail most of the time, by making the table big (and
     sparse), we make it more likely to fail quickly *)
  let table =
    Hashtbl.Poly.create ()
      ~size:(Int.max 1024 (List.length special_functions * 2))
  in
  List.iter special_functions ~f:(fun { Rule.Special_function. name; ident; expand } ->
    if Hashtbl.mem table ident then
      Printf.ksprintf invalid_arg
        "Context_free.V1.map_top_down: \
         %s present twice in list of special functions"
        name;
    Hashtbl.add_exn table ~key:ident ~data:expand);
  table
;;

let rec consume_group attr l =
  match l with
  | [] -> None
  | x :: l ->
    match Attribute.consume attr x, consume_group attr l with
    | None, None -> None
    | None, Some (l, vals) ->
      Some ((x :: l),
            (None :: vals))
    | Some (x, value), None ->
      Some ((x :: l),
            (Some value :: List.map l ~f:(fun _ -> None)))
    | Some (x, value), Some (l, vals) ->
      Some ((x :: l),
            (Some value :: vals))
;;

(* Same as [List.rev] then [List.concat] but expecting the input to be of length <= 2 *)
let rev_concat = function
  | [] -> []
  | [x] -> x
  | [x; y] -> y @ x
  | l -> List.concat (List.rev l)
;;

let sort_attr_group_inline l =
  List.sort l ~cmp:(fun a b ->
    String.compare
      (Rule.Attr_group_inline.attr_name a)
      (Rule.Attr_group_inline.attr_name b))

let sort_attr_inline l =
  List.sort l ~cmp:(fun a b ->
    String.compare
      (Rule.Attr_inline.attr_name a)
      (Rule.Attr_inline.attr_name b))

class map_top_down rules =
  let special_functions =
    Rule.filter Special_function rules |> table_of_special_functions
  in
  let extensions = Rule.filter Extension rules in
  let class_expr       = E.filter_by_context EC.class_expr       extensions
  and class_field      = E.filter_by_context EC.class_field      extensions
  and class_type       = E.filter_by_context EC.class_type       extensions
  and class_type_field = E.filter_by_context EC.class_type_field extensions
  and core_type        = E.filter_by_context EC.core_type        extensions
  and expression       = E.filter_by_context EC.expression       extensions
  and module_expr      = E.filter_by_context EC.module_expr      extensions
  and module_type      = E.filter_by_context EC.module_type      extensions
  and pattern          = E.filter_by_context EC.pattern          extensions
  and signature_item   = E.filter_by_context EC.signature_item   extensions
  and structure_item   = E.filter_by_context EC.structure_item   extensions
  in

  let attr_str_type_decls =
    Rule.filter Attr_str_type_decl rules
    |> sort_attr_group_inline
  in
  let attr_sig_type_decls =
    Rule.filter Attr_sig_type_decl rules
    |> sort_attr_group_inline
  in

  let attr_str_type_exts =
    Rule.filter Attr_str_type_ext rules
    |> sort_attr_inline
  in
  let attr_sig_type_exts =
    Rule.filter Attr_sig_type_ext rules
    |> sort_attr_inline
  in

  let attr_str_exceptions =
    Rule.filter Attr_str_exception rules
    |> sort_attr_inline
  in
  let attr_sig_exceptions =
    Rule.filter Attr_sig_exception rules
    |> sort_attr_inline
  in

  object(self)
    inherit Ast_traverse.map_with_path as super

    (* No point recursing into every location *)
    method! location _ x = x

    method! core_type path x =
      map_node EC.core_type core_type super#core_type x.ptyp_loc path x

    method! pattern path x =
      map_node EC.pattern pattern super#pattern x.ppat_loc path x

    method! expression path e =
      let e =
        match e.pexp_desc with
        | Pexp_extension _ ->
          map_node EC.expression expression (fun _ e -> e) e.pexp_loc path e
        | _ -> e
      in
      match e.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident id; _ } as func, args) -> begin
          match Hashtbl.find special_functions id.txt with
          | None ->
            self#pexp_apply_without_traversing_function path e func args
          | Some pattern ->
            match pattern e with
            | None ->
              self#pexp_apply_without_traversing_function path e func args
            | Some e ->
              self#expression path e
        end
      | Pexp_ident id -> begin
          match Hashtbl.find special_functions id.txt with
          | None ->
            super#expression path e
          | Some pattern ->
            match pattern e with
            | None ->
              super#expression path e
            | Some e ->
              self#expression path e
        end
      | _ ->
        super#expression path e

    (* Pre-conditions:
       - e.pexp_desc = Pexp_apply(func, args)
       - func.pexp_desc = Pexp_ident _
    *)
    method private pexp_apply_without_traversing_function path e func args =
      let { pexp_desc = _; pexp_loc; pexp_attributes } = e in
      let func =
        let { pexp_desc; pexp_loc; pexp_attributes } = func in
        let pexp_attributes = self#attributes path pexp_attributes in
        { pexp_desc
        ; pexp_loc (* location doesn't need to be traversed *)
        ; pexp_attributes
        }
      in
      let args = List.map args ~f:(fun (lab, exp) -> (lab, self#expression path exp)) in
      let pexp_attributes = self#attributes path pexp_attributes in
      { pexp_loc
      ; pexp_attributes
      ; pexp_desc = Pexp_apply (func, args)
      }

    method! class_type path x =
      map_node EC.class_type class_type super#class_type x.pcty_loc path x

    method! class_type_field path x =
      map_node EC.class_type_field class_type_field super#class_type_field
        x.pctf_loc path x

    method! class_expr path x =
      map_node EC.class_expr class_expr super#class_expr x.pcl_loc path x

    method! class_field path x =
      map_node EC.class_field class_field super#class_field x.pcf_loc path x

    method! module_type path x =
      map_node EC.module_type module_type super#module_type x.pmty_loc path x

    method! module_expr path x =
      map_node EC.module_expr module_expr super#module_expr x.pmod_loc path x

    method! structure_item path x =
      map_node EC.structure_item structure_item super#structure_item x.pstr_loc path x

    method! signature_item path x =
      map_node EC.signature_item signature_item super#signature_item x.psig_loc path x

    method! class_structure path { pcstr_self; pcstr_fields } =
      let pcstr_self = self#pattern path pcstr_self in
      let pcstr_fields =
        map_nodes EC.class_field class_field super#class_field
          (fun x -> x.pcf_loc) path pcstr_fields
      in
      { pcstr_self; pcstr_fields }

    method! class_signature path { pcsig_self; pcsig_fields } =
      let pcsig_self = self#core_type path pcsig_self in
      let pcsig_fields =
        map_nodes EC.class_type_field class_type_field super#class_type_field
          (fun x -> x.pctf_loc) path pcsig_fields
      in
      { pcsig_self; pcsig_fields }

    method! structure path st =
      match st with
      | [] -> []
      | item :: rest ->
        match item.pstr_desc with
        | Pstr_extension (ext, attrs) -> begin
            let loc = item.pstr_loc in
            match E.For_context.convert_inline structure_item ~loc ~path ext with
            | None ->
              let item = super#structure_item path item in
              let rest = self#structure path rest in
              item :: rest
            | Some items ->
              assert_no_attributes attrs;
              self#structure path (items @ rest)
          end
        | Pstr_type(rf, tds) ->
          let tds, extra_items =
            List.fold_left attr_str_type_decls ~init:(tds, [])
              ~f:(fun (tds, generated_items) (Rule.Attr_group_inline.T group) ->
                match consume_group group.attribute tds with
                | None -> (tds, generated_items)
                | Some (tds, values) ->
                  let extra_items =
                    group.expand ~loc:item.pstr_loc ~path rf tds values
                  in
                  (tds, extra_items :: generated_items))
          in
          let item = { item with pstr_desc = Pstr_type(rf, tds) } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#structure_item path item in
          let rest = self#structure path rest in
          item :: rest
        | Pstr_typext te ->
          let te, extra_items =
            List.fold_left attr_str_type_exts ~init:(te, [])
              ~f:(fun (te, generated_items) (Rule.Attr_inline.T a) ->
                match Attribute.consume a.attribute te with
                | None -> (te, generated_items)
                | Some (te, value) ->
                  let extra_items = a.expand ~loc:item.pstr_loc ~path te value in
                  (te, extra_items :: generated_items))
          in
          let item = { item with pstr_desc = Pstr_typext te } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#structure_item path item in
          let rest = self#structure path rest in
          item :: rest
        | Pstr_exception ec ->
          let ec, extra_items =
            List.fold_left attr_str_exceptions ~init:(ec, [])
              ~f:(fun (ec, generated_items) (Rule.Attr_inline.T a) ->
                match Attribute.consume a.attribute ec with
                | None -> (ec, generated_items)
                | Some (ec, value) ->
                  let extra_items = a.expand ~loc:item.pstr_loc ~path ec value in
                  (ec, extra_items :: generated_items))
          in
          let item = { item with pstr_desc = Pstr_exception ec } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#structure_item path item in
          let rest = self#structure path rest in
          item :: rest
        | _ ->
          let item = self#structure_item path item in
          let rest = self#structure path rest in
          item :: rest

    method! signature path sg =
      match sg with
      | [] -> []
      | item :: rest ->
        match item.psig_desc with
        | Psig_extension (ext, attrs) -> begin
            let loc = item.psig_loc in
            match E.For_context.convert_inline signature_item ~loc ~path ext with
            | None ->
              let item = super#signature_item path item in
              let rest = self#signature path rest in
              item :: rest
            | Some items ->
              assert_no_attributes attrs;
              self#signature path (items @ rest)
          end
        | Psig_type(rf, tds) ->
          let tds, extra_items =
            List.fold_left attr_sig_type_decls ~init:(tds, [])
              ~f:(fun (tds, generated_items) (Rule.Attr_group_inline.T group) ->
                match consume_group group.attribute tds with
                | None -> (tds, generated_items)
                | Some (tds, values) ->
                  let extra_items =
                    group.expand  ~loc:item.psig_loc ~path rf tds values
                  in
                  (tds, extra_items :: generated_items))
          in
          let item = { item with psig_desc = Psig_type(rf, tds) } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#signature_item path item in
          let rest = self#signature path rest in
          item :: rest
        | Psig_typext te ->
          let te, extra_items =
            List.fold_left attr_sig_type_exts ~init:(te, [])
              ~f:(fun (te, generated_items) (Rule.Attr_inline.T a) ->
                match Attribute.consume a.attribute te with
                | None -> (te, generated_items)
                | Some (te, value) ->
                  let extra_items = a.expand ~loc:item.psig_loc ~path te value in
                  (te, extra_items :: generated_items))
          in
          let item = { item with psig_desc = Psig_typext te } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#signature_item path item in
          let rest = self#signature path rest in
          item :: rest
        | Psig_exception ec ->
          let ec, extra_items =
            List.fold_left attr_sig_exceptions ~init:(ec, [])
              ~f:(fun (ec, generated_items) (Rule.Attr_inline.T a) ->
                match Attribute.consume a.attribute ec with
                | None -> (ec, generated_items)
                | Some (ec, value) ->
                  let extra_items = a.expand ~loc:item.psig_loc ~path ec value in
                  (ec, extra_items :: generated_items))
          in
          let item = { item with psig_desc = Psig_exception ec } in
          let rest = rev_concat (rest :: extra_items) in
          let item = super#signature_item path item in
          let rest = self#signature path rest in
          item :: rest
        | _ ->
          let item = self#signature_item path item in
          let rest = self#signature path rest in
          item :: rest
  end
