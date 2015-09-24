open Parsetree
open Asttypes
open Ast_builder.Default

let lident x = Longident.Lident x

let core_type_of_type_declaration td =
  let loc = td.ptype_name.loc in
  ptyp_constr ~loc
    (Located.map lident td.ptype_name)
    (List.map fst td.ptype_params)
;;

let combinator_type_of_type_declaration td ~f =
  let result_type = f ~loc:td.ptype_name.loc (core_type_of_type_declaration td) in
  List.fold_right
    (fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      ptyp_arrow ~loc "" (f ~loc tp) acc)
    td.ptype_params
    result_type
;;

let gen_symbol_new =
  (* We need a bit of random as ppx can be run as multiple processes *)
  let rnd = Random.State.make_self_init () in
  let cnt = ref 0 in
  fun ?(prefix="_x") () ->
    Printf.sprintf "%s__%03d_%d" prefix !cnt (Random.State.bits rnd)
;;

let gen_symbol_old =
  let cnt = ref 0 in
  fun ?(prefix = "_x") () ->
    incr cnt;
    Printf.sprintf "%s__%03i_" prefix !cnt
;;

let gen_symbol =
  if true then
    gen_symbol_old
  else
    gen_symbol_new
;;

let string_of_core_type ct =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Pprintast.core_type ppf ct;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
;;

let get_type_param_name (ty, _) =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_var name -> Located.mk ~loc name
  | _ -> Location.raise_errorf ~loc "not a type parameter"


exception Stop
let type_is_recursive short_circuit type_names = object(self)
  inherit Ast_traverse.iter as super

  method! core_type ctyp =
    match short_circuit ctyp with
    | Some false -> ()
    | Some true  -> raise_notrace Stop
    | None ->
      match ctyp.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident id; _ }, _) when List.mem id type_names ->
        raise Stop
      | _ -> super#core_type ctyp

  method! constructor_declaration cd =
    (* Don't recurse through cd.pcd_res *)
    List.iter (fun ty -> self#core_type ty) cd.pcd_args
end

let types_are_recursive ?(stop_on_functions = true) ?(short_circuit = fun _ -> None)
      tds =
  let type_names = List.map (fun td -> td.ptype_name.txt) tds in
  let short_circuit =
    if stop_on_functions then
      fun ty ->
        match ty.ptyp_desc with
        | Ptyp_arrow _ -> Some false
        | _ -> short_circuit ty
    else short_circuit
  in
  let check = (type_is_recursive short_circuit type_names)#type_declaration in
  try
    List.iter (fun td -> check td) tds;
    false
  with Stop ->
    true

let really_recursive rec_flag tds =
  match rec_flag with
  | Recursive    -> if types_are_recursive tds then Recursive else Nonrecursive
  | Nonrecursive -> Nonrecursive

let rec last x l =
  match l with
  | [] -> x
  | x :: l -> last x l
;;

let loc_of_payload (name, payload) =
  match payload with
  | PStr []          -> name.loc
  | PStr (x :: l)    -> { x.pstr_loc with loc_end = (last x l).pstr_loc.loc_end }
  | PTyp t           -> t.ptyp_loc
  | PPat (x, None)   -> x.ppat_loc
  | PPat (x, Some e) -> { x.ppat_loc with loc_end = e.pexp_loc.loc_end }
;;

let loc_of_attribute ((name, _) as attr) =
  (* "ocaml.doc" attributes are generated with [Location.none], which is not helpful for
     error messages. *)
  if name.loc = Location.none then
    loc_of_payload attr
  else
    { name.loc with loc_end = (loc_of_payload attr).loc_end }
;;

let curry_applications expr =
  let open Ast_builder_generated.M in
  match expr.pexp_desc with
  | Pexp_apply (f,orig_forward_args) ->
    let loc = expr.pexp_loc in
    let rec loop = function
      | [] -> f
      | last_arg::rev_front_args -> pexp_apply ~loc (loop rev_front_args) [last_arg]
    in
    loop (List.rev orig_forward_args)
  | _ -> expr
;;

let assert_no_attributes : attributes -> unit = function
  | [] -> ()
  | attr :: _ ->
    let loc = loc_of_attribute attr in
    Location.raise_errorf ~loc "Attributes not allowed here"

let assert_no_attributes_in = object
  inherit Ast_traverse.iter

  method! attribute a = assert_no_attributes [a]
end
