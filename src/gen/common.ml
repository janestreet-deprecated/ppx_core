open StdLabels

let map_keyword = function
  | "open"
  | "private"
  | "downto"
  | "to"
  | "mutable"
  | "rec"
  | "nonrec"
  | "virtual"
  | "type"
  | "mod"
  | "begin"
  | "end" as s -> s ^ "_"
  | s -> s
;;

let lowercase_name_of_path path =
  let rec flatten_path (path : Path.t) acc =
    match path with
    | Pident id -> String.lowercase (Ident.name id) :: acc
    | Pdot (path, x, _) -> flatten_path path (x :: acc)
    | Papply _ -> assert false
  in
  String.concat ~sep:"_" (flatten_path path [])
;;

let function_name_of_path (path : Path.t) =
  match path with
  | Pident id -> Ident.name id
  | Pdot (path, "t", _) -> lowercase_name_of_path path
  | Pdot (Pident id, "loc", _) when Ident.name id = "Location" -> "loc"
  | Pdot (Pident id, s, _) when Ident.name id = "Asttypes" -> s
  | path -> lowercase_name_of_path path
;;

let rec add_type_expr_dependencies env acc (te : Types.type_expr) =
  match te.desc with
  | Ttuple tes ->
    List.fold_left tes ~init:acc ~f:(add_type_expr_dependencies env)
  | Tconstr (path, params, _) ->
    let acc = List.fold_left params ~init:acc ~f:(add_type_expr_dependencies env) in
    if List.mem_assoc path ~map:acc then
      acc
    else begin
      match Env.find_type path env with
      | td -> add_type_declaration_dependencies env ((path, td) :: acc) td
      | exception Not_found -> acc
    end
  | Tlink te
  | Tsubst te -> add_type_expr_dependencies env acc te
  | Tvar _
  | Tarrow _
  | Tobject _
  | Tfield _
  | Tnil
  | Tvariant _
  | Tunivar _
  | Tpoly _
  | Tpackage _ -> acc

and add_type_declaration_dependencies env acc (td : Types.type_declaration) =
  match td.type_kind with
  | Type_open -> acc
  | Type_record (lds, _) ->
    List.fold_left lds ~init:acc
      ~f:(fun acc (ld : Types.label_declaration) ->
         add_type_expr_dependencies env acc ld.ld_type)
  | Type_variant cds ->
    List.fold_left cds ~init:acc
      ~f:(fun acc (cd : Types.constructor_declaration) ->
         List.fold_left cd.cd_args ~init:acc
           ~f:(add_type_expr_dependencies env))
  | Type_abstract ->
    match td.type_manifest with
    | None -> acc
    | Some te -> add_type_expr_dependencies env acc te
;;

let get_types env unit =
  let signature =
    let path = Env.lookup_module ~load:true (Longident.Lident unit) env in
    match (Env.find_module path env).md_type with
    | Mty_signature signature -> signature
    | _ -> Printf.ksprintf failwith "%s is not a signature" unit
  in
  let types =
    let rec loop sg acc =
      match sg with
      | [] -> List.rev acc
      | Types.Sig_type (id, td, _) :: rest -> loop rest ((Path.Pident id, td) :: acc)
      | _ :: rest -> loop rest acc
    in
    loop signature []
  in
  let types =
    List.fold_left types ~init:types ~f:(fun acc (_, td) ->
      add_type_declaration_dependencies env acc td)
  in
  List.sort types ~cmp:(fun (p1, _) (p2, _) ->
    String.compare (function_name_of_path p1) (function_name_of_path p2))
;;
