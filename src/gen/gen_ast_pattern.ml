open StdLabels
open Printf
open Types
open Asttypes
open! Parsetree
open Ast_helper

let env = Env.initial_safe_string

let loc =
  (* This is fine, because the location info is thrown away when the generated code
     written out to the .ml file *)
  Location.none

let lident x = Longident.Lident x

module Loc = struct
  let mk     x = { Location.loc; txt = x }
  let lident x = mk (Longident.parse x)
end

module List = struct
  include List

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      match f x with
      | None -> filter_map l ~f
      | Some x -> x :: filter_map l ~f
end

let evar v = Exp.ident (Loc.lident v)
let pvar v = Pat.var (Loc.mk v)

let common_prefix l =
  match l with
  | [] -> ""
  | x :: l ->
    match String.index x '_' with
    | i ->
      let plen = i + 1 in
      let prefix = String.sub x ~pos:0 ~len:plen in
      let has_prefix s =
        String.length s >= plen && String.sub s ~pos:0 ~len:plen = prefix
      in
      if List.for_all l ~f:has_prefix then
        prefix
      else
        ""
    | exception _ -> ""
;;

let rec longident_of_path : Path.t -> Longident.t = function
  | Pident id -> Lident (Ident.name id)
  | Pdot (t, x, _) -> Ldot (longident_of_path t, x)
  | Papply (a, b) -> Lapply (longident_of_path a, longident_of_path b)
;;

let rec core_type_of_type vars te =
  match te.desc with
  | Tvar _ -> List.assoc te vars
  | Ttuple tes    -> Typ.tuple (List.map tes ~f:(core_type_of_type vars))
  | Tconstr (path, params, _) ->
    Typ.constr (Loc.mk (longident_of_path path))
      (List.map params ~f:(core_type_of_type vars))
  | Tlink te
  | Tsubst te -> core_type_of_type vars te
  | Tarrow _
  | Tobject _
  | Tfield _
  | Tnil
  | Tvariant _
  | Tunivar _
  | Tpoly _
  | Tpackage _ -> assert false
;;

let mk_t ?(t="t") ty a b =
  Typ.constr (Loc.lident t)
    [ ty
    ; a
    ; b
    ]

let mk_t' ?t ty a b =
  mk_t ?t ty
    (ksprintf Typ.var "a%d" a)
    (ksprintf Typ.var "a%d" b)
;;

let parser_types vars l =
  let tys = List.map l ~f:(core_type_of_type vars) in
  List.mapi tys ~f:(fun i ty -> mk_t' ty i (i + 1))
;;

let without_prefix ~prefix s =
  let plen = String.length prefix in
  String.sub s ~pos:plen ~len:(String.length s - plen)
;;

let map_keyword = Common.map_keyword

let function_name_of_id ?(prefix="") id =
  let s = without_prefix ~prefix (Ident.name id) in
(*  let prefix =
    if prefix <> "" && (prefix.[0] = 'p' || prefix.[0] = 'P') then
      String.sub prefix ~pos:1 ~len:(String.length prefix - 1)
    else
      prefix
  in*)
  match prefix ^ s with
  | "::" -> "cons"
  | "[]" -> "nil"
  | "true" -> "true_"
  | "false" -> "false_"
  | s -> String.lowercase s |> map_keyword
;;

let fqn_longident' path s : Longident.t =
  match longident_of_path path with
  | Lident _ -> Lident s
  | Ldot (p, _) -> Ldot (p, s)
  | Lapply _ -> assert false
;;

let fqn_longident path id : Longident.t = fqn_longident' path (Ident.name id)

let is_location_loc : Path.t -> bool = function
  | Pdot (Pident id, "loc", _) when Ident.name id = "Location" -> true
  | Pdot (Pident id, "loc", _) when Ident.name id = "Asttypes" -> true
  | _ -> false
;;

let apply_parsers funcs args types =
  List.fold_right2 (List.combine funcs args) types ~init:[%expr k]
    ~f:(fun (func, arg) typ acc ->
      match typ.desc with
      | Tconstr (path, _, _) when is_location_loc path ->
        [%expr
          let k = [%e evar func] ctx [%e arg].loc [%e arg].txt k in
          [%e acc]
        ]
      | _ ->
        [%expr
          let k = [%e evar func] ctx loc [%e arg] k in
          [%e acc]
        ])
;;

let assert_no_attributes ~path ~prefix =
  [%expr
    Common.assert_no_attributes
      [%e Exp.field (evar "x")
            (Loc.mk @@ fqn_longident' path (prefix ^ "attributes"))]
  ]

let gen_combinator_for_constructor ?wrapper path ~prefix cd =
  match cd.cd_args with
  | Cstr_record _ -> failwith "Cstr_record not supported"
  | Cstr_tuple cd_args ->
    let args =
      List.mapi cd_args ~f:(fun i _ -> sprintf "x%d" i)
    in
    let funcs =
      List.mapi cd_args ~f:(fun i _ -> sprintf "f%d" i)
    in
    let pat =
      Pat.construct (Loc.mk (fqn_longident path cd.cd_id))
        (match args with
         | []  -> None
         | [x] -> Some (pvar x)
         | _   -> Some (Pat.tuple (List.map args ~f:pvar)))
    in
    let exp =
      apply_parsers funcs (List.map args ~f:evar) cd_args
    in
    let expected = without_prefix ~prefix (Ident.name cd.cd_id) in
    let body =
      [%expr
        match x with
        | [%p pat] -> ctx.matched <- ctx.matched + 1; [%e exp]
        | _ -> fail loc [%e Exp.constant (Pconst_string (expected, None))]
      ]
    in
    let body =
      match wrapper with
      | None -> body
      | Some (path, prefix, has_attrs) ->
        let body =
          [%expr
            let loc = [%e Exp.field (evar "x")
                            (Loc.mk @@ fqn_longident' path (prefix ^ "loc"))]
            in
            let x = [%e Exp.field (evar "x")
                          (Loc.mk @@ fqn_longident' path (prefix ^ "desc"))]
            in
            [%e body]
          ]
        in
        if has_attrs then
          [%expr
            [%e assert_no_attributes ~path ~prefix];
            [%e body]
          ]
        else
          body
    in
    let body =
      let loc =
        match wrapper with
        | None -> [%pat? loc]
        | Some _ -> [%pat? _loc]
      in
      [%expr T (fun ctx [%p loc] x k -> [%e body])]
    in
    let body =
      List.fold_right funcs ~init:body ~f:(fun func acc ->
        [%expr fun (T [%p pvar func]) -> [%e acc]])
    in
    [%stri let [%p pvar (function_name_of_id ~prefix cd.cd_id)] = [%e body]]
;;

let gen_combinator_for_record path ~prefix ~has_attrs lds =
  let fields = List.map lds ~f:(fun ld -> fqn_longident path ld.ld_id) in
  let funcs =
    List.map lds ~f:(fun ld -> map_keyword (without_prefix ~prefix (Ident.name ld.ld_id)))
  in
  let body =
    apply_parsers funcs
      (List.map fields ~f:(fun field -> Exp.field (evar "x") (Loc.mk field)))
      (List.map lds ~f:(fun ld -> ld.ld_type))
  in
  let body =
    if has_attrs then
      [%expr
        [%e assert_no_attributes ~path ~prefix];
        [%e body]
      ]
    else
      body
  in
  let body = [%expr T (fun ctx loc x k -> [%e body])] in
  let body =
    List.fold_right funcs ~init:body ~f:(fun func acc ->
      Exp.fun_ (Labelled func) None [%pat? T [%p pvar func]] acc)
  in
  [%stri let [%p pvar (Common.function_name_of_path path)] = [%e body]]
;;

let alphabet =
  Array.init (Char.code 'z' - Char.code 'a' + 1)
    ~f:(fun i -> String.make 1 (Char.chr (i + Char.code 'a')))
;;

let vars_of_list l = List.mapi l ~f:(fun i _ -> alphabet.(i))

let lowercase_name_of_path path =
  let rec flatten_path (path : Path.t) acc =
    match path with
    | Pident id -> String.lowercase (Ident.name id) :: acc
    | Pdot (path, x, _) -> flatten_path path (x :: acc)
    | Papply _ -> assert false
  in
  String.concat ~sep:"_" (flatten_path path [])
;;

let type_name (path : Path.t) =
  match path with
  | Pident id -> Ident.name id
  | Pdot (path, "t", _) -> lowercase_name_of_path path
  | Pdot (Pident id, "loc", _) when Ident.name id = "Location" -> "loc"
  | Pdot (Pident id, s, _) when Ident.name id = "Asttypes" -> s
  | path -> lowercase_name_of_path path

let prefix_of_record lds = common_prefix (List.map lds ~f:(fun ld -> Ident.name ld.ld_id))

let filter_labels ~prefix lds =
  List.filter lds ~f:(fun ld ->
    match without_prefix ~prefix (Ident.name ld.ld_id) with
    | "loc" | "attributes" -> false
    | _ -> true)
;;

let is_wrapper ~prefix lds =
  match lds with
  | [ { ld_id = id; ld_type = { desc = Tconstr (p, _, _); _ }; _ } ]
    when Ident.name id = prefix ^ "desc" ->
    Some p
  | _ -> None
;;

let has_ld ~prefix lds label =
  List.exists lds ~f:(fun ld ->
    Ident.name ld.ld_id = prefix ^ label)
;;

let attributes_parser ~prefix ~name ~has_loc =
  let field s = Exp.field (evar "x") (Loc.lident @@ prefix ^ s) in
  let body =
    [%expr
      let k = f1 ctx loc [%e field "attributes"] k in
      let x =
        [%e Exp.record [(Loc.lident (prefix ^ "attributes"), [%expr []])]
              (Some (evar "x"))]
      in
      let k = f2 ctx loc x k in
      k
    ]
  in
  let body =
    if has_loc then
      [%expr
        let loc = [%e field "loc"] in
        [%e body]
      ]
    else
      body
  in
  let loc_patt =
    if has_loc then [%pat? _loc] else [%pat? loc]
  in
  [%stri
    let [%p pvar @@ name] = fun (T f1) (T f2) ->
      T (fun ctx [%p loc_patt] x k -> [%e body])
  ]

let gen_td ?wrapper (path : Path.t) td =
  if is_location_loc path then
    []
  else
    match td.type_kind with
    | Type_variant cds -> begin
        let prefix =
          common_prefix (List.map cds ~f:(fun cd -> Ident.name cd.cd_id))
        in
        let items =
          List.map cds ~f:(fun cd ->
            gen_combinator_for_constructor ?wrapper path ~prefix cd)
        in
        match wrapper with
        | Some (_, prefix, has_attrs) ->
          let field s = Exp.field (evar "x") (Loc.lident @@ prefix ^ s) in
          let items =
            if has_attrs then
              attributes_parser ~has_loc:true ~prefix ~name:(prefix ^ "attributes")
              :: items
            else
              items
          in
          [%stri
            let [%p pvar @@ prefix ^ "loc"] = fun (T f1) (T f2) ->
              T (fun ctx _loc x k ->
                let loc = [%e field "loc"] in
                let k = f1 ctx loc loc k in
                let k = f2 ctx loc x k in
                k
              )
          ] :: items
        | _ -> items
      end
    | Type_record (lds, _) ->
      let prefix = prefix_of_record lds in
      let has_attrs = has_ld ~prefix lds "attributes" in
      let has_loc   = has_ld ~prefix lds "loc" in
      let lds = filter_labels ~prefix lds in
      let items = [gen_combinator_for_record path ~prefix ~has_attrs lds] in
      if has_attrs then
        attributes_parser ~has_loc ~prefix
          ~name:(Common.function_name_of_path path ^ "_attributes")
        :: items
      else
        items
    | Type_abstract | Type_open -> []
;;

let is_abstract td =
  match td.type_kind with
  | Type_abstract -> true
  | _ -> false
;;

let dump fn ~ext printer x =
  let oc = open_out (fn ^ ext) in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." printer x;
  close_out oc

let generate unit =
  (*  let fn = Misc.find_in_path_uncap !Config.load_path (unit ^ ".cmi") in*)
  let types = Common.get_types env unit in
  let types_with_wrapped =
    List.map types ~f:(fun (path, td) ->
      match td.type_kind with
      | Type_record (lds, _) ->
        let prefix = prefix_of_record lds in
        let lds' = filter_labels ~prefix lds in
        (match is_wrapper ~prefix lds' with
         | None -> (path, td, None)
         | Some p ->
           let has_attrs = has_ld ~prefix lds "attributes" in
           (path, td, Some (prefix, has_attrs, p)))
      | _ -> (path, td, None))
  in
  let wrapped =
    List.filter_map types_with_wrapped ~f:(fun (_, _, x) ->
      match x with
      | None -> None
      | Some (_, _, p) -> Some p)
  in
  let types =
    List.filter types_with_wrapped ~f:(fun (path, _, _) ->
      not (List.mem path ~set:wrapped))
    |> List.map ~f:(fun (path, td, wrapped) ->
      match wrapped with
      | None -> (path, td, None)
      | Some (prefix, has_attrs, p) ->
        (path, td, Some (prefix, has_attrs, p, List.assoc p types)))
  in
  (*  let all_types = List.map fst types in*)
  let items =
    List.map types ~f:(fun (path, td, wrapped) ->
      if is_abstract td then
        []
      else
        match wrapped with
        | None -> gen_td path td
        | Some (prefix, has_attrs, path', td') ->
          gen_td ~wrapper:(path, prefix, has_attrs) path' td'
    )
    |> List.flatten
  in
  let st =
    Str.open_ (Opn.mk (Loc.lident "Parsetree"))
    :: Str.open_ (Opn.mk (Loc.lident "Ast_pattern0"))
    :: items
  in
  dump "ast_pattern_generated" Pprintast.structure st ~ext:".ml"

let args =
  [ "-I", Arg.String (fun s ->
      Config.load_path :=
        Misc.expand_directory Config.standard_library s
        :: !Config.load_path),
    "<dir> Add <dir> to the list of include directories"
  ]

let usage = Printf.sprintf "%s [options] <unit names>\n" Sys.argv.(0)

let () =
  Config.load_path := [Config.standard_library];
  let units = ref [] in
  Arg.parse (Arg.align args) (fun fn -> units := fn :: !units) usage;
  try
    List.iter (List.rev !units) ~f:generate
  with exn ->
    Errors.report_error Format.err_formatter exn;
    exit 2
