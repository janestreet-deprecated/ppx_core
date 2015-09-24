open StdLabels
open Printf
open Types
open! Asttypes
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

let prefix_of_record lds = common_prefix (List.map lds ~f:(fun ld -> Ident.name ld.ld_id))

module Gen(M : sig val fixed_loc : bool end) = struct
  open M

  let gen_combinator_for_constructor ~wrapper:(wpath, wprefix, has_attrs) path ~prefix cd =
    let args =
      List.mapi cd.cd_args ~f:(fun i _ -> sprintf "x%d" i)
    in
    let exp =
      Exp.construct (Loc.mk (fqn_longident path cd.cd_id))
        (match args with
         | []  -> None
         | [x] -> Some (evar x)
         | _   -> Some (Exp.tuple (List.map args ~f:evar)))
    in
    let body =
      let fields =
        [ ( Loc.mk (fqn_longident' wpath (wprefix ^ "loc"))
          , evar "loc"
          )
        ; ( Loc.mk (fqn_longident' wpath (wprefix ^ "desc"))
          , exp
          )
        ]
      in
      let fields =
        if has_attrs then
          ( Loc.mk (fqn_longident' wpath (wprefix ^ "attributes"))
          , [%expr []]
          )
          :: fields
        else
          fields
      in
      Exp.record fields None
    in
    let body =
(*      match args with
      | [] -> [%expr fun () -> [%e body]]
      | _ ->*)
        List.fold_right args ~init:body ~f:(fun arg acc ->
          [%expr fun [%p pvar arg] -> [%e acc]])
    in
(*    let body =
      if not has_attrs then
        body
      else
        [%expr fun ?(attrs=[]) -> [%e body]]
    in*)
    let body =
      if fixed_loc then
        body
      else
        [%expr fun ~loc -> [%e body]]
    in
    [%stri let [%p pvar (function_name_of_id ~prefix cd.cd_id)] = [%e body]]
  ;;

  let gen_combinator_for_record path ~prefix lds =
    let fields = List.map lds ~f:(fun ld -> fqn_longident path ld.ld_id) in
    let funcs =
      List.map lds ~f:(fun ld ->
        map_keyword (without_prefix ~prefix (Ident.name ld.ld_id)))
    in
    let body =
      Exp.record
        (List.map2 fields funcs ~f:(fun field func ->
           (Loc.mk field, if func = "attributes" then [%expr []] else evar func)))
        None
    in
    let body =
      let l = List.filter funcs ~f:(fun f -> f <> "loc" && f <> "attributes") in
      match l with
      | [x] -> Exp.fun_ "" None (pvar x) body
      | _ ->
        List.fold_right l ~init:body ~f:(fun func acc ->
          Exp.fun_ func None (pvar func) acc
        )
    in
(*    let body =
      if List.mem "attributes" ~set:funcs then
        [%expr fun ?(attrs=[]) -> [%e body]]
      else
        body
    in*)
    let body =
      if List.mem "loc" ~set:funcs && not fixed_loc then
        [%expr fun ~loc -> [%e body]]
      else
        body
    in
    [%stri let [%p pvar (Common.function_name_of_path path)] = [%e body]]
  ;;

  let gen_td ?wrapper (path : Path.t) td =
    if is_location_loc path then
      []
    else
      match td.type_kind with
      | Type_variant cds ->
        begin match wrapper with
        | None -> []
        | Some wrapper ->
          let prefix =
            common_prefix (List.map cds ~f:(fun cd -> Ident.name cd.cd_id))
          in
          List.map cds ~f:(fun cd ->
            gen_combinator_for_constructor ~wrapper path ~prefix cd)
        end
      | Type_record (lds, _) ->
        let prefix = prefix_of_record lds in
        [gen_combinator_for_record path ~prefix lds]
      | Type_abstract | Type_open -> []
  ;;
end

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
           let has_attrs = List.exists lds ~f:(fun ld ->
             Ident.name ld.ld_id = prefix ^ "attributes")
           in
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
  let items fixed_loc =
    let module G = Gen(struct let fixed_loc = fixed_loc end) in
    List.map types ~f:(fun (path, td, wrapped) ->
      if is_abstract td then
        []
      else
        match wrapped with
        | None -> G.gen_td path td
        | Some (prefix, has_attrs, path', td') ->
          G.gen_td ~wrapper:(path, prefix, has_attrs) path' td'
    )
    |> List.flatten
  in
  let st =
    [ Str.open_ (Opn.mk (Loc.lident "Parsetree"))
    ; Str.module_ (Mb.mk (Loc.mk "M") (Mod.structure (items false)))
    ; Str.module_ (Mb.mk (Loc.mk "Make")
                      (Mod.functor_ (Loc.mk "Loc") (Some (Mty.signature [
                         Sig.value (Val.mk (Loc.mk "loc") [%type: Location.t])
                       ]))
                         (Mod.structure
                            ([%stri let loc = Loc.loc]
                             :: items true))))
    ]
  in
  dump "ast_builder_generated" Pprintast.structure st ~ext:".ml"

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
