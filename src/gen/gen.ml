open Types
open Ast_helper

let loc =
  (* This is fine, because the location info is thrown away when the generated code s
     written out to the .ml file *)
  Location.none

let lident x = Longident.Lident x

module Loc = struct
  let mk ~loc x = { Location.loc; txt = x }
  let lident ~loc x = mk ~loc (Longident.parse x)
end

let evar v = Exp.ident (Loc.lident ~loc v)
let pvar v = Pat.var (Loc.mk ~loc v)

class type what = object
  method name : string

  method class_params : (Parsetree.core_type * Asttypes.variance) list

  method apply
    :  Parsetree.expression
    -> (string * Parsetree.expression) list
    -> Parsetree.expression

  method abstract
    :  Parsetree.pattern
    -> Parsetree.expression
    -> Parsetree.expression

  (* Basic combinator type *)
  method typ : Parsetree.core_type -> Parsetree.core_type

  method array : Parsetree.expression
  method any   : Parsetree.expression

  method combine
    :  (string * Parsetree.expression) list
    -> reconstruct:Parsetree.expression
    -> Parsetree.expression
end

let mapper : what = object
  method name = "map"

  method class_params = []

  method apply expr args = Exp.apply expr args
  method abstract patt expr = Exp.fun_ "" None patt expr

  method typ ty = Typ.arrow "" ty ty

  method array  = [%expr Array.map]
  method any    = [%expr fun x -> x]

  method combine combinators ~reconstruct =
    List.fold_right (fun (v, expr) acc ->
      Exp.let_ Nonrecursive [Vb.mk (pvar v) expr] acc)
      combinators reconstruct
end

let iterator : what = object
  method name = "iter"

  method class_params = []

  method apply expr args = Exp.apply expr args
  method abstract patt expr = Exp.fun_ "" None patt expr

  method typ ty = [%type: [%t ty] -> unit]
  method array  = [%expr Array.iter]
  method any    = [%expr ignore]

  method combine combinators ~reconstruct:_ =
    match List.rev combinators with
    | [] -> [%expr ()]
    | (_, expr) :: rest ->
      List.fold_left (fun acc (_v, expr) ->
        Exp.sequence expr acc)
        expr rest
end

let folder : what = object
  method name = "fold"

  method class_params = [(Typ.var "acc", Asttypes.Invariant)]

  method apply expr args = Exp.apply expr (args @ [("", evar "acc")])
  method abstract patt expr = Exp.fun_ "" None patt (Exp.fun_ "" None (pvar "acc") expr)

  method typ ty = [%type: [%t ty] -> 'acc -> 'acc]
  method array =
    [%expr
      fun a acc ->
        let r = ref acc in
        for i = 0 to Array.length a - 1 do
          r := f (Array.unsafe_get a i) !r
        done;
        !r
    ]
  method any = [%expr fun _ acc -> acc]

  method combine combinators ~reconstruct:_ =
    match combinators with
    | [(_, expr)] -> expr
    | _ ->
      List.fold_right
        (fun (_v, expr) acc ->
           [%expr
             let acc = [%e expr] in
             [%e acc]
           ])
        combinators [%expr acc]
end

let fold_mapper : what = object
  method name = "fold_map"

  method class_params = [(Typ.var "acc", Asttypes.Invariant)]

  method apply expr args = Exp.apply expr (args @ [("", evar "acc")])
  method abstract patt expr = Exp.fun_ "" None patt (Exp.fun_ "" None (pvar "acc") expr)

  method typ ty = [%type: [%t ty] -> 'acc -> [%t ty] * 'acc]
  method array =
    [%expr
      fun a acc ->
        let len = Array.length a in
        if len = 0 then
          (a, acc)
        else begin
          let x, acc = f (Array.unsafe_get a 0) acc in
          let a' = Array.create len x in
          let r = ref acc in
          for i = 1 to len - 1 do
            let x, acc = f (Array.unsafe_get a i) !r in
            Array.unsafe_set a' i x;
            r := acc
          done;
          (a', !r)
        end
    ]
  method any = [%expr fun x acc -> (x, acc)]

  method combine combinators ~reconstruct =
    List.fold_right
      (fun (v, expr) acc ->
         [%expr
           let ([%p pvar v], acc) = [%e expr] in
           [%e acc]
         ])
      combinators [%expr ([%e reconstruct], acc)]
end

exception Found
let uses_var var =
  let super = Ast_mapper.default_mapper in
  let expr this (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident { txt = Longident.Lident id; _ } when id = var ->
      raise_notrace Found
    | _ -> super.expr this e
  in
  let mapper = { super with expr } in
  fun e ->
    try
      ignore (expr mapper e : Parsetree.expression);
      false
    with Found ->
      true
;;

let mapper_with_context : what =
  let uses_ctx = uses_var "ctx" in
  object
    method name = "map_with_context"

    method class_params = [(Typ.var "ctx", Asttypes.Invariant)]

    method apply expr args = Exp.apply expr (("", evar "ctx") :: args)
    method abstract patt expr =
      if uses_ctx expr then
        Exp.fun_ "" None (pvar "ctx") (Exp.fun_ "" None patt expr)
      else
        Exp.fun_ "" None (pvar "_ctx") (Exp.fun_ "" None patt expr)

    method typ ty = [%type: 'ctx -> [%t ty] -> [%t ty]]
    method array = [%expr fun ctx a -> Array.map (f ctx) a]
    method any = [%expr fun _ctx x -> x]

    method combine combinators ~reconstruct =
      List.fold_right
        (fun (v, expr) acc ->
           [%expr
             let [%p pvar v] = [%e expr] in
             [%e acc]
           ])
        combinators reconstruct
  end

let alphabet =
  Array.init (Char.code 'z' - Char.code 'a' + 1)
    (fun i -> String.make 1 (Char.chr (i + Char.code 'a')))
;;

let vars_of_list l = List.mapi (fun i _ -> alphabet.(i)) l

let rec longident_of_path : Path.t -> Longident.t = function
  | Pident id -> Lident (Ident.name id)
  | Pdot (t, x, _) -> Ldot (longident_of_path t, x)
  | Papply (a, b) -> Lapply (longident_of_path a, longident_of_path b)

let mapper_type ~(what:what) path td =
  let vars = vars_of_list td.type_params in
  let params = List.map (fun v -> Typ.var v) vars in
  let ty = Typ.constr (Loc.mk ~loc (longident_of_path path)) params in
  let ty =
    List.fold_right
      (fun param ty -> Typ.arrow "" (what#typ param) ty)
      params (what#typ ty)
  in
  Typ.poly vars ty
;;

let method_name = Common.function_name_of_path

let rec type_expr_mapper ~(what:what) ~all_types ~var_mappers te =
  match te.desc with
  | Tvar _ -> evar (List.assoc te var_mappers)
  | Ttuple tes ->
    let vars = vars_of_list tes in
    let deconstruct = Pat.tuple (List.map (fun s -> pvar s) vars) in
    let reconstruct = Exp.tuple (List.map (fun s -> evar s) vars) in
    let mappers = map_variables ~what ~all_types ~var_mappers vars tes in
    what#abstract deconstruct (what#combine mappers ~reconstruct)
  | Tconstr (path, params, _) ->
    if List.mem path all_types then
      let map = Exp.send (evar "self") (method_name path) in
      match params with
      | [] -> map
      | _ ->
        Exp.apply map
          (List.map
             (fun te -> ("", type_expr_mapper ~what ~all_types ~var_mappers te))
             params)
    else
      what#any
  | Tlink te
  | Tsubst te -> type_expr_mapper ~what ~all_types ~var_mappers te
  | Tarrow _
  | Tobject _
  | Tfield _
  | Tnil
  | Tvariant _
  | Tunivar _
  | Tpoly _
  | Tpackage _ -> what#any

and map_variables ~(what:what) ~all_types ~var_mappers vars tes =
  List.map2
    (fun te var ->
       (var,
        what#apply (type_expr_mapper ~what ~all_types ~var_mappers te) [("", evar var)]))
    tes vars
;;

let gen_record ~(what:what) ~all_types ~var_mappers lds =
  let vars = List.map (fun ld -> Ident.name ld.ld_id) lds in
  let deconstruct =
    Pat.record
      (List.map (fun v -> (Loc.lident ~loc v, pvar v)) vars)
      Closed
  in
  let reconstruct =
    Exp.record
      (List.map (fun v -> (Loc.lident ~loc v, evar v)) vars)
      None
  in
  let mappers =
    map_variables ~what ~all_types ~var_mappers
      vars
      (List.map (fun ld -> ld.ld_type) lds)
  in
  what#abstract deconstruct (what#combine mappers ~reconstruct)

let gen_variant ~(what:what) ~all_types ~var_mappers cds =
  let cases =
    List.map
      (fun cd ->
         let vars = vars_of_list cd.cd_args in
         let cstr = Loc.mk ~loc (lident (Ident.name cd.cd_id)) in
         let deconstruct =
           Pat.construct cstr
             (match vars with
              | [] -> None
              | _ -> Some (Pat.tuple (List.map pvar vars)))
         in
         let reconstruct =
           Exp.construct cstr
             (match vars with
              | [] -> None
              | _ -> Some (Exp.tuple (List.map evar vars)))
         in
         let mappers =
           map_variables ~what ~all_types ~var_mappers vars cd.cd_args
         in
         Exp.case deconstruct (what#combine mappers ~reconstruct))
      cds
  in
  what#abstract (pvar "x") (Exp.match_ (evar "x") cases)

let gen_mapper ~(what:what) ~all_types path td =
  if Path.same path Predef.path_array then
    what#array
  else begin
    let var_mappers =
      List.mapi
        (fun i t ->
           (t, Printf.sprintf "map_%s" alphabet.(i)))
        td.type_params
    in
    let body =
      match td.type_kind with
      | Type_open -> what#any
      | Type_record (lds, _) -> gen_record  ~what ~all_types ~var_mappers lds
      | Type_variant cds     -> gen_variant ~what ~all_types ~var_mappers cds
      | Type_abstract ->
        match td.type_manifest with
        | None -> what#any
        | Some te -> type_expr_mapper ~what ~all_types ~var_mappers te
    in
    List.fold_right
      (fun (_, v) acc -> Exp.fun_ "" None (pvar v) acc)
      var_mappers body
  end
;;

let env = Env.initial_safe_string

let dump ~what ~ext printer x =
  let oc = open_out ("ast_traverse_" ^ what#name ^ ext) in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." printer x;
  close_out oc

let generate unit =
  (*  let fn = Misc.find_in_path_uncap !Config.load_path (unit ^ ".cmi") in*)
  let types = Common.get_types env unit in
  let all_types = List.map fst types in
  List.iter
    (fun (what : what) ->
       let methods =
         List.map
           (fun (path, td) ->
              let mapper = gen_mapper ~what ~all_types path td in
              let mapper = Exp.constraint_ mapper (mapper_type ~what path td) in
              Cf.method_ (Loc.mk ~loc (method_name path)) Public
                (Cf.concrete Fresh mapper))
           types
       in
       let method_sigs =
         List.map
           (fun (path, td) ->
              Ctf.method_ (method_name path) Public Concrete (mapper_type ~what path td))
           types
       in
       let cl =
         Ci.mk (Loc.mk ~loc "t") ~params:what#class_params
           (Cl.structure
              (Cstr.mk (pvar "self") methods))
       in
       let cl_type =
         Ci.mk (Loc.mk ~loc "t") ~params:what#class_params
           (Cty.signature
              (Csig.mk (Typ.any ()) method_sigs))
       in
       let st =
         [ Str.open_ (Opn.mk (Loc.mk ~loc (lident unit)))
         ; Str.class_ [cl]
         ]
       in
       let sg =
         [ Sig.open_ (Opn.mk (Loc.mk ~loc (lident unit)))
         ; Sig.class_ [cl_type]
         ]
       in
       dump ~what Pprintast.structure st ~ext:".ml";
       dump ~what Pprintast.signature sg ~ext:".mli")
    (* This list must be kept in sync with the targets listed in ../jbuild *)
    [mapper; iterator; folder; fold_mapper; mapper_with_context]

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
    List.iter generate (List.rev !units)
  with exn ->
    Errors.report_error Format.err_formatter exn;
    exit 2
