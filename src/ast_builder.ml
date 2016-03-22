open StdLabels
open Parsetree

module Default = struct
  module Located = struct

    type 'a t = 'a Location.loc

    open Location

    let mk ~loc x = { loc; txt = x }

    let map f t = { t with txt = f t.txt }
    let map_lident x = map (fun x -> Longident.Lident x) x

    let of_ident ~loc id = mk ~loc (Ident.name id)

    let lident ~loc x = mk ~loc (Longident.parse x)

    let lident_of_ident ~loc id = lident ~loc (Ident.name id)
  end

  include Ast_builder_generated.M

  let nonrec_type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest =
    let td = type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest in
    { td with ptype_attributes =
                ({ txt = "nonrec"; loc }, PStr []) :: td.ptype_attributes }
  ;;

  let eint ~loc t = pexp_constant ~loc (Pconst_integer (string_of_int t, None))
  let echar ~loc t = pexp_constant ~loc (Pconst_char t)
  let estring ~loc t = pexp_constant ~loc (Pconst_string (t, None))
  let efloat ~loc t = pexp_constant ~loc (Pconst_float (t, None))
  let eint32 ~loc t = pexp_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))
  let eint64 ~loc t = pexp_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))
  let enativeint ~loc t = pexp_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let pint ~loc t = ppat_constant ~loc (Pconst_integer (string_of_int t, None))
  let pchar ~loc t = ppat_constant ~loc (Pconst_char t)
  let pstring ~loc t = ppat_constant ~loc (Pconst_string (t, None))
  let pfloat ~loc t = ppat_constant ~loc (Pconst_float (t, None))
  let pint32 ~loc t = ppat_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))
  let pint64 ~loc t = ppat_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))
  let pnativeint ~loc t = ppat_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let ebool ~loc t = pexp_construct ~loc (Located.lident ~loc (string_of_bool t)) None
  let pbool ~loc t = ppat_construct ~loc (Located.lident ~loc (string_of_bool t)) None

  let evar ~loc v = pexp_ident ~loc (Located.mk ~loc (Longident.parse v))
  let pvar ~loc v = ppat_var ~loc (Located.mk ~loc v)

  let eunit ~loc = pexp_construct ~loc (Located.lident ~loc "()") None
  let punit ~loc = ppat_construct ~loc (Located.lident ~loc "()") None

  let pexp_tuple ~loc l =
    match l with
    | [x] -> x
    | _   -> pexp_tuple ~loc l

  let ppat_tuple ~loc l =
    match l with
    | [x] -> x
    | _   -> ppat_tuple ~loc l

  let ptyp_tuple ~loc l =
    match l with
    | [x] -> x
    | _   -> ptyp_tuple ~loc l

  let pexp_apply ~loc e el =
    match e, el with
    | _, [] -> e
    | { pexp_desc = Pexp_apply (e, args)
      ; pexp_attributes = []; _ }, _ ->
      { e with pexp_desc = Pexp_apply (e, args @ el) }
    | _ -> pexp_apply ~loc e el
  ;;

  let eapply ~loc e el =
    pexp_apply ~loc e (List.map el ~f:(fun e -> (Asttypes.Nolabel, e)))

  let eabstract ~loc ps e =
    List.fold_right ps ~init:e ~f:(fun p e -> pexp_fun ~loc Asttypes.Nolabel None p e)
  ;;

  let pconstruct cd arg = ppat_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg
  let econstruct cd arg = pexp_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let rec elist ~loc l =
    match l with
    | [] ->
      pexp_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
      pexp_construct ~loc (Located.mk ~loc (Longident.Lident "::"))
        (Some (pexp_tuple ~loc [x; elist ~loc l]))
  ;;

  let rec plist ~loc l =
    match l with
    | [] ->
      ppat_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
      ppat_construct ~loc (Located.mk ~loc (Longident.Lident "::"))
        (Some (ppat_tuple ~loc [x; plist ~loc l]))
  ;;
end

module type Loc = Ast_builder_intf.Loc
module type S   = Ast_builder_intf.S

module Make(Loc : sig val loc : Location.t end) : S = struct
  include Ast_builder_generated.Make(Loc)

  let nonrec_type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    Default.nonrec_type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest
  ;;

  module Located = struct
    include Default.Located

    let mk              x = mk              ~loc x
    let of_ident        x = of_ident        ~loc x
    let lident          x = lident          ~loc x
    let lident_of_ident x = lident_of_ident ~loc x
  end

  let pexp_tuple l = Default.pexp_tuple ~loc l
  let ppat_tuple l = Default.ppat_tuple ~loc l
  let ptyp_tuple l = Default.ptyp_tuple ~loc l

  let pexp_apply e el = Default.pexp_apply ~loc e el

  let eint       t = Default.eint       ~loc t
  let echar      t = Default.echar      ~loc t
  let estring    t = Default.estring    ~loc t
  let efloat     t = Default.efloat     ~loc t
  let eint32     t = Default.eint32     ~loc t
  let eint64     t = Default.eint64     ~loc t
  let enativeint t = Default.enativeint ~loc t
  let ebool      t = Default.ebool      ~loc t
  let evar       t = Default.evar       ~loc t

  let pint       t = Default.pint       ~loc t
  let pchar      t = Default.pchar      ~loc t
  let pstring    t = Default.pstring    ~loc t
  let pfloat     t = Default.pfloat     ~loc t
  let pint32     t = Default.pint32     ~loc t
  let pint64     t = Default.pint64     ~loc t
  let pnativeint t = Default.pnativeint ~loc t
  let pbool      t = Default.pbool      ~loc t
  let pvar       t = Default.pvar       ~loc t

  let eunit = Default.eunit ~loc
  let punit = Default.punit ~loc

  let econstruct = Default.econstruct
  let pconstruct = Default.pconstruct

  let eapply e el = Default.eapply ~loc e el
  let eabstract ps e = Default.eabstract ~loc ps e

  let elist l = Default.elist ~loc l
  let plist l = Default.plist ~loc l
end

let make loc = (module Make(struct let loc = loc end) : S)
