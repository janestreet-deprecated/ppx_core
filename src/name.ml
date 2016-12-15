open StdLabels
open MoreLabels

module String_set = Set.Make(String)
module String_map = Map.Make(String)

(* set of matches for "foo.bar.blah":
   - "foo.bar.blah"
   -     "bar.blah"
   -         "blah"
*)
let matches ~pattern matched =
  pattern = matched || (
    let len_pattern = String.length pattern in
    let len_matched = String.length matched in
    let start = len_pattern - len_matched in
    start > 0 && pattern.[start - 1] = '.' &&
    (
      let i = ref 0 in
      while !i < len_matched &&
            String.unsafe_get matched !i = String.unsafe_get pattern (start + !i)
      do
        incr i
      done;
      !i = len_matched
    )
  )

let fold_dot_suffixes name ~init:acc ~f =
  let rec loop pos acc =
    if pos >= 0 then
      match String.rindex_from name pos '.' with
      | exception Not_found -> f name acc
      | i ->
        let sub_name = String.sub name ~pos:(i + 1) ~len:(String.length name - (i + 1)) in
        loop (i - 1) (f sub_name acc)
    else
      acc
  in
  loop (String.length name - 1) acc
;;

let get_outer_namespace name =
  match String.index name '.' with
  | exception Not_found -> None
  | i -> Some (String.sub name ~pos:0 ~len:i)

module Whitelisted = struct
  (* White list the following attributes, as well as all their dot suffixes.

     Since these attributes are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.

     Sadly, the compiler silently ignores them if they are misplaced...
  *)
 let create_set fully_qualified_names =
    List.fold_left
      ~f:(fun acc name -> fold_dot_suffixes name ~init:acc ~f:String_set.add)
      ~init:String_set.empty
      fully_qualified_names

 let attributes =
   create_set
      [ "ocaml.warning"
      ; "ocaml.ppwarning"
      ; "ocaml.deprecated"
      ; "ocaml.doc"
      ; "ocaml.text"
      ; "ocaml.noalloc"
      ; "ocaml.unboxed"
      ; "ocaml.untagged"
      ; "ocaml.inline"
      ; "ocaml.inlined"
      ; "ocaml.specialise"
      ; "ocaml.specialised"
      ; "ocaml.unroll"
      ]

  (* White list the following extensions.

     Since these extensions are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.
  *)
  let extensions =
    create_set
      [ "ocaml.error"
      ; "ocaml.extension_constructor"
      ]

  let is_whitelisted ~kind name =
    match kind with
    | `Attribute -> String_set.mem name attributes
    | `Extension -> String_set.mem name extensions

  let get_attribute_list () = String_set.elements attributes
  let get_extension_list () = String_set.elements extensions
end

module Reserved_namespaces = struct
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 5

  let reserve ns = Hashtbl.add tbl ~key:ns ~data:()

  let () = reserve "merlin"

  let is_in_reserved_namespaces name =
    match get_outer_namespace name with
    | Some ns -> Hashtbl.mem tbl ns
    | _ -> false

  let check_not_reserved ~kind name =
    let kind, list =
      match kind with
      | `Attribute -> "attribute", Whitelisted.attributes
      | `Extension -> "extension", Whitelisted.extensions
    in
    if String_set.mem name list then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as it matches an \
         %s reserved by the compiler"
        kind name kind
    else if is_in_reserved_namespaces name then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as its namespace \
         is marked as reserved"
        kind name

end

module Registrar = struct
  type element =
    { fully_qualified_name : string
    ; declared_at          : Caller_id.t
    }

  type all_for_context = { mutable all : element String_map.t }

  type 'a t =
    { all_by_context    : ('a, all_for_context) Hashtbl.t
    ; skip              : string list
    ; kind              : string
    ; string_of_context : 'a -> string option
    }

  let create ~kind ~current_file ~string_of_context =
    { all_by_context = Hashtbl.create 32
    ; skip           = [current_file; __FILE__]
    ; kind
    ; string_of_context
    }

  let get_all_for_context t context =
    match Hashtbl.find t.all_by_context context with
    | x -> x
    | exception Not_found ->
      let all_for_context = { all = String_map.empty } in
      Hashtbl.add t.all_by_context ~key:context ~data:all_for_context;
      all_for_context
  ;;

  let register ~kind t context name =
    Reserved_namespaces.check_not_reserved ~kind name;
    let caller = Caller_id.get ~skip:t.skip in
    let all = get_all_for_context t context in
    (match String_map.find name all.all with
     | exception Not_found -> ()
     | e ->
       let declared_at = function
         | None -> ""
         | Some (loc : Printexc.location) ->
           Printf.sprintf " declared at %s:%d" loc.filename loc.line_number
       in
       let context =
         match t.string_of_context context with
         | None -> ""
         | Some s -> " on " ^ s ^ "s"
       in
       Printf.ksprintf
         failwith "%s '%s'%s%s matches %s '%s'%s"
         (String.capitalize t.kind) name context (declared_at caller)
         t.kind e.fully_qualified_name (declared_at e.declared_at)
    );
    let t =
      { fully_qualified_name = name
      ; declared_at          = caller
      }
    in
    all.all <- fold_dot_suffixes name ~init:all.all ~f:(fun name acc ->
      String_map.add acc ~key:name ~data:t);
  ;;

  let spellcheck t context ?(white_list=[]) name =
    let all =
      let all = get_all_for_context t context in
      String_map.fold all.all ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
    in
    match Spellcheck.spellcheck (all @ white_list) name with
    | Some _ as x -> x
    | None ->
      let other_contexts =
        Hashtbl.fold t.all_by_context ~init:[] ~f:(fun ~key:ctx ~data:{ all } acc ->
          if context <> ctx && String_map.mem name all then
            match t.string_of_context ctx with
            | None -> acc
            | Some s -> (s ^ "s") :: acc
          else
            acc)
      in
      let pp_text = Format.pp_print_text in
      let current_context ppf =
        match t.string_of_context context with
        | None | Some "" -> ()
        | Some s ->
          let a_or_an =
            match s.[0] with
            | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> "an"
            | _ -> "a"
          in
          Format.fprintf ppf "@ but@ is@ used@ here@ in@ the@ context@ of@ %s@ %a"
            a_or_an pp_text s
      in
      match List.sort ~cmp:(fun x y -> - (String.compare x y)) other_contexts with
      | [] -> None
      | [c] ->
        Some
          (Format.asprintf
             "@[Hint:@ `%s'@ is@ available@ for@ %a%t.@]@\n\
              Did you put it at the wrong level?"
             name pp_text c current_context)
      | last :: rev_others ->
        let others = List.rev rev_others in
        Some
          (Format.asprintf
             "@[Hint:@ `%s'@ is@ available@ for@ %a@ and@ %a%t.@]@\n\
              Did you put it at the wrong level?"
             name
             (Format.pp_print_list pp_text
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
             others pp_text last current_context)
  ;;

  let raise_errorf t context ?white_list fmt (name : string Location.loc) =
    Printf.ksprintf (fun msg ->
      match spellcheck t context name.txt ?white_list with
      | None ->
        Location.raise_errorf ~loc:name.loc "%s" msg
      | Some s ->
        Location.raise_errorf ~loc:name.loc "%s.\n%s" msg s)
      fmt name.txt
  ;;
end
