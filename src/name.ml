open StdLabels
open MoreLabels

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
    String.sub pattern ~pos:start ~len:(len_pattern - start) = matched
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

  let register t context name =
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
      let current_context =
        match t.string_of_context context with
        | None | Some "" -> ""
        | Some s ->
          let a_or_an =
            match s.[0] with
            | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> "an"
            | _ -> "a"
          in
          Printf.sprintf " but is used here in the context of %s %s" a_or_an s
      in
      match List.sort ~cmp:(fun x y -> - (String.compare x y)) other_contexts with
      | [] -> None
      | [c] ->
        Some
          (Printf.sprintf "Hint: `%s' is available for %s%s. \
                           Did you put it at the wrong level?"
             name c current_context)
      | last :: rev_others ->
        let others = List.rev rev_others in
        Some
          (Printf.sprintf "Hint: `%s' is available for %s and %s%s. \
                           Did you put it at the wrong level?"
             name (String.concat ~sep:", " others) last current_context)
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
