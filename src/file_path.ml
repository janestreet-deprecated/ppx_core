open StdLabels

let is_prefix ~prefix x =
  let prefix_len = String.length prefix in
  String.length x >= prefix_len && prefix = String.sub x ~pos:0 ~len:prefix_len
;;

let chop_prefix ~prefix x =
  if is_prefix ~prefix x then
    let prefix_len = String.length prefix in
    Some (String.sub x ~pos:prefix_len ~len:(String.length x - prefix_len))
  else
    None
;;

let get_default_path (loc : Location.t) =
  let fname = loc.loc_start.pos_fname in
  match chop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None       -> fname
;;

let get_default_path_str : Parsetree.structure -> string = function
  | [] -> ""
  | { pstr_loc = loc; _ } :: _ -> get_default_path loc
;;

let get_default_path_sig : Parsetree.signature -> string = function
  | [] -> ""
  | { psig_loc = loc; _ } :: _ -> get_default_path loc
;;
