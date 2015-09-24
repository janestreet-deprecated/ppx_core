open Ppx_core.Std

let faulty_transformation = object
  inherit Ast_traverse.map as super

  method! expression = function
    | [%expr 42] -> [%expr 42]
    | e -> super#expression e
end

let apply st =
  let st = Attribute.freshen_and_collect#structure st in
  let st = faulty_transformation#structure st in
  Attribute.check_unused#structure st;
  try
    Attribute.check_all_seen ();
    None
  with exn ->
    Some exn
;;

let () =
  let input_without_attributes =
    [%str
      let x = 42
    ]
  in
  let input_with_attributes =
    [%str
      let x = (42 [@foo])
    ]
  in
  match
    apply input_without_attributes,
    apply input_with_attributes
  with
  | None, Some _ -> ()
  | _ ->
    prerr_endline "FAILED";
    exit 1
