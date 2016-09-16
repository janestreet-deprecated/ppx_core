(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let dispatch = function
  | After_rules ->
    let gen_and_mv generator files =
      let in_src = List.map (fun fn -> "src" / fn) files in
      rule ("gen " ^ String.concat " " files)
        ~dep:generator
        ~prods:in_src
        (fun _ _ ->
          Seq (Cmd (S [ P generator
                      ; A "-I"
                      ; A "+compiler-libs"
                      ; A "Parsetree"
                      ])
               :: List.map2 mv files in_src))
    in

    gen_and_mv "src/gen/gen.byte"
      [ "ast_traverse_map.ml"
      ; "ast_traverse_map.mli"
      ; "ast_traverse_iter.ml"
      ; "ast_traverse_iter.mli"
      ; "ast_traverse_fold.ml"
      ; "ast_traverse_fold.mli"
      ; "ast_traverse_fold_map.ml"
      ; "ast_traverse_fold_map.mli"
      ; "ast_traverse_map_with_context.ml"
      ; "ast_traverse_map_with_context.mli"
      ];

    gen_and_mv "src/gen/gen_ast_pattern.byte" ["ast_pattern_generated.ml"];
    gen_and_mv "src/gen/gen_ast_builder.byte" ["ast_builder_generated.ml"];

    let flags = S [A "-open"; A "Base"] in
    flag ["ocamldep"; "ocaml"; "open_base"] flags;
    flag ["compile";  "ocaml"; "open_base"] flags;
    flag ["doc";      "ocaml"; "open_base"] flags
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    dispatch hook;
    dispatch_default hook)

