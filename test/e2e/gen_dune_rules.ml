let global_stanza filenames =
  let bases = List.map Filename.remove_extension filenames in
  let pp_sexp_list = Fmt.(list ~sep:(const string "\n   ")) in
  Fmt.pr
    {|(executables
 (names
   %a
 )
 (libraries alcotest)
 (modules
   %a
 )
)
|}
    (pp_sexp_list Fmt.string) bases (pp_sexp_list Fmt.string) bases

let example_rule_stanza ~expect_failure filename =
  let base = Filename.remove_extension filename in
  let expect_failure =
    if expect_failure then "../expect_failure.exe " else ""
  in
  (* Run Alcotest to get *.actual, then pass through the strip_randomness
     sanitiser to get *.processed. *)
  Fmt.pr
    {|
(rule
 (target %s.actual)
 (action
  (with-outputs-to %%{target}
   (run %s%%{dep:%s.exe})
  )
 )
)

(rule
 (target %s.processed)
 (action
  (with-outputs-to %%{target}
   (run ../strip_randomness.exe %%{dep:%s.actual})
  )
 )
)

|}
    base expect_failure base base base

let example_alias_stanza filename =
  let base = Filename.remove_extension filename in
  Fmt.pr
    {|
(alias
 (name runtest)
 (action
   (diff %s.expected %s.processed)
 )
)
|}
    base base

let is_example filename = Filename.check_suffix filename ".ml"

let main expect_failure =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_example
  |> function
  | [] -> () (* no tests to execute *)
  | tests ->
      global_stanza tests;
      List.iter
        (fun test ->
          example_rule_stanza ~expect_failure test;
          example_alias_stanza test)
        tests

open Cmdliner

let expect_failure =
  let doc =
    Arg.info ~doc:"Negate the return status of the tests" [ "expect-failure" ]
  in
  Arg.(value & flag doc)

let term =
  Term.
    (const main $ expect_failure, info ~version:"%%VERSION%%" "gen_dune_rules")

let () = Term.(exit @@ eval term)
