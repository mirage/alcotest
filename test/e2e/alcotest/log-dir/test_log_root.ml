(* Under dune, the logs go to the [_build] of the workspace, not to the one in
   [_build/default], where dune deletes what no rule wrote. *)
let () =
  Alcotest.run ~and_exit:false "log root"
    [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ];
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | None -> ()
  | Some root ->
      let dir =
        List.fold_left Filename.concat root [ "_build"; "_tests"; "run" ]
      in
      let runs = try Array.to_list (Sys.readdir dir) with Sys_error _ -> [] in
      if not (List.mem "log root" runs) then
        failwith ("no run directory in " ^ dir)
