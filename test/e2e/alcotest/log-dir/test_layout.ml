(* Each run has its own directory in [run/<suite>], named after its start time
   and ID, and [latest] points to the last run where symbolic links are
   supported. With [--junit], its report is in [junit]. *)
let run ?(junit = true) root name =
  let argv = if junit then [| name; "--junit" |] else [| name |] in
  Alcotest.run ~argv ~and_exit:false ~log_dir:root name
    [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ]

let entries dir = List.sort compare (Array.to_list (Sys.readdir dir))
let check what expected actual = if expected <> actual then failwith what

let () =
  let root = Filename.temp_file "alcotest" "" in
  Sys.remove root;
  run root "alpha";
  run root "beta";
  run root "alpha";
  let top =
    if Sys.win32 then [ "junit"; "run" ] else [ "junit"; "latest"; "run" ]
  in
  check "top level" top (entries root);
  let run_dir = Filename.concat root "run" in
  check "suites" [ "alpha"; "beta" ] (entries run_dir);
  let runs suite = entries (Filename.concat run_dir suite) in
  check "runs of alpha" 2 (List.length (runs "alpha"));
  check "runs of beta" 1 (List.length (runs "beta"));
  List.iter
    (fun suite ->
      List.iter
        (fun name ->
          (* [YYYYMMDD-HHMMSS-ID] *)
          check ("name " ^ name) true
            (String.length name = 24 && name.[8] = '-' && name.[15] = '-');
          let dir = List.fold_left Filename.concat run_dir [ suite; name ] in
          check ("logs of " ^ name) [ "t.000.output" ] (entries dir);
          let reports = entries (Filename.concat root "junit") in
          check ("report of " ^ name) true
            (List.mem (Printf.sprintf "%s-%s.xml" suite name) reports))
        (runs suite))
    [ "alpha"; "beta" ];
  (* Without [--junit], there is no report. *)
  let root = Filename.temp_file "alcotest" "" in
  Sys.remove root;
  run ~junit:false root "alpha";
  let top = if Sys.win32 then [ "run" ] else [ "latest"; "run" ] in
  check "no report" top (entries root)
