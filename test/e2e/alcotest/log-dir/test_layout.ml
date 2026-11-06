(* Each run has its own directory in [run/<suite>], named after its start time
   and ID, and [latest] points to the last one where symbolic links are
   supported. *)
let run root name =
  Alcotest.run ~and_exit:false ~log_dir:root name
    [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ]

let entries dir = List.sort compare (Array.to_list (Sys.readdir dir))
let check what expected actual = if expected <> actual then failwith what

let () =
  let root = Filename.temp_file "alcotest" "" in
  Sys.remove root;
  run root "alpha";
  run root "beta";
  run root "alpha";
  let top = if Sys.win32 then [ "run" ] else [ "latest"; "run" ] in
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
          check ("logs of " ^ name) [ "t.000.output" ] (entries dir))
        (runs suite))
    [ "alpha"; "beta" ]
