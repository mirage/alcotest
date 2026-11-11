(* Under dune, the logs go next to the build context, wherever it is.
   Otherwise they go to [_build/_tests] in the current directory. *)
let run () =
  Alcotest.run ~and_exit:false "log root"
    [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ]

let check_runs root =
  let dir = Filename.concat root "run" in
  let runs = try Array.to_list (Sys.readdir dir) with Sys_error _ -> [] in
  if not (List.mem "log root" runs) then failwith ("no run directory in " ^ dir)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755)

let () =
  run ();
  (match Sys.getenv_opt "INSIDE_DUNE" with
  | Some context when not (Filename.is_relative context) ->
      check_runs (Filename.concat (Filename.dirname context) "_tests")
  | _ -> ());
  let top = Sys.getcwd () in
  Unix.putenv "DUNE_SOURCEROOT" "/nonexistent";
  (* A build directory outside the source tree, as with [DUNE_BUILD_DIR]. *)
  let build_dir = Filename.concat top "moved-build" in
  let context = Filename.concat build_dir "default" in
  let test_dir = Filename.concat context "test" in
  mkdir_p test_dir;
  Unix.putenv "INSIDE_DUNE" context;
  Sys.chdir test_dir;
  run ();
  check_runs (Filename.concat build_dir "_tests");
  (* Outside the build context. *)
  let outside = Filename.concat top "outside" in
  mkdir_p outside;
  Sys.chdir outside;
  run ();
  check_runs (List.fold_left Filename.concat outside [ "_build"; "_tests" ])
