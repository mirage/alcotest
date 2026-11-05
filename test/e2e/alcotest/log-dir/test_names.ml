(* Suite and group names become file names in the log directory: whatever the
   name, each suite gets its own directory directly under [run], each test its
   own log file in the directory of its run, and nothing is written outside the
   log directory. *)
let names =
  [
    "test/foo.ml";
    "a\\b";
    ".";
    "..";
    "...";
    "../escape";
    "/absolute";
    ".hidden";
    "trailing.";
    "trailing ";
    "nul";
    "CON";
    "com1.txt";
    "Lpt9";
    "latest";
    "run";
    "with space";
    "a:b*c?d\"e<f>g|h";
    "caf\xc3\xa9";
    "\xe6\x97\xa5\xe6\x9c\xac";
    "invalid \xff utf-8";
    "nul\000byte";
    "new\nline";
    String.make 300 'x';
    String.make 300 'x' ^ "y";
    (* Each invalid byte is written as a 3-byte character. *)
    String.make 200 '\xff';
  ]

let fail fmt = Printf.ksprintf failwith fmt

(* Windows drops a trailing dot or space, and reserves device names even with
   an extension, so such names are checked on every system. *)
let check_portable name =
  let n = String.length name in
  let base = List.hd (String.split_on_char '.' name) in
  let device =
    match String.uppercase_ascii base with
    | "CON" | "PRN" | "AUX" | "NUL" -> true
    | b ->
        String.length b = 4
        && List.mem (String.sub b 0 3) [ "COM"; "LPT" ]
        && b.[3] >= '1'
        && b.[3] <= '9'
  in
  if
    n = 0
    || n > 255
    || name.[0] = '.'
    || name.[n - 1] = '.'
    || name.[n - 1] = ' '
    || device
  then fail "not a portable file name: %S" name

let entries dir =
  let names = List.sort compare (Array.to_list (Sys.readdir dir)) in
  List.iter check_portable names;
  names

let check_outside parent =
  if entries parent <> [ "logs" ] then
    fail "written outside the log directory: %s"
      (String.concat ", " (entries parent))

(* The directories of the runs of each suite. *)
let runs root =
  let top =
    if Sys.win32 then [ "junit"; "run" ] else [ "junit"; "latest"; "run" ]
  in
  if entries root <> top then
    fail "unexpected entries: %s" (String.concat ", " (entries root));
  let run = Filename.concat root "run" in
  List.concat_map
    (fun suite ->
      let dir = Filename.concat run suite in
      if not (Sys.is_directory dir) then fail "not a directory: %S" suite;
      List.map (Filename.concat dir) (entries dir))
    (entries run)

let test_suites () =
  let parent = Filename.temp_file "alcotest" "" in
  Sys.remove parent;
  Sys.mkdir parent 0o755;
  let root = Filename.concat parent "logs" in
  List.iter
    (fun name ->
      Alcotest.run ~and_exit:false ~log_dir:root name
        [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ])
    names;
  check_outside parent;
  let runs = runs root in
  if List.length runs <> List.length names then
    fail "expected %d runs, found %d" (List.length names) (List.length runs);
  List.iter
    (fun dir ->
      if entries dir <> [ "t.000.output" ] then fail "unexpected run %S" dir;
      let report =
        Printf.sprintf "%s-%s.xml"
          (Filename.basename (Filename.dirname dir))
          (Filename.basename dir)
      in
      if not (List.mem report (entries (Filename.concat root "junit"))) then
        fail "no report %S" report)
    runs

let test_groups () =
  let parent = Filename.temp_file "alcotest" "" in
  Sys.remove parent;
  Sys.mkdir parent 0o755;
  let root = Filename.concat parent "logs" in
  let test name = (name, [ Alcotest.test_case "ok" `Quick ignore ]) in
  Alcotest.run ~and_exit:false ~log_dir:root "groups" (List.map test names);
  check_outside parent;
  match runs root with
  | [ dir ] ->
      let logs = entries dir in
      if List.length logs <> List.length names then
        fail "expected %d logs, found %d" (List.length names) (List.length logs)
  | runs -> fail "expected one run, found %d" (List.length runs)

let () =
  test_suites ();
  test_groups ()
