(* Test executables run in parallel share one log directory, so several
   processes replace [latest] at the same time: none of them may fail. *)
let children = 32
let runs = 100

let child root =
  let argv = [| "race"; "-o"; root |] in
  for _ = 1 to runs do
    Alcotest.run ~argv ~and_exit:false ~verbose:false "race"
      [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ]
  done

let () =
  if not Sys.win32 then (
    let root = Printf.sprintf "race-%d" (Unix.getpid ()) in
    Sys.mkdir root 0o755;
    let pids =
      List.init children (fun _ ->
          match Unix.fork () with
          | 0 -> (
              let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
              Unix.dup2 null Unix.stdout;
              match child root with
              | () -> Unix._exit 0
              | exception e ->
                  prerr_endline (Printexc.to_string e);
                  Unix._exit 1)
          | pid -> pid)
    in
    let failed =
      List.filter
        (fun pid ->
          match Unix.waitpid [] pid with
          | _, Unix.WEXITED 0 -> false
          | _ -> true)
        pids
    in
    if failed <> [] then
      Printf.ksprintf failwith "%d of %d processes failed" (List.length failed)
        children;
    if not (Sys.is_directory (Filename.concat root "latest")) then
      failwith "no latest run")
