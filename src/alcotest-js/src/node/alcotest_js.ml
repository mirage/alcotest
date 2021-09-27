module Node_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)
  open M.Syntax

  let time = Unix.gettimeofday
  let getcwd = Node.Process.cwd

  let unlink_if_exists file =
    let rec inner ~retries =
      try Node.Fs.unlink file with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          if retries > 5 then
            Fmt.failwith "Failed %d times to unlink file %s (Unix.EINTR)."
              retries file
          else inner ~retries:(retries + 1)
    in
    inner ~retries:0

  let symlink ~to_dir ~target ~link_name =
    let rec inner ~retries =
      try Node.Fs.symlink ~to_dir target link_name
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        if retries > 5 then
          Fmt.failwith "Failed %d times to create symlink %s (Unix.EEXIST)"
            retries target
        else (
          unlink_if_exists link_name;
          inner ~retries:(retries + 1))
    in
    inner ~retries:0

  let prepare_log_trap ~root ~uuid ~name =
    let dir = Filename.concat root uuid in
    if not (Node.Fs.exists dir) then (
      Node.mkdir_p dir 0o770;
      let this_exe = Filename.concat root name
      and latest = Filename.concat root "latest" in
      unlink_if_exists this_exe;
      unlink_if_exists latest;
      symlink ~to_dir:true ~target:dir ~link_name:this_exe;
      symlink ~to_dir:true ~target:dir ~link_name:latest)
    else if not (Node.Fs.is_directory dir) then
      Fmt.failwith "exists but is not a directory: %S" dir

  let stdout_isatty = Node.Process.stdout_isatty
  let stdout_columns () = Some (Node.Process.stdout_columns ())

  type file_descriptor = int

  let log_trap_supported = true
  let file_exists = Node.Fs.exists
  let open_write_only x = Node.Fs.openfile_for_writing x 0o666
  let close = Node.Fs.close

  let with_redirect fd_file fn =
    let* () = M.return () in
    Fmt.(flush stdout) ();
    Fmt.(flush stderr) ();
    let fd_write_stream = Node.Fs.create_write_stream fd_file "a" in
    let write_bound = Node.Fs.bind_write fd_write_stream in
    let _, reset_stdout = Node.Process.set_stdout_write write_bound in
    let _, reset_stderr = Node.Process.set_stderr_write write_bound in
    let+ r = try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e in
    Fmt.(flush stdout ());
    Fmt.(flush stderr ());
    reset_stdout ();
    reset_stderr ();
    match r with `Ok x -> x | `Error e -> raise e

  let setup_std_outputs = Fmt_tty.setup_std_outputs

  (* Implementation similar to that of [Bos.Os.Dir]. *)
  let home_directory () =
    (* Node doesn't have a binding to getpwuid :/ *)
    match Node.Process.home () with
    | Some v -> Ok v
    | None -> Error (`Msg "HOME environment variable is undefined")
end

module V1 = struct
  include Alcotest_engine.V1.Test

  module T =
    Alcotest_engine.V1.Cli.Make (Node_platform) (Alcotest_engine.Monad.Identity)

  include T
end

include V1
