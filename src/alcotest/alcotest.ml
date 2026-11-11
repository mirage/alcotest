module Unix_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)

  module Unix = struct
    include Unix

    (* Windows also accepts ['/'], which is not [Filename.dir_sep]. *)
    let native_separators path =
      if Sys.win32 then String.map (function '/' -> '\\' | c -> c) path
      else path

    let mkdir_p path mode =
      List.iter
        (fun path ->
          try Unix.mkdir path mode
          with Unix.Unix_error (Unix.EEXIST, _, _) ->
            if Sys.is_directory path then () (* the directory exists *)
            else Fmt.str "mkdir: %s: is a file" path |> failwith)
        (Alcotest_stdlib_ext.Path.ancestors ~sep:Filename.dir_sep
           (native_separators path))
  end

  open M.Syntax

  let time = Unix.gettimeofday

  let timestamp t =
    let tm = Unix.gmtime t in
    Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let write_file path contents =
    (try Unix.mkdir_p (Filename.dirname path) 0o770
     with Unix.Unix_error (e, _, p) ->
       raise (Sys_error (p ^ ": " ^ Unix.error_message e)));
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc contents)

  let getcwd = Sys.getcwd

  (* Test executables running in parallel share [root], so [latest] is
     replaced with a rename, which is atomic, and never removed. [target] is
     relative to [root]. *)
  let link_latest ~root ~uuid target =
    let latest = Filename.concat root "latest" in
    let tmp = latest ^ "." ^ uuid in
    Unix.symlink ~to_dir:true target tmp;
    Unix.rename tmp latest

  (* The time comes first, so that the runs of a suite are listed in order. *)
  let prepare_log_trap ~root ~suite ~uuid =
    let tm = Unix.gmtime (Unix.time ()) in
    let name =
      Fmt.str "%04d%02d%02d-%02d%02d%02d-%s" (tm.tm_year + 1900) (tm.tm_mon + 1)
        tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec uuid
    in
    let target = List.fold_left Filename.concat "run" [ suite; name ] in
    let dir = Filename.concat root target in
    if not (Sys.file_exists dir) then (
      Unix.mkdir_p dir 0o770;
      if (Sys.unix || Sys.cygwin) && Unix.has_symlink () then
        link_latest ~root ~uuid target)
    else if not (Sys.is_directory dir) then
      Fmt.failwith "exists but is not a directory: %S" dir;
    dir

  let stdout_isatty () = Unix.(isatty stdout)

  let stdout_columns () =
    match Option.bind (Sys.getenv_opt "ALCOTEST_COLUMNS") int_of_string_opt with
    | Some columns when columns > 0 -> Some columns
    | _ -> (
        match Terminal.get_dimensions () with
        | Some { columns; _ } when columns > 0 -> Some columns
        | _ -> None)

  external before_test :
    output:out_channel -> stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_before_test"

  external after_test : stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_after_test"

  (* On Windows, [Unix.stdout] and [Unix.stderr] cache their HANDLE, which a
     C-level dup2 would close. Redirecting through them keeps them valid. *)
  let redirect_std_outputs output =
    if Sys.win32 then (
      let saved_stdout = Unix.dup Unix.stdout
      and saved_stderr = Unix.dup Unix.stderr in
      let output = Unix.descr_of_out_channel output in
      Unix.dup2 output Unix.stdout;
      Unix.dup2 output Unix.stderr;
      fun () ->
        Unix.dup2 saved_stdout Unix.stdout;
        Unix.dup2 saved_stderr Unix.stderr;
        Unix.close saved_stdout;
        Unix.close saved_stderr)
    else (
      before_test ~output ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
      fun () -> after_test ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr)

  type file_descriptor = out_channel

  let log_trap_supported = true
  let file_exists = Sys.file_exists
  let open_write_only = open_out
  let close = close_out

  let with_redirect fd_file fn =
    let* () = M.return () in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    let restore_std_outputs = redirect_std_outputs fd_file in
    let+ r = try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    restore_std_outputs ();
    match r with `Ok x -> x | `Error e -> raise e

  let contains ~affix s =
    let exception Found in
    try
      let len = String.length affix in
      for i = 0 to String.length s - len do
        if String.sub s i len = affix then raise_notrace Found
      done;
      false
    with Found -> true

  (* The locale of the character type, in the order of POSIX.1-2024, Base
     Definitions, 8.2: "If the LC_ALL environment variable is defined and is not
     null, the value of LC_ALL shall be used.", then LC_CTYPE and LANG. The same
     section leaves codesets "implementation-defined", so both spellings count,
     as in tmux (tmux.c). *)
  let locale_is_utf_8 () =
    let non_empty var =
      match Sys.getenv_opt var with Some "" -> None | v -> v
    in
    match List.filter_map non_empty [ "LC_ALL"; "LC_CTYPE"; "LANG" ] with
    | [] -> false
    | locale :: _ ->
        let locale = String.uppercase_ascii locale in
        contains ~affix:"UTF-8" locale || contains ~affix:"UTF8" locale

  let setup_std_outputs ?style_renderer ?utf_8
      (stdout : Alcotest_engine.Formatters.stdout)
      (stderr : Alcotest_engine.Formatters.stderr) =
    let style_renderer oc =
      match style_renderer with
      | Some value -> value
      | None ->
          let dumb =
            match Sys.getenv "TERM" with
            | "dumb" | "" -> true
            | _ -> false
            | exception _ -> true
          in
          let is_a_tty =
            try Unix.(isatty (descr_of_out_channel oc))
            with Unix.Unix_error _ -> false
          in
          if (not dumb) && is_a_tty then `Ansi_tty else `None
    in
    let utf_8 =
      match utf_8 with Some value -> value | None -> locale_is_utf_8 ()
    in
    Fmt.set_style_renderer
      (stdout :> Format.formatter)
      (style_renderer Stdlib.stdout);
    Fmt.set_utf_8 (stdout :> Format.formatter) utf_8;
    Fmt.set_style_renderer
      (stderr :> Format.formatter)
      (style_renderer Stdlib.stderr);
    Fmt.set_utf_8 (stderr :> Format.formatter) utf_8

  (* Implementation similar to that of [Bos.Os.Dir]. *)
  let home_directory () =
    let env_var_fallback () =
      try Ok (Sys.getenv "HOME")
      with Not_found -> Error (`Msg "HOME environment variable is undefined")
    in
    if Sys.win32 then env_var_fallback ()
    else
      try
        let uid = Unix.getuid () in
        Ok (Unix.getpwuid uid).Unix.pw_dir
      with Not_found -> env_var_fallback ()
end

module V1 = struct
  include Alcotest_engine.V1.Test

  module T =
    Alcotest_engine.V1.Cli.Make (Unix_platform) (Alcotest_engine.Monad.Identity)

  include T
end

include V1
