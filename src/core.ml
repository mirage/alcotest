(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

let compare_int : int -> int -> int = compare

module IntSet = Set.Make (struct
  type t = int

  let compare : int -> int -> int = compare
end)

type speed_level = [ `Quick | `Slow ]

exception Registration_error of string

exception Check_error of string

module type S = sig
  type return

  type 'a test_case = string * speed_level * ('a -> return)

  exception Test_error

  val test_case : string -> speed_level -> ('a -> return) -> 'a test_case

  type 'a test = string * 'a test_case list

  val list_tests : 'a test list -> return

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:Re.re option * IntSet.t option ->
    ?output_dir:string ->
    'a

  val run : (string -> unit test list -> return) with_options

  val run_with_args : (string -> 'a -> 'a test list -> return) with_options
end

module Make (M : Monad.S) = struct
  module M = Monad.Extend (M)
  include M.Infix

  (* Types *)
  type return = unit M.t

  type 'a run = 'a -> unit M.t

  type path = Path of (string * int)

  exception Test_error

  let compare_path (Path (s, i)) (Path (s', i')) =
    match String.compare s s' with 0 -> compare_int i i' | n -> n

  let short_string_of_path (Path (n, i)) = Printf.sprintf "%s.%03d" n i

  let file_of_path (Path (n, i)) =
    let path = Path (String.Ascii.lowercase n, i) in
    Printf.sprintf "%s.output" (short_string_of_path path)

  type run_result =
    [ `Ok
    | `Exn of path * string * string
    | `Error of path * string
    | `Skip
    | `Todo of string ]

  type 'a rrun = 'a -> run_result M.t

  type 'a test_case = string * speed_level * 'a run

  let test_case n s f = (n, s, f)

  type 'a test = string * 'a test_case list

  module Suite : sig
    type 'a t

    val empty : unit -> 'a t

    val add : 'a t -> path * string * speed_level * 'a rrun -> 'a t

    val tests : 'a t -> (path * 'a rrun) list

    val doc_of_path : 'a t -> path -> string

    val speed_of_path : 'a t -> path -> speed_level
  end = struct
    module String_set = Set.Make (String)

    type 'a t = {
      tests : (path * 'a rrun) list;
      (* caches computed from the library values. *)
      filepaths : String_set.t;
      doc : (path, string) Hashtbl.t;
      speed : (path, speed_level) Hashtbl.t;
    }

    let empty () =
      let tests = [] in
      let filepaths = String_set.empty in
      let doc = Hashtbl.create 0 in
      let speed = Hashtbl.create 0 in
      { tests; filepaths; doc; speed }

    let check_path_is_unique t path =
      let exn_of_path (Path (name, _)) =
        Registration_error (Fmt.strf "Duplicate test name: %s" name)
      in
      if String_set.mem (file_of_path path) t.filepaths then
        raise (exn_of_path path)

    let add t (path, doc, speed, testfn) =
      check_path_is_unique t path;
      let tests = (path, testfn) :: t.tests in
      let filepaths = String_set.add (file_of_path path) t.filepaths in
      Hashtbl.add t.doc path doc;
      Hashtbl.add t.speed path speed;
      { t with tests; filepaths }

    let tests t = List.rev t.tests

    let doc_of_path t path = try Hashtbl.find t.doc path with Not_found -> ""

    let speed_of_path t path =
      try Hashtbl.find t.speed path with Not_found -> `Slow
  end

  (* global state *)
  type 'a t = {
    (* library values. *)
    name : string;
    suite : 'a Suite.t;
    (* runtime state. *)
    mutable errors : string list;
    (* runtime options. *)
    max_label : int;
    speed_level : speed_level;
    show_errors : bool;
    json : bool;
    verbose : bool;
    compact : bool;
    test_dir : string;
    run_id : string;
  }

  let empty () =
    let name = Filename.basename Sys.argv.(0) in
    let errors = [] in
    let suite = Suite.empty () in
    let max_label = 0 in
    let verbose = false in
    let compact = false in
    let speed_level = `Slow in
    let show_errors = false in
    let json = false in
    let test_dir = Sys.getcwd () in
    let run_id = Uuidm.to_string ~upper:true Uuidm.nil in
    {
      name;
      errors;
      suite;
      max_label;
      speed_level;
      show_errors;
      json;
      verbose;
      compact;
      test_dir;
      run_id;
    }

  let compare_speed_level s1 s2 =
    match (s1, s2) with
    | `Quick, `Quick | `Slow, `Slow -> 0
    | `Quick, _ -> 1
    | _, `Quick -> -1

  let left nb pp ppf a =
    let s = Fmt.to_to_string pp a in
    let nb = nb - String.length s in
    if nb <= 0 then pp ppf a
    else (
      pp ppf a;
      Fmt.string ppf (String.v ~len:nb (fun _ -> ' ')) )

  let print t k = if not t.json then k Fmt.stdout

  let string_of_channel ic =
    let n = 32768 in
    let s = Bytes.create n in
    let b = Buffer.create 1024 in
    let rec iter ic b s =
      let nread = try input ic s 0 n with End_of_file -> 0 in
      if nread > 0 then (
        Buffer.add_substring b (Bytes.unsafe_to_string s) 0 nread;
        iter ic b s )
    in
    iter ic b s;
    Buffer.contents b

  let output_dir t = Filename.concat t.test_dir t.run_id

  let output_file t path = Filename.concat (output_dir t) (file_of_path path)

  let mkdir_p path mode =
    let is_win_drive_letter x =
      String.length x = 2 && x.[1] = ':' && Char.Ascii.is_letter x.[0]
    in
    let sep = Filename.dir_sep in
    let rec mk parent = function
      | [] -> ()
      | name :: names ->
          let path = parent ^ sep ^ name in
          ( try Unix.mkdir path mode
            with Unix.Unix_error (Unix.EEXIST, _, _) ->
              if Sys.is_directory path then () (* the directory exists *)
              else Fmt.strf "mkdir: %s: is a file" path |> failwith );
          mk path names
    in
    match String.cuts ~empty:true ~sep path with
    | "" :: xs -> mk sep xs
    (* check for Windows drive letter *)
    | dl :: xs when is_win_drive_letter dl -> mk dl xs
    | xs -> mk "." xs

  let prepare t =
    let test_dir = output_dir t in
    if not (Sys.file_exists test_dir) then (
      mkdir_p test_dir 0o770;
      if Sys.unix || Sys.cygwin then (
        let this_exe = Filename.concat t.test_dir t.name
        and latest = Filename.concat t.test_dir "latest" in
        if Sys.file_exists this_exe then Sys.remove this_exe;
        if Sys.file_exists latest then Sys.remove latest;
        Unix.symlink ~to_dir:true test_dir this_exe;
        Unix.symlink ~to_dir:true test_dir latest ) )
    else if not (Sys.is_directory test_dir) then
      failwith (Fmt.strf "exists but is not a directory: %S" test_dir)

  let color c ppf fmt = Fmt.(styled c string) ppf fmt

  let red_s fmt = color `Red fmt

  let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt

  let green_s fmt = color `Green fmt

  let yellow_s fmt = color `Yellow fmt

  let bold_s fmt = color `Bold fmt

  let cyan_s fmt = color `Cyan fmt

  let pp_path t ppf (Path (n, i)) =
    Fmt.pf ppf "%a%3d" (left (t.max_label + 8) cyan_s) n i

  let print_info t p =
    print t (fun ppf ->
        Fmt.pf ppf "%a   %s" (pp_path t) p (Suite.doc_of_path t.suite p))

  let left_c = 20

  let error t path fmt =
    Fmt.kstrf
      (fun error ->
        let logs =
          let filename = output_file t path in
          if t.verbose || not (Sys.file_exists filename) then
            Fmt.strf "%s\n" error
          else
            let file = open_in filename in
            let output = string_of_channel file in
            close_in file;
            Fmt.strf "in `%s`:\n%s" filename output
        in
        let error =
          Fmt.strf "-- %s [%s] Failed --\n%s"
            (short_string_of_path path)
            (Suite.doc_of_path t.suite path)
            logs
        in
        t.errors <- error :: t.errors)
      fmt

  let reset t = print t (fun ppf -> Fmt.string ppf "\r")

  let newline t = print t (fun ppf -> Fmt.string ppf "\n")

  let print_ch t ch = print t (fun ppf -> Fmt.string ppf ch)

  let print_full_result t p = function
    | `Ok ->
        print t (fun ppf -> left left_c green_s ppf "[OK]");
        print_info t p
    | `Exn _ ->
        print t (fun ppf -> left left_c red_s ppf "[FAIL]");
        print_info t p
    | `Error _ ->
        print t (fun ppf -> left left_c red_s ppf "[ERROR]");
        print_info t p
    | `Skip ->
        print t (fun ppf -> left left_c yellow_s ppf "[SKIP]");
        print_info t p
    | `Todo _ ->
        print t (fun ppf -> left left_c yellow_s ppf "[TODO]");
        print_info t p

  let print_compact_result t = function
    | `Exn _ -> print_ch t "F"
    | `Error _ -> print_ch t "E"
    | `Skip -> print_ch t "S"
    | `Todo _ -> print_ch t "T"
    | `Ok -> print_ch t "."

  let print_event t = function
    | `Start _ when t.compact -> ()
    | `Start p ->
        print t (fun ppf -> left left_c yellow_s ppf " ...");
        print_info t p
    | `Result (_, r) when t.compact -> print_compact_result t r
    | `Result (p, r) ->
        reset t;
        print_full_result t p r;
        newline t

  let failure : run_result -> bool = function
    | `Ok | `Skip -> false
    | `Error _ | `Exn _ | `Todo _ -> true

  let has_run : run_result -> bool = function
    | `Ok | `Error _ | `Exn _ -> true
    | `Skip | `Todo _ -> false

  let bt () = match Printexc.get_backtrace () with "" -> "" | s -> "\n" ^ s

  let exn path name err =
    let err = Printf.sprintf "%s%s" err (bt ()) in
    `Exn (path, name, err)

  let protect_test path (f : 'a run) : 'a rrun =
   fun args ->
    try
      f args >|= fun () ->
      `Ok
    with
    | Check_error err ->
        let err = Printf.sprintf "Test error: %s%s" err (bt ()) in
        M.return @@ `Error (path, err)
    | Failure f -> M.return @@ exn path "failure" f
    | Invalid_argument f -> M.return @@ exn path "invalid" f
    | e -> M.return @@ exn path "exception" (Printexc.to_string e)

  let perform_test t args (path, test) =
    print_event t (`Start path);
    test args >|= fun result ->
    (* Store errors *)
    let () =
      match result with
      | `Exn (p, n, s) -> error t p "[%s] %s" n s
      | `Error (p, s) -> error t p "%s" s
      | _ -> ()
    in
    print_event t (`Result (path, result));
    result

  let perform_tests t tests args = M.List.map_s (perform_test t args) tests

  let with_redirect file fn =
    flush stdout;
    flush stderr;
    let fd_stdout = Unix.descr_of_out_channel stdout in
    let fd_stderr = Unix.descr_of_out_channel stderr in
    let fd_old_stdout = Unix.dup fd_stdout in
    let fd_old_stderr = Unix.dup fd_stderr in
    let fd_file = Unix.(openfile file [ O_WRONLY; O_TRUNC; O_CREAT ] 0o660) in
    Unix.dup2 fd_file fd_stdout;
    Unix.dup2 fd_file fd_stderr;
    Unix.close fd_file;
    ( try
        fn () >|= fun o ->
        `Ok o
      with e -> M.return @@ `Error e )
    >|= fun r ->
    flush stdout;
    flush stderr;
    Unix.dup2 fd_old_stdout fd_stdout;
    Unix.dup2 fd_old_stderr fd_stderr;
    Unix.close fd_old_stdout;
    Unix.close fd_old_stderr;
    match r with `Ok x -> x | `Error e -> raise e

  let skip_fun _ = M.return `Skip

  let skip_label (path, _) = (path, skip_fun)

  let filter_test (regexp, cases) (test : path * 'a rrun) =
    let Path (n, i), _ = test in
    let regexp_match = function None -> true | Some r -> Re.execp r n in
    let case_match = function None -> true | Some set -> IntSet.mem i set in
    regexp_match regexp && case_match cases

  let map_test f l = List.map (fun (path, test) -> (path, f path test)) l

  let filter_tests ~subst path tests =
    let tests =
      List.fold_left
        (fun acc test ->
          if filter_test path test then test :: acc
          else if subst then skip_label test :: acc
          else acc)
        [] tests
    in
    List.rev tests

  let redirect_test_output t path (f : 'a rrun) =
    if t.verbose then f
    else fun args ->
      let output_file = output_file t path in
      with_redirect output_file (fun () ->
          f args >|= fun result ->
          ( match result with
          | `Error (_path, str) -> Printf.printf "%s\n" str
          | `Exn (_path, n, str) -> Printf.printf "[%s] %s\n" n str
          | `Ok | `Todo _ | `Skip -> () );
          result)

  let select_speed t path (f : 'a rrun) : 'a rrun =
    if
      compare_speed_level (Suite.speed_of_path t.suite path) t.speed_level >= 0
    then f
    else skip_fun

  type result = { success : int; failures : int; time : float }

  (* Return the json for the api, dirty out, to avoid new dependencies *)
  let json_of_result r =
    Printf.sprintf "{\"success\":%i,\"failures\":%i,\"time\":%f}" r.success
      r.failures r.time

  let s = function 0 | 1 -> "" | _ -> "s"

  let show_result t result =
    (* Function to display errors for each test *)
    let display_errors () =
      match result.failures with
      | 0 -> ()
      | _ ->
          if result.failures > 0 then
            let print_error error = Printf.printf "%s\n" error in
            if t.verbose || t.show_errors then
              List.iter print_error (List.rev t.errors)
            else print_error (List.hd (List.rev t.errors))
    in
    match t.json with
    | true -> Printf.printf "%s\n" (json_of_result result)
    | false ->
        if t.compact then newline t;
        display_errors ();
        let test_results ppf =
          match result.failures with
          | 0 -> green_s ppf "Test Successful"
          | n -> red ppf "%d error%s!" n (s n)
        in
        let full_logs ppf =
          if t.verbose then Fmt.string ppf ""
          else
            Fmt.pf ppf "The full test results are available in `%s`.\n"
              (output_dir t)
        in
        if (not t.compact) || result.failures > 0 then
          Fmt.pr "%t%t in %.3fs. %d test%s run.\n%!" full_logs test_results
            result.time result.success (s result.success)

  let result t test args =
    prepare t;
    let start_time = Unix.time () in
    let test = map_test (redirect_test_output t) test in
    let test = map_test (select_speed t) test in
    perform_tests t test args >|= fun results ->
    let time = Unix.time () -. start_time in
    let success = List.length (List.filter has_run results) in
    let failures = List.filter failure results in
    { time; success; failures = List.length failures }

  let list_registered_tests t () =
    let paths = List.map fst (Suite.tests t.suite) in
    let paths = List.sort compare_path paths in
    List.iter
      (fun path ->
        Fmt.(pf stdout)
          "%a    %s\n" (pp_path t) path
          (Suite.doc_of_path t.suite path))
      paths;
    M.return ()

  let validate_name name =
    let pattern = "^[a-zA-Z0-9_- ]+$" in
    let re = Re.(compile @@ Pcre.re pattern) in
    if not (Re.execp re name) then
      let msg =
        Fmt.strf "%a %S is not a valid test label (must match %s)." red
          "Error:" name pattern
      in
      Error msg
    else Ok ()

  let register t name (ts : 'a test_case list) =
    let max_label = max t.max_label (String.length name) in
    let test_details =
      List.mapi
        (fun i (doc, speed, test) ->
          let path = Path (name, i) in
          let doc =
            if doc = "" || doc.[String.length doc - 1] = '.' then doc
            else doc ^ "."
          in
          (path, doc, speed, protect_test path test))
        ts
    in
    let suite = List.fold_left Suite.add t.suite test_details in
    { t with suite; max_label }

  (* Accumulate name validation errors rather than failing fast *)
  let register_acc t_acc name (ts : 'a test_case list) =
    match (t_acc, validate_name name) with
    | Error error_acc, Error e -> Error (e :: error_acc)
    | Error error_acc, Ok () -> Error error_acc
    | Ok _, Error e -> Error [ e ]
    | Ok t, Ok () -> Ok (register t name ts)

  let register_all t tl =
    List.fold_left (fun t (name, tests) -> register_acc t name tests) (Ok t) tl

  let run_tests ?filter t () args =
    let suite = Suite.tests t.suite in
    ( match filter with
    | None -> result t suite args
    | Some labels ->
        let is_empty = filter_tests ~subst:false labels suite = [] in
        if is_empty then (
          Fmt.(pf stderr)
            "%a\n" red
            "Invalid request (no tests to run, filter skipped everything)!";
          exit 1 )
        else
          let tests = filter_tests ~subst:true labels suite in
          result t tests args )
    >|= fun result ->
    show_result t result;
    result.failures

  let list_tests (tl : 'a test list) =
    match register_all (empty ()) tl with
    | Error error_acc ->
        Fmt.(pf stderr) "%a\n" Fmt.(list string) (List.rev error_acc);
        exit 1
    | Ok t -> list_registered_tests t ()

  let default_output_dir () =
    let fname_concat l = List.fold_left Filename.concat "" l in
    fname_concat [ Sys.getcwd (); "_build"; "_tests" ]

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:Re.re option * IntSet.t option ->
    ?output_dir:string ->
    'a

  let run_with_args ?(and_exit = true) ?(verbose = false) ?(compact = false)
      ?(quick_only = false) ?(show_errors = false) ?(json = false) ?filter
      ?(output_dir = default_output_dir ()) name args (tl : 'a test list) =
    let speed_level = if quick_only then `Quick else `Slow in
    let random_state = Random.State.make_self_init () in
    let run_id = Uuidm.v4_gen random_state () |> Uuidm.to_string ~upper:true in
    let t =
      {
        (empty ()) with
        run_id;
        name;
        verbose;
        compact;
        speed_level;
        json;
        show_errors;
        test_dir = output_dir;
      }
    in
    match register_all t tl with
    | Error error_acc ->
        Fmt.(pf stderr) "%a\n" Fmt.(list string) (List.rev error_acc);
        exit 1
    | Ok t -> (
        ( Fmt.(pf stdout) "Testing %a.\n" bold_s name;
          Fmt.(pf stdout) "This run has ID `%s`.\n" run_id;
          run_tests ?filter t () args )
        >|= fun test_failures ->
        match (test_failures, and_exit) with
        | 0, true -> exit 0
        | 0, false -> ()
        | _, true -> exit 1
        | _, false -> raise Test_error )

  let run ?and_exit ?verbose ?compact ?quick_only ?show_errors ?json ?filter
      ?output_dir name (tl : unit test list) =
    run_with_args ?and_exit ?verbose ?compact ?quick_only ?show_errors ?json
      ?filter ?output_dir name () tl
end
