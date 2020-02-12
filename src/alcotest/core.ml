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
open Utils

let compare_int : int -> int -> int = compare

module IntSet = Set.Make (struct
  type t = int

  let compare : int -> int -> int = compare
end)

exception Registration_error of string

exception Check_error of string

module type S = sig
  type return

  type speed_level = [ `Quick | `Slow ]

  type 'a test_case = string * speed_level * ('a -> return)

  exception Test_error

  val test_case : string -> speed_level -> ('a -> return) -> 'a test_case

  type 'a test = string * 'a test_case list

  val list_tests : 'a test list -> return

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?tail_errors:[ `Unlimited | `Limit of int ] ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:Re.re option * IntSet.t option ->
    ?log_dir:string ->
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

  type speed_level = [ `Quick | `Slow ]

  exception Test_error

  let compare_path (`Path (s, i)) (`Path (s', i')) =
    match String.compare s s' with 0 -> compare_int i i' | n -> n

  let short_string_of_path (`Path (n, i)) = Printf.sprintf "%s.%03d" n i

  let file_of_path (`Path (n, i)) =
    let path = `Path (String.Ascii.lowercase n, i) in
    Printf.sprintf "%s.output" (short_string_of_path path)

  type 'a rrun = 'a -> Pp.run_result M.t

  type 'a test_case = string * speed_level * 'a run

  let test_case n s f = (n, s, f)

  type 'a test = string * 'a test_case list

  module Suite : sig
    type 'a t

    type 'a test_case = {
      path : Pp.path;
      speed_level : speed_level;
      fn : 'a rrun;
    }

    val empty : unit -> 'a t

    val add : 'a t -> Pp.path * string * speed_level * 'a rrun -> 'a t

    val tests : 'a t -> 'a test_case list

    val doc_of_path : 'a t -> Pp.path -> string
  end = struct
    module String_set = Set.Make (String)

    type 'a test_case = {
      path : Pp.path;
      speed_level : speed_level;
      fn : 'a rrun;
    }

    type 'a t = {
      tests : 'a test_case list;
      (* caches computed from the library values. *)
      filepaths : String_set.t;
      doc : (Pp.path, string) Hashtbl.t;
    }

    let empty () =
      let tests = [] in
      let filepaths = String_set.empty in
      let doc = Hashtbl.create 0 in
      { tests; filepaths; doc }

    let check_path_is_unique t path =
      let exn_of_path (`Path (name, _)) =
        Registration_error (Fmt.strf "Duplicate test name: %s" name)
      in
      if String_set.mem (file_of_path path) t.filepaths then
        raise (exn_of_path path)

    let add t (path, doc, speed_level, fn) =
      check_path_is_unique t path;
      let tests = { path; speed_level; fn } :: t.tests in
      let filepaths = String_set.add (file_of_path path) t.filepaths in
      Hashtbl.add t.doc path doc;
      { t with tests; filepaths }

    let tests t = List.rev t.tests

    let doc_of_path t path = try Hashtbl.find t.doc path with Not_found -> ""
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
    tail_errors : [ `Unlimited | `Limit of int ];
    log_dir : string;
    run_id : string;
  }

  let empty () =
    let name = Filename.basename Sys.argv.(0) in
    let errors = [] in
    let suite = Suite.empty () in
    let max_label = 0 in
    let verbose = false in
    let compact = false in
    let tail_errors = `Unlimited in
    let speed_level = `Slow in
    let show_errors = false in
    let json = false in
    let log_dir = Sys.getcwd () in
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
      tail_errors;
      log_dir;
      run_id;
    }

  let compare_speed_level s1 s2 =
    match (s1, s2) with
    | `Quick, `Quick | `Slow, `Slow -> 0
    | `Quick, _ -> 1
    | _, `Quick -> -1

  (*
     Reverse a list, taking at most the first n elements of the original
     list.
  *)
  let rev_head n l =
    let rec aux acc n l =
      match l with
      | x :: xs -> if n > 0 then aux (x :: acc) (n - 1) xs else acc
      | [] -> acc
    in
    aux [] n l

  (*
     Show the last lines of a log file.
     The goal is to not clutter up the console output.
  *)
  let read_tail max_lines ic =
    let rev_lines = ref [] in
    try
      while true do
        rev_lines := input_line ic :: !rev_lines
      done;
      assert false
    with End_of_file ->
      let selected_lines =
        match max_lines with
        | `Unlimited -> List.rev !rev_lines
        | `Limit n -> rev_head n !rev_lines
      in
      let omitted_count = List.length !rev_lines - List.length selected_lines in
      let display_lines =
        if omitted_count = 0 then selected_lines
        else
          Fmt.strf "... (omitting %i line%a)" omitted_count Pp.pp_plural
            omitted_count
          :: selected_lines
      in
      String.concat ~sep:"\n" display_lines ^ "\n"

  let log_dir t = Filename.concat t.log_dir t.run_id

  let pp_suite_results t =
    Pp.suite_results ~verbose:t.verbose ~show_errors:t.show_errors ~json:t.json
      ~compact:t.compact ~log_dir:(log_dir t)

  let pp_event t =
    if not t.json then
      Pp.event ~compact:t.compact ~max_label:t.max_label
        ~doc_of_path:(Suite.doc_of_path t.suite)
    else Fmt.nop

  let pp_info t =
    Pp.info ~max_label:t.max_label ~doc_of_path:(Suite.doc_of_path t.suite)

  let output_file t path = Filename.concat (log_dir t) (file_of_path path)

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
    let log_dir = log_dir t in
    if not (Sys.file_exists log_dir) then (
      mkdir_p log_dir 0o770;
      if Sys.unix || Sys.cygwin then (
        let this_exe = Filename.concat t.log_dir t.name
        and latest = Filename.concat t.log_dir "latest" in
        if Sys.file_exists this_exe then Sys.remove this_exe;
        if Sys.file_exists latest then Sys.remove latest;
        Unix.symlink ~to_dir:true log_dir this_exe;
        Unix.symlink ~to_dir:true log_dir latest ) )
    else if not (Sys.is_directory log_dir) then
      failwith (Fmt.strf "exists but is not a directory: %S" log_dir)

  let color c ppf fmt = Fmt.(styled c string) ppf fmt

  let red_s fmt = color `Red fmt

  let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt

  let bold_s fmt = color `Bold fmt

  let pp_error ~verbose ~doc_of_path ~output_file ~tail_errors ppf (path, error)
      =
    let logs =
      let filename = output_file path in
      if verbose || not (Sys.file_exists filename) then Fmt.strf "%s\n" error
      else
        let file = open_in filename in
        let output = read_tail tail_errors file in
        close_in file;
        Fmt.strf "in `%s`:\n%s" filename output
    in
    Fmt.pf ppf "-- %s [%s] Failed --\n%s"
      (short_string_of_path path)
      (doc_of_path path) logs

  let failure : Pp.run_result -> bool = function
    | `Ok | `Skip -> false
    | `Error _ | `Exn _ | `Todo _ -> true

  let has_run : Pp.run_result -> bool = function
    | `Ok | `Error _ | `Exn _ -> true
    | `Skip | `Todo _ -> false

  let bt () = match Printexc.get_backtrace () with "" -> "" | s -> "\n" ^ s

  let exn path name err =
    let err = Printf.sprintf "%s%s" err (bt ()) in
    `Exn (path, name, err)

  let protect_test path (f : 'a run) : 'a rrun =
   fun args ->
    M.catch
      (fun () -> f args >|= fun () -> `Ok)
      (function
        | Check_error err ->
            let err = Printf.sprintf "Test error: %s%s" err (bt ()) in
            M.return @@ `Error (path, err)
        | Failure f -> M.return @@ exn path "failure" f
        | Invalid_argument f -> M.return @@ exn path "invalid" f
        | e -> M.return @@ exn path "exception" (Printexc.to_string e))

  let perform_test t args suite =
    let open Suite in
    let test = suite.fn in
    let pp_event = pp_event t in
    pp_event Fmt.stdout (`Start suite.path);
    test args >|= fun result ->
    (* Store errors *)
    let () =
      let pp_error =
        pp_error ~verbose:t.verbose
          ~doc_of_path:(Suite.doc_of_path t.suite)
          ~output_file:(output_file t) ~tail_errors:t.tail_errors
      in
      let error =
        match result with
        | `Exn (p, n, _) -> [ Fmt.strf "%a" pp_error (p, n) ]
        | `Error e -> [ Fmt.strf "%a" pp_error e ]
        | _ -> []
      in
      t.errors <- error @ t.errors
    in
    pp_event Fmt.stdout (`Result (suite.path, result));
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
    (try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e) >|= fun r ->
    flush stdout;
    flush stderr;
    Unix.dup2 fd_old_stdout fd_stdout;
    Unix.dup2 fd_old_stderr fd_stderr;
    Unix.close fd_old_stdout;
    Unix.close fd_old_stderr;
    match r with `Ok x -> x | `Error e -> raise e

  let skip_fun _ = M.return `Skip

  let skip_label test_case = Suite.{ test_case with fn = skip_fun }

  let filter_test_case (regexp, cases) test_case =
    let (`Path (n, i)) = test_case.Suite.path in
    let regexp_match = function None -> true | Some r -> Re.execp r n in
    let case_match = function None -> true | Some set -> IntSet.mem i set in
    regexp_match regexp && case_match cases

  let filter_test_cases ~subst path test_cases =
    test_cases
    |> List.filter_map (fun tc ->
           if filter_test_case path tc then Some tc
           else if subst then Some (skip_label tc)
           else None)

  let redirect_test_output t test_case =
    let output_file = output_file t test_case.Suite.path in
    let fn args =
      with_redirect output_file (fun () ->
          test_case.fn args >|= fun result ->
          Pp.rresult_error Fmt.stdout result;
          result)
    in
    { test_case with fn }

  let select_speed speed_level (test_case : 'a Suite.test_case) :
      'a Suite.test_case =
    if compare_speed_level test_case.speed_level speed_level >= 0 then test_case
    else Suite.{ test_case with fn = skip_fun }

  let result t test args =
    prepare t;
    let start_time = Unix.time () in
    let test =
      if t.verbose then test else List.map (redirect_test_output t) test
    in
    let test = List.map (select_speed t.speed_level) test in
    perform_tests t test args >|= fun results ->
    let time = Unix.time () -. start_time in
    let success = List.length (List.filter has_run results) in
    let failures = List.length (List.filter failure results) in
    Pp.{ time; success; failures; errors = List.rev t.errors }

  let list_registered_tests t () =
    Suite.tests t.suite
    |> List.map (fun t -> t.Suite.path)
    |> List.sort compare_path
    |> Fmt.(list ~sep:(const string "\n") (pp_info t) stdout)

  let validate_name name =
    let pattern = "^[a-zA-Z0-9_- ]+$" in
    let re = Re.(compile @@ Pcre.re pattern) in
    if not (Re.execp re name) then
      let msg =
        Fmt.strf "%a %S is not a valid test label (must match %s)." red "Error:"
          name pattern
      in
      Error msg
    else Ok ()

  let register t (name, (ts : 'a test_case list)) =
    let max_label = max t.max_label (String.length name) in
    let test_details =
      List.mapi
        (fun i (doc, speed, test) ->
          let path = `Path (name, i) in
          let doc =
            if doc = "" || doc.[String.length doc - 1] = '.' then doc
            else doc ^ "."
          in
          (path, doc, speed, protect_test path test))
        ts
    in
    let suite = List.fold_left Suite.add t.suite test_details in
    { t with suite; max_label }

  let register_all t tl =
    let validate (n, ts) = validate_name n |> Result.map (fun _ -> (n, ts)) in
    List.map validate tl
    |> List.lift_result
    |> Result.map (List.fold_left register t)

  let run_tests ?filter t () args =
    let suite = Suite.tests t.suite in
    ( match filter with
    | None -> result t suite args
    | Some labels ->
        let is_empty = filter_test_cases ~subst:false labels suite = [] in
        if is_empty then (
          Fmt.(pf stderr)
            "%a\n" red
            "Invalid request (no tests to run, filter skipped everything)!";
          exit 1 )
        else
          let tests = filter_test_cases ~subst:true labels suite in
          result t tests args )
    >|= fun result ->
    Fmt.(pf stdout) "%a" (pp_suite_results t) result;
    result.failures

  let list_tests (tl : 'a test list) =
    match register_all (empty ()) tl with
    | Error error_acc ->
        Fmt.(pf stderr) "%a\n" Fmt.(list string) error_acc;
        exit 1
    | Ok t ->
        list_registered_tests t ();
        M.return ()

  let default_log_dir () =
    let fname_concat l = List.fold_left Filename.concat "" l in
    fname_concat [ Sys.getcwd (); "_build"; "_tests" ]

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?tail_errors:[ `Unlimited | `Limit of int ] ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:Re.re option * IntSet.t option ->
    ?log_dir:string ->
    'a

  let run_with_args ?(and_exit = true) ?(verbose = false) ?(compact = false)
      ?(tail_errors = `Unlimited) ?(quick_only = false) ?(show_errors = false)
      ?(json = false) ?filter ?(log_dir = default_log_dir ()) name args
      (tl : 'a test list) =
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
        tail_errors;
        speed_level;
        json;
        show_errors;
        log_dir;
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

  let run ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors
      ?json ?filter ?log_dir name (tl : unit test list) =
    run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir name () tl
end
