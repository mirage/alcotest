(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

exception Check_error of string

(* Types *)
type speed_level = [`Quick | `Slow]

type run = unit -> unit

type path = Path of (string * int)

type run_result = [
  | `Ok
  | `Exn of path * string * string
  | `Error of path * string
  | `Skip
  | `Todo of string
]

type rrun = unit -> run_result

type test_case = string * speed_level * run

type test = string * test_case list

let quiet = ref false

(* global state *)
type t = {

  (* library values. *)
  name : string;
  tests: (path * rrun) list;

  (* caches computed from the library values. *)
  paths: path list;
  doc  : path -> string option;
  speed: path -> speed_level option;

  (* runtime state. *)
  mutable errors: string list;

  (* runtime options. *)
  max_label: int;
  speed_level: speed_level;
  show_errors: bool;
  json       : bool;
  verbose    : bool;
  test_dir   : string;

}

let empty () =
  let name = Filename.basename Sys.argv.(0) in
  let errors = [] in
  let paths = [] in
  let doc _ = None in
  let speed _ = None in
  let tests = [] in
  let max_label = 0 in
  let verbose = false in
  let speed_level = `Slow in
  let show_errors = false in
  let json = false in
  let test_dir = Sys.getcwd () in
  { name; errors; tests; paths; doc; speed;
    max_label; speed_level;
    show_errors; json; verbose; test_dir }

let compare_speed_level s1 s2 =
  match s1, s2 with
  | `Quick, `Quick
  | `Slow , `Slow  -> 0
  | `Quick, _      -> 1
  | _     , `Quick -> -1

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let terminal_columns =
  try
    (* terminfo *)
    with_process_in "tput cols" (fun ic -> int_of_string (input_line ic))
  with _ -> try
      (* GNU stty *)
      with_process_in "stty size" (fun ic ->
          match String.cuts (input_line ic) ~sep:" " with
          | [_ ; v] -> int_of_string v
          | _ -> failwith "stty")
    with _ -> try
        (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with _ ->
        (* default *)
        80

let line ppf ?color c =
  let line = String.v ~len:terminal_columns (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string)  line
  | None   -> Fmt.pf ppf "%s\n%!"line

let left nb pp ppf a =
  let s = Fmt.to_to_string pp a in
  let nb = nb - String.length s in
  if nb <= 0 then pp ppf a
  else (
    pp ppf a;
    Fmt.string ppf (String.v ~len:nb (fun _ -> ' '))
  )

let print t k = if not t.json then k Fmt.stdout

let string_of_channel ic =
  let n = 32768 in
  let s = Bytes.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_substring b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let short_string_of_path (Path (n, i)) = Printf.sprintf "%s.%03d" n i

let file_of_path path ext =
  Printf.sprintf "%s.%s" (short_string_of_path path) ext

let output_file t path = Filename.concat t.test_dir (file_of_path path "output")

let prepare t =
  if not (Sys.file_exists t.test_dir) then Unix.mkdir t.test_dir 0o755

let color c ppf fmt = Fmt.(styled c string) ppf fmt
let red_s fmt = color `Red fmt
let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt
let green_s fmt = color `Green fmt
let yellow_s fmt = color `Yellow fmt
let bold_s fmt = color `Bold fmt
let cyan_s fmt = color `Cyan fmt

let pp_path t ppf (Path (n, i)) =
  Fmt.pf ppf "%a%3d" (left (t.max_label+8) cyan_s) n i

let doc_of_path t path =
  match t.doc path with
  | None  -> ""
  | Some d -> d

let speed_of_path t path =
  match t.speed path with
  | None   -> `Slow
  | Some s -> s

let print_info t p =
  print t (fun ppf ->
      Fmt.pf ppf "%a   %s" (pp_path t) p (doc_of_path t p)
    )

let left_c = 20

let error t path fmt =
  let filename = output_file t path in
  let output =
    if not (Sys.file_exists filename) then "--"
    else
      let file = open_in filename in
      let output = string_of_channel file in
      close_in file;
      output
  in
  print t (fun ppf -> left left_c red_s ppf "[ERROR]");
  print_info t path;
  Printf.kprintf (fun str ->
      let error =
        Fmt.strf "%s\n%s\n%s:\n%s\n%s\n"
          (Fmt.(strf_like stdout) "-- %s Failed --" (short_string_of_path path))
          (doc_of_path t path)
          filename output str in
      t.errors <- error :: t.errors
    ) fmt

let reset t = print t (fun ppf -> Fmt.string ppf "\r")
let newline t = print t (fun ppf -> Fmt.string ppf "\n")

let print_result t p = function
  | `Ok            ->
    print t (fun ppf -> left left_c green_s ppf "[OK]");
    print_info t p
  | `Exn (p, n, s) -> error t p "[%s] %s" n s
  | `Error (p, s)  -> error t p "%s" s
  | `Skip          ->
    print t (fun ppf -> left left_c yellow_s ppf "[SKIP]");
    print_info t p
  | `Todo _        ->
    print t (fun ppf -> left left_c yellow_s ppf "[TODO]");
    print_info t p

let print_event t = function
  | `Start p       ->
    print t (fun ppf -> left left_c yellow_s ppf " ...");
    print_info t p;
    if t.verbose then newline t
  | `Result (p, r) ->
    reset t;
    print_result t p r;
    newline t

let failure: run_result -> bool = function
  | `Ok
  | `Skip  -> false
  | `Error _
  | `Exn _
  | `Todo _ -> true

let has_run: run_result -> bool = function
  | `Ok
  | `Error _
  | `Exn _ -> true
  | `Skip
  | `Todo _    -> false

let bt () = match Printexc.get_backtrace () with "" -> "" | s  -> "\n" ^ s
let exn path name err =
  let err = Printf.sprintf "%s%s" err (bt ()) in
  `Exn (path, name, err)

let protect_test path (f:run): rrun =
  fun () ->
    try f (); `Ok
    with
    | Check_error err ->
      let err = Printf.sprintf "Test error: %s%s" err (bt ()) in
      `Error (path, err)
    | Failure f -> exn path "failure" f
    | Invalid_argument f -> exn path "invalid" f
    | e -> exn path "exception" (Printexc.to_string e)

let perform_test t (path, test) =
  print_event t (`Start path);
  let result = test () in
  print_event t (`Result (path, result));
  result

let perform_tests t tests = List.map (perform_test t) tests

let with_redirect file fn =
  flush stdout;
  flush stderr;
  let fd_stdout = Unix.descr_of_out_channel stdout in
  let fd_stderr = Unix.descr_of_out_channel stderr in
  let fd_old_stdout = Unix.dup fd_stdout in
  let fd_old_stderr = Unix.dup fd_stderr in
  let fd_file = Unix.(openfile file [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
  Unix.dup2 fd_file fd_stdout;
  Unix.dup2 fd_file fd_stderr;
  Unix.close fd_file;
  let r =
    try `Ok (fn ())
    with e -> `Error e in
  flush stdout;
  flush stderr;
  Unix.dup2 fd_old_stdout fd_stdout;
  Unix.dup2 fd_old_stderr fd_stderr;
  Unix.close fd_old_stdout;
  Unix.close fd_old_stderr;
  match r with
  | `Ok x -> x
  | `Error e -> raise e

let skip_fun () = `Skip

let skip_label (path, _) = path, skip_fun

let filter_test labels (test: path * rrun) =
  let Path (n, i), _ = test in
  match labels with
  | []    -> Some test
  | [m]   -> if n=m then Some test else None
  | [m;j] -> if n=m && int_of_string j = i then Some test else None
  | _     -> failwith "filter_test"

let map_test f l = List.map (fun (path, test) -> path, f path test) l

let filter_tests ~subst path tests =
  let tests = List.fold_left (fun acc test ->
      match filter_test path test with
      | None   -> if subst then skip_label test :: acc else acc
      | Some r -> r :: acc
    ) [] tests in
  List.rev tests

let redirect_test_output t path (f:rrun) =
  if t.verbose then f
  else fun () ->
    let output_file = output_file t path in
    with_redirect output_file (fun () ->
      let result = f () in
      begin match result with
        | `Error (_path, str) -> Printf.printf "%s\n" str
        | `Exn (_path, n, str) -> Printf.printf "[%s] %s\n" n str
        | `Ok | `Todo _ | `Skip -> ()
      end;
      result
    )

let select_speed t path (f:rrun): rrun =
  if compare_speed_level (speed_of_path t path) t.speed_level >= 0 then
    f
  else
    skip_fun

type result = {
  success: int;
  failures: int;
  time: float
}

(* Return the json for the api, dirty out, to avoid new dependencies *)
let json_of_result r =
  Printf.sprintf "{\"sucess\":%i,\"failures\":%i,\"time\":%f}"
    r.success r.failures r.time

let s = function 0 | 1 -> "" | _ -> "s"

let show_result t result =
  (* Function to display errors for each test *)
  let display_errors () = match result.failures with
    | 0 -> ()
    | _ ->
      if t.verbose || t.show_errors || result.failures = 1 then
        List.iter (fun error -> Printf.printf "%s\n" error) (List.rev t.errors)
  in
  match t.json with
  | true  -> Printf.printf "%s\n" (json_of_result result)
  | false ->
    display_errors ();
    let msg ppf () = match result.failures with
      | 0 -> green_s ppf "Test Successful"
      | n -> red     ppf "%d error%s!" n (s n)
    in
    Fmt.(pf stdout) "The full test results are available in `%s`.\n\
                     %a in %.3fs. %d test%s run.\n%!"
      t.test_dir msg () result.time result.success (s result.success)

let result t test =
  prepare t;
  let start_time = Sys.time () in
  let test = map_test (redirect_test_output t) test in
  let test = map_test (select_speed t) test in
  let results = perform_tests t test in
  let time = Sys.time () -. start_time in
  let success = List.length (List.filter has_run results) in
  let failures = List.filter failure results in
  { time; success; failures = List.length failures }

let list_tests t () =
  let paths = List.sort Pervasives.compare t.paths in
  List.iter (fun path ->
      Fmt.(pf stdout) "%a    %s\n" (pp_path t) path (doc_of_path t path)
    ) paths;
  0

let is_ascii s = String.for_all Char.Ascii.is_valid s

let err_ascii s =
  let err =
    Printf.sprintf
      "%S is not a valid test label (it should be an ASCII string), skipping." s
  in
  Fmt.(pf stderr) "%a %s\n%!" red "Error:" err

let register t name (ts:test_case list) =
  if not (is_ascii name) then (err_ascii name; t)
  else (
    let max_label = max t.max_label (String.length name) in
    let paths = Hashtbl.create 16 in
    let docs = Hashtbl.create 16 in
    let speeds = Hashtbl.create 16 in
    let ts = List.mapi (fun i (doc, speed, test) ->
        let path = Path (name, i) in
        let doc =
          if doc = "" || doc.[String.length doc - 1] = '.' then doc
          else doc ^ "." in
        Hashtbl.add paths path true;
        Hashtbl.add docs path doc;
        Hashtbl.add speeds path speed;
        path, protect_test path test
      ) ts in
    let tests = t.tests @ ts in
    let paths = Hashtbl.fold (fun k _ acc -> k :: acc) paths [] in
    let paths = t.paths @ paths in
    let doc p = try Some (Hashtbl.find docs p) with Not_found -> t.doc p in
    let speed p = try Some (Hashtbl.find speeds p) with Not_found -> t.speed p in
    { t with paths; tests; doc; speed; max_label; }
  )

exception Test_error

let bool_of_env name =
  try match Sys.getenv name with
    | "" | "0" | "false" -> false
    | _ -> true
  with Not_found -> false

let apply fn t test_dir verbose show_errors quick json =
  let show_errors = show_errors || bool_of_env "ALCOTEST_SHOW_ERRORS" in
  let speed_level = if quick then `Quick else `Slow in
  if json then quiet := false;
  let t = { t with verbose; test_dir; json; show_errors; speed_level } in
  fn t

let run_registred_tests t () =
  let result = result t t.tests in
  show_result t result;
  result.failures

let run_subtest t labels () =
  let is_empty = filter_tests ~subst:false labels t.tests = [] in
  if is_empty then (
    Fmt.(pf stderr) "%a\n" red "Invalid request!";
    exit 1
  ) else
    let tests = filter_tests ~subst:true labels t.tests in
    let result = result t tests in
    show_result t result;
    result.failures

open Cmdliner

let json =
  let doc = "Display JSON for the results, to be used by a script." in
  Arg.(value & flag & info ["json"] ~docv:"" ~doc)

let test_dir =
  let doc = "Where to store the log files of the tests." in
  Arg.(value & opt string "_tests"  & info ["o"] ~docv:"DIR" ~doc)

let verbose =
  let doc = "Display the test outputs." in
  Arg.(value & flag & info ["v"; "verbose"] ~docv:"" ~doc)

let show_errors =
  let doc = "Display the test errors. Can also be set using the \
             $(i,ALCOTEST_SHOW_ERRORS) env variable." in
  Arg.(value & flag & info ["e"; "show-errors"] ~docv:"" ~doc)

let quicktests =
  let doc = "Run only the quick tests." in
  Arg.(value & flag & info ["q"; "quick-tests"] ~docv:"" ~doc)

let of_env t =
  Term.(pure (apply (fun t -> t) t)
        $ test_dir $ verbose $ show_errors $ quicktests $ json)

let set_color style_renderer =
  Fmt_tty.setup_std_outputs ?style_renderer ()

let set_color = Term.(const set_color $ Fmt_cli.style_renderer ())

let default_cmd t =
  let doc = "Run all the tests." in
  Term.(pure run_registred_tests $ of_env t $ set_color),
  Term.info t.name ~version:Alcotest_version.v ~doc

let test_cmd t =
  let doc = "Run a given test." in
  let label =
    let doc = "The list of labels identifying a subsets of the tests to run" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"LABEL")
  in
  Term.(pure run_subtest $ of_env t $ label $ set_color),
  Term.info "test" ~doc

let list_cmd t =
  let doc = "List all available tests." in
  Term.(pure list_tests $ of_env t $ set_color),
  Term.info "list" ~doc

let run ?(and_exit = true) name (tl:test list) =
  Fmt.(pf stdout) "Testing %a.\n" bold_s name;
  let t = empty () in
  let t = List.fold_left (fun t (name, tests) -> register t name tests) t tl in
  match Term.eval_choice (default_cmd t) [list_cmd t; test_cmd t] with
  | `Ok 0    -> if and_exit then exit 0 else ()
  | `Error _ -> if and_exit then exit 1 else raise Test_error
  | `Ok i    -> if and_exit then exit i else raise Test_error
  | _        -> if and_exit then exit 0 else ()

module type TESTABLE = sig
  type t
  val pp: Format.formatter -> t -> unit
  val equal: t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

let int =
  let module M = struct
    type t = int
    let pp = Format.pp_print_int
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let char =
  let module M = struct
    type t = char
    let pp = Format.pp_print_char
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let string =
  let module M = struct
    type t = string
    let pp = Format.pp_print_string
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let bool =
  let module M = struct
    type t = bool
    let pp = Format.pp_print_bool
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let list (type a) elt =
  let (module Elt: TESTABLE with type t = a) = elt in
  let module M = struct
    type t = a list
    let pp = Fmt.Dump.list Elt.pp
    let equal l1 l2 =
      List.length l1 = List.length l2 && List.for_all2 (Elt.equal) l1 l2
  end in
  (module M: TESTABLE with type t = M.t)

let slist (type a) (a: a testable) compare: a list testable =
  let module A = (val a) in
  let module L = (val list a) in
  (module struct
    type t = A.t list
    let equal x y = L.equal (List.sort compare x) (List.sort compare y)
    let pp = L.pp
  end)

let of_pp (type a) pp: a testable =
  (module struct type t = a let equal = (=) let pp = pp end)

let pair (type a) (type b) (a:a testable) (b:b testable): (a * b) testable =
  let module A = (val a) in
  let module B = (val b) in
  (module struct
    type t = a * b
    let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2
    let pp ppf (a, b) = A.pp ppf a; Format.pp_print_cut ppf (); B.pp ppf b
  end)

let option (type a) elt =
  let (module Elt: TESTABLE with type t = a) = elt in
  let module M = struct
    type t = a option
    let pp fmt t = match t with
      | None   -> Format.pp_print_string fmt "None"
      | Some x -> Format.fprintf fmt "Some @[(%a)@]" Elt.pp x
    let equal x y = match x, y with
      | None  , None   -> true
      | Some x, Some y -> Elt.equal x y
      | _ -> false
  end in
  (module M: TESTABLE with type t = M.t)

let result (type a) (type e) a e =
  let (module A: TESTABLE with type t = a) = a in
  let (module E: TESTABLE with type t = e) = e in
  let module M = struct
    type t = (a, e) Result.result
    let pp fmt t = match t with
      | Result.Ok    t -> Format.fprintf fmt "Ok @[(%a)@]" A.pp t
      | Result.Error e -> Format.fprintf fmt "Error @[(%a)@]" E.pp e
    let equal x y = match x, y with
      | Result.Ok    x, Result.Ok    y -> A.equal x y
      | Result.Error x, Result.Error y -> E.equal x y
      | _             , _              -> false
  end in
  (module M: TESTABLE with type t = M.t)

let pass (type a) =
  let module M = struct
    type t = a
    let pp fmt _ = Format.pp_print_string fmt "Alcotest.pass"
    let equal _ _ = true
  end in
  (module M: TESTABLE with type t = M.t)

let show_line msg =
  if !quiet then ()
  else (
    line Fmt.stderr ~color:`Yellow '-';
    Printf.eprintf "ASSERT %s\n" msg;
    line Fmt.stderr ~color:`Yellow '-';
  )

let check_err fmt = Format.ksprintf (fun err -> raise (Check_error err)) fmt

let check (type a) (module T: TESTABLE with type t = a) msg x y =
  show_line msg;
  if not (T.equal x y) then (
    let buf = Buffer.create 20 in
    let fmt = Format.formatter_of_buffer buf in
    Format.fprintf fmt "Error %s: expecting %a, got %a." msg T.pp x T.pp y;
    Format.pp_print_flush fmt ();
    failwith (Buffer.contents buf)
  )

let fail msg =
  show_line msg;
  check_err "Error %s." msg

let collect_exception f =
  try f (); None with e -> Some e

let check_raises msg exn f =
  show_line msg;
  match collect_exception f with
    None ->
    check_err "Fail %s: expecting %s, got nothing." msg (Printexc.to_string exn)
  | Some e ->
    if e <> exn then
      check_err "Fail %s: expecting %s, got %s."
        msg (Printexc.to_string exn) (Printexc.to_string e)

let line (oc:out_channel) ?color c =
  let color = match color with
    | None         -> None
    | Some `Blue   -> Some `Cyan
    | Some `Yellow -> Some `Yellow
  in
  let str: string = Fmt.(to_to_string @@ fun ppf -> line ppf ?color) c in
  Printf.fprintf oc "%s" str
