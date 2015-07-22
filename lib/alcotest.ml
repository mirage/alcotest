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

exception Check_error of string

let sp = Printf.sprintf

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
  max_doc  : int;
  speed_level: speed_level;
  show_errors: bool;
  json       : bool;
  verbose    : bool;
  log_dir    : string;

}

let empty () =
  let name = Filename.basename Sys.argv.(0) in
  let errors = [] in
  let paths = [] in
  let doc _ = None in
  let speed _ = None in
  let tests = [] in
  let max_label = 0 in
  let max_doc = 0 in
  let verbose = false in
  let speed_level = `Slow in
  let show_errors = false in
  let json = false in
  let log_dir = Sys.getcwd () in
  { name; errors; tests; paths; doc; speed;
    max_label; max_doc; speed_level;
    show_errors; json; verbose; log_dir }

let compare_speed_level s1 s2 =
  match s1, s2 with
  | `Quick, `Quick
  | `Slow , `Slow  -> 0
  | `Quick, _      -> 1
  | _     , `Quick -> -1

(* Printers *)

let red    fmt = sp ("\027[31m"^^fmt^^"\027[m")
let green  fmt = sp ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sp ("\027[33m"^^fmt^^"\027[m")
let blue   fmt = sp ("\027[36m"^^fmt^^"\027[m")

let red_s    = red "%s"
let green_s  = green "%s"
let yellow_s = yellow "%s"
let blue_s   = blue "%s"

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let dup oc =
  Unix.out_channel_of_descr (Unix.dup (Unix.descr_of_out_channel oc))

let terminal_columns =
  try
    (* terminfo *)
    with_process_in "tput cols" (fun ic -> int_of_string (input_line ic))
  with _ -> try
      (* GNU stty *)
      with_process_in "stty size" (fun ic ->
          match Stringext.split (input_line ic) ~on:' ' with
          | [_ ; v] -> int_of_string v
          | _ -> failwith "stty")
    with _ -> try
        (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with _ ->
        (* default *)
        80

let line oc ?color c =
  let line = match color with
    | Some `Blue   -> blue_s (String.make terminal_columns c)
    | Some `Yellow -> yellow_s (String.make terminal_columns c)
    | None         -> String.make terminal_columns c in
  Printf.fprintf oc "%s\n%!" line

let left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then s
  else s ^ String.make nb ' '

let print t s = if not t.json then Printf.printf "%s%!" s

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

let short_string_of_path (Path (n, i)) = sp "%s.%3d" n i
let file_of_path path ext = sp "%s.%s" (short_string_of_path path) ext
let output_file t path = Filename.concat t.log_dir (file_of_path path "output")

let prepare t =
  if not (Sys.file_exists t.log_dir) then Unix.mkdir t.log_dir 0o755

let string_of_path t (Path (n, i)) =
  sp "%s%3d" (left (sp "%s" (blue_s n)) (t.max_label+8)) i

let doc_of_path t path =
  match t.doc path with
  | None  -> ""
  | Some d -> d

let speed_of_path t path =
  match t.speed path with
  | None   -> `Slow
  | Some s -> s

let eprintf t fmt =
  Printf.ksprintf (fun str -> if not t.json then Printf.eprintf "%s" str) fmt

let print_info t p =
  print t (sp "%s   %s" (string_of_path t p) (doc_of_path t p))

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
  print t (left (red "[ERROR]") left_c);
  print_info t path;
  Printf.kprintf (fun str ->
      let error =
        sp "%s\n%s\n%s:\n%s\n%s\n"
          (red "-- %s Failed --" (short_string_of_path path))
          (doc_of_path t path)
          filename output str in
      t.errors <- error :: t.errors
    ) fmt

let reset t = print t "\r"
let newline t = print t "\n"

let print_result t p = function
  | `Ok            -> print t (left (green "[OK]") left_c); print_info t p
  | `Exn (p, n, s) -> error t p "[%s] %s" n s
  | `Error (p, s)  -> error t p "%s" s
  | `Skip          -> print t (left (yellow "[SKIP]") left_c); print_info t p
  | `Todo _        -> print t (left (yellow "[TODO]") left_c); print_info t p

let print_event t = function
  | `Start p       -> print t (left (yellow " ...") left_c); print_info t p
  | `Result (p, r) -> reset t; print_result t p r; newline t

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
  let err = sp "%s%s" err (bt ()) in
  `Exn (path, name, err)

let protect_test path (f:run): rrun =
  fun () ->
    try f (); `Ok
    with
    | Check_error err ->
      let err = sp "Test error: %s%s" err (bt ()) in
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

let with_redirect oc file fn =
  flush oc;
  let fd_oc = Unix.descr_of_out_channel oc in
  let fd_old = Unix.dup fd_oc in
  let fd_file = Unix.(openfile file [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
  Unix.dup2 fd_file fd_oc;
  Unix.close fd_file;
  let r =
    try `Ok (fn ())
    with e -> `Error e in
  flush oc;
  Unix.dup2 fd_old fd_oc;
  Unix.close fd_old;
  match r with
  | `Ok x -> x
  | `Error e -> raise e

let same_label x y = (String.lowercase x) = (String.lowercase y)
let skip_fun () = `Skip

let skip_label (path, _) = path, skip_fun

let filter_test ~subst labels (test: path * rrun) =
  let Path (n, i), _ = test in
  match labels with
  | []    -> Some test
  | [m]   -> if n=m then Some test else None
  | [m;j] -> if n=m && int_of_string j = i then Some test else None
  | _     -> failwith "filter_test"

let map_test f l = List.map (fun (path, test) -> path, f path test) l

let filter_tests ~subst path tests =
  let tests = List.fold_left (fun acc test ->
      match filter_test ~subst path test with
      | None   -> if subst then skip_label test :: acc else acc
      | Some r -> r :: acc
    ) [] tests in
  List.rev tests

let redirect_test_output t path (f:rrun) =
  if t.verbose then f
  else fun () ->
    let output_file = output_file t path in
    with_redirect stdout output_file
      (fun () -> with_redirect stderr output_file f)

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
  sp "{\"sucess\":%i,\"failures\":%i,\"time\":%f}" r.success r.failures r.time

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
    let msg = match result.failures with
      | 0 -> green "Test Successful"
      | n -> red_s (sp "%d error%s!" n (s n))
    in
    Printf.printf "%s in %.3fs. %d test%s run.\n%!"
      msg result.time result.success (s result.success)

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

let list_tests t =
  let paths = List.sort Pervasives.compare t.paths in
  List.iter (fun path ->
      Printf.printf "%s    %s\n" (string_of_path t path) (doc_of_path t path)
    ) paths

let register t name (ts:test_case list) =
  let paths = Hashtbl.create 16 in
  let docs = Hashtbl.create 16 in
  let speeds = Hashtbl.create 16 in
  let max_label = ref t.max_label in
  let max_doc = ref t.max_doc in
  let ts = List.mapi (fun i (doc, speed, test) ->
      max_label := max !max_label (String.length name);
      max_doc   := max !max_doc (String.length doc);
      let path = Path (name, i) in
      let doc =
        if doc.[String.length doc - 1] = '.' then doc
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
  let max_label = !max_label in
  let max_doc = !max_doc in
  { t with paths; tests; doc; speed; max_label; max_doc }

exception Test_error

let bool_of_env name =
  try match Sys.getenv name with
    | "" | "0" | "false" -> false
    | _ -> true
  with Not_found -> false

let apply fn t log_dir verbose show_errors quick json =
  let show_errors = show_errors || bool_of_env "ALCOTEST_SHOW_ERRORS" in
  let speed_level = if quick then `Quick else `Slow in
  if json then quiet := false;
  let t = { t with verbose; log_dir; json; show_errors; speed_level } in
  fn t

let run_registred_tests t =
  let result = result t t.tests in
  show_result t result;
  if result.failures > 0 then raise Test_error

let run_subtest t labels =
  let is_empty = filter_tests ~subst:false labels t.tests = [] in
  if is_empty then (
    Printf.printf "%s\n" (red "Invalid request!"); exit 1
  ) else
    let tests = filter_tests ~subst:true labels t.tests in
    let result = result t tests in
    show_result t result;
    if result.failures > 0 then raise Test_error

open Cmdliner

let json =
  let doc = "Display JSON for the results, to be used by a script." in
  Arg.(value & flag & info ["json"] ~docv:"" ~doc)

let test_dir =
  let doc = "Where to store the log files of the tests." in
  Arg.(value & opt string "./_tests/"  & info ["-o"] ~docv:"DIR" ~doc)

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

let default_cmd t =
  let doc = "Run all the tests." in
  Term.(pure run_registred_tests $ of_env t),
  Term.info t.name ~version:Alcotest_version.current ~doc

let test_cmd t =
  let doc = "Run a given test." in
  let label =
    let doc = "The list of labels identifying a subsets of the tests to run" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"LABEL")
  in
  Term.(pure run_subtest $ of_env t $ label),
  Term.info "test" ~doc

let list_cmd t =
  let doc = "List all available tests." in
  Term.(pure list_tests $ of_env t),
  Term.info "list" ~doc

let run ?(and_exit = true) name (tl:test list) =
  let t = empty () in
  let t = List.fold_left (fun t (name, tests) -> register t name tests) t tl in
  let err = Format.formatter_of_buffer (Buffer.create 10) in
  match Term.eval_choice ~err (default_cmd t) [list_cmd t; test_cmd t] with
  | `Error _ -> if and_exit then exit 1 else raise Test_error
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

let list (type a) elt =
  let (module Elt: TESTABLE with type t = a) = elt in
  let module M = struct
    type t = a list
    let rec pp_print_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
      | [] -> ()
      | [v] -> pp_v ppf v
      | v :: vs ->
        pp_v ppf v;
        pp_sep ppf ();
        pp_print_list ~pp_sep pp_v ppf vs
    let pp = pp_print_list Elt.pp
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

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

let show_line msg =
  if !quiet then ()
  else (
    line stderr ~color:`Yellow '-';
    Printf.eprintf "ASSERT %s\n" msg;
    line stderr ~color:`Yellow '-';
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

let check_raises msg exn f =
  show_line msg;
  try
    f ();
    check_err "Fail %s: expecting %s, got nothing." msg (Printexc.to_string exn)
  with e ->
    ()
