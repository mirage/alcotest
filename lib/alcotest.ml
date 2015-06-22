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

let sp = Printf.sprintf

(* Types *)
type speed_level = [`Quick | `Slow]

type test_case = string * speed_level * (unit -> unit)

type test = string * test_case list

(* FIXME: should remove dependency to OUnit *)
type node = OUnit.node

type single_test = OUnit.test
let single_test fn = OUnit.TestCase fn

(* global state *)
type t = {

  (* library options *)
  name : string;
  tests: single_test list;

  nodes: node list list;
  doc  : node list -> string option;
  speed: node list -> speed_level option;

  (* runtime state *)
  mutable errors: string list;

  (* runtime options *)
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
  let nodes = [] in
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
  { name; errors; tests; nodes; doc; speed;
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
  let split s c =
    Re_str.split (Re_str.regexp (sp "[%c]" c)) s in
  try           (* terminfo *)
    with_process_in "tput cols"
      (fun ic -> int_of_string (input_line ic))
  with _ -> try (* GNU stty *)
      with_process_in "stty size"
        (fun ic ->
           match split (input_line ic) ' ' with
           | [_ ; v] -> int_of_string v
           | _ -> failwith "stty")
    with _ -> try (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with _ ->
        80

let line oc ?color c =
  let line = match color with
    | Some `Blue   -> blue_s (String.make terminal_columns c)
    | Some `Yellow -> yellow_s (String.make terminal_columns c)
    | None         -> String.make terminal_columns c in
  Printf.fprintf oc "%s\n%!" line

let indent_left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    s ^ String.make nb ' '

let indent_right s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    String.make nb ' ' ^ s

let left_column t =
  t.max_label + t.max_doc + 16

let right_column t =
  terminal_columns
  - left_column t
  + 15

let right t s =
  if not t.json then Printf.printf "%s\n%!" (indent_right s (right_column t))

let left t s =
  if not t.json then Printf.printf "%s%!" (indent_left s (left_column t))

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

let file_of_path path ext =
  let path = List.tl (List.rev path) in
  sp "%s.%s" (String.concat "-" (List.map OUnit.string_of_node path)) ext

let output_file t path =
  Filename.concat t.log_dir (file_of_path path "output")

let prepare t =
  if not (Sys.file_exists t.log_dir) then Unix.mkdir t.log_dir 0o755

let string_of_node t = function
  | OUnit.ListItem i -> sp "%3d" i
  | OUnit.Label l    -> indent_left (sp "%s" (blue_s l)) (t.max_label+8)

let string_of_path t path =
  let rec aux = function
    | []   -> "--"
    | OUnit.ListItem _ :: t -> aux t
    | h::r ->
      string_of_node t h ^ String.concat " " (List.map (string_of_node t) r)
  in
  aux (List.rev path)

let doc_of_path t path =
  let path = List.rev (List.tl (List.rev path)) in
  match t.doc path with
  | None   -> ""
  | Some d -> d

let speed_of_path t path =
  let path = List.rev (List.tl (List.rev path)) in
  match t.speed path with
  | None   -> `Slow
  | Some s -> s

let short_string_of_path path =
  let path = List.rev (List.tl (List.rev path)) in
  OUnit.string_of_path path

let eprintf t fmt =
  Printf.ksprintf (fun str -> if not t.json then Printf.eprintf "%s" str) fmt

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
  right t (red "[ERROR]");
  Printf.kprintf (fun str ->
      let error =
        sp "%s\n%s\n%s:\n%s\n%s\n"
          (red "-- %s Failed --" (short_string_of_path path))
          (doc_of_path t path)
          filename output str in
      t.errors <- error :: t.errors
    ) fmt

let print_result t = function
  | OUnit.RSuccess p     -> right t (green "[OK]")
  | OUnit.RFailure (p,s) -> error t p "Failure: %s" s
  | OUnit.RError (p, s)  -> error t p "%s" s
  | OUnit.RSkip _        -> right t (yellow "[SKIP]")
  | OUnit.RTodo _        -> right t (yellow "[TODO]")

let print_event t = function
  | OUnit.EStart p  ->
    left t (sp "%s   %s" (string_of_path t p) (doc_of_path t p))
  | OUnit.EResult r -> print_result t r
  | OUnit.EEnd p    -> ()

let failure = function
  | OUnit.RSuccess _
  | OUnit.RSkip _  -> false
  | OUnit.RError _
  | OUnit.RFailure _
  | OUnit.RTodo _ -> true

let has_run = function
  | OUnit.RSuccess _
  | OUnit.RError _
  | OUnit.RFailure _ -> true
  | OUnit.RSkip _
  | OUnit.RTodo _    -> false

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

let map_test fn test =
  let rec aux path = function
    | OUnit.TestCase tf      -> OUnit.TestCase (fn path tf)
    | OUnit.TestList tl      ->
      let tl = List.mapi (fun i t -> aux (OUnit.ListItem i :: path) t) tl in
      OUnit.TestList tl
    | OUnit.TestLabel (l ,t) ->
      let t = aux (OUnit.Label l :: path) t in
      OUnit.TestLabel (l, t) in
  aux [] test

let same_label x y =
  (String.lowercase x) = (String.lowercase y)

let skip_fun () =
  OUnit.skip_if true "Not selected"

let skip =
  OUnit.TestCase skip_fun

let skip_label l =
  OUnit.TestLabel (l, skip)

let filter_test ~subst labels test =
  let rec aux path suffix test =
    match suffix, test with
    | []       , _
    | _        , OUnit.TestCase _       -> Some test
    | h::suffix, OUnit.TestLabel (l ,t) ->
      if same_label h l then
        match aux (OUnit.Label h :: path) suffix t with
        | None  -> if subst then Some (OUnit.TestLabel (l, skip)) else None
        | Some t ->Some (OUnit.TestLabel (l, t))
      else None
    | h::suffix, OUnit.TestList tl ->
      let tl, _ = List.fold_left (fun (tl, i) t ->
          if same_label (string_of_int i) h then
            match aux (OUnit.ListItem i :: path) suffix t with
            | None   -> (if subst then skip :: tl else tl), i+1
            | Some t -> (t :: tl), i+1
          else          (if subst then skip :: tl else tl), i+1
        ) ([], 0) tl in
      match List.rev tl with
      | [] -> None
      | tl -> Some (OUnit.TestList tl) in
  match test with
  | OUnit.TestCase _
  | OUnit.TestLabel _ -> aux [] labels test
  | OUnit.TestList tl ->
    let tl = List.fold_left (fun acc test ->
        match aux [] labels test with
        | None   -> if subst then skip :: acc else acc
        | Some r -> r :: acc
      ) [] tl in
    if tl = [] then None else Some (OUnit.TestList tl)

let filter_tests ~subst labels tests =
  let tests = List.fold_left (fun acc test ->
      match test with
      | OUnit.TestCase _
      | OUnit.TestList _ -> assert false
      | OUnit.TestLabel (l, _) ->
        match filter_test ~subst labels test with
        | None   -> if subst then skip_label l :: acc else acc
        | Some r -> r :: acc
    ) [] tests in
  List.rev tests

let redirect_test_output t labels test_fun =
  if t.verbose then test_fun
  else fun () ->
    let output_file = output_file t labels in
    with_redirect stdout output_file (fun () ->
        with_redirect stderr output_file (fun () ->
            try test_fun ()
            with exn -> begin
                eprintf t "\nTest error: %s\n" (Printexc.to_string exn);
                Printexc.print_backtrace stderr;
                raise exn
              end
          )
      )

let select_speed t labels test_fun =
  if compare_speed_level (speed_of_path t labels) t.speed_level >= 0 then
    test_fun
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
      if t.verbose || t.show_errors || result.success = 1 then
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
  let results = OUnit.perform_test (print_event t) test in
  let time = Sys.time () -. start_time in
  let success = List.length (List.filter has_run results) in
  let failures = List.filter failure results in
  { time; success; failures = List.length failures }

let list_tests t =
  let nodes = List.sort (fun x y -> compare (List.rev x) (List.rev y)) t.nodes in
  List.iter (fun path ->
      Printf.printf "%s    %s\n" (string_of_path t path) (doc_of_path t path)
    ) nodes

let register t name (ts:test_case list) =
  let nodes = Hashtbl.create 16 in
  let docs = Hashtbl.create 16 in
  let speeds = Hashtbl.create 16 in
  let max_label = ref t.max_label in
  let max_doc = ref t.max_doc in
  let ts = List.mapi (fun i (doc, speed, test) ->
      max_label := max !max_label (String.length name);
      max_doc   := max !max_doc (String.length doc);
      let path = [ OUnit.ListItem i; OUnit.Label name ] in
      let doc =
        if doc.[String.length doc - 1] = '.' then doc
        else doc ^ "." in
      Hashtbl.add nodes path true;
      Hashtbl.add docs path doc;
      Hashtbl.add speeds path speed;
      OUnit.TestCase test
    ) ts in
  let tests = t.tests @ [ OUnit.TestLabel (name, OUnit.TestList ts) ] in
  let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) nodes [] in
  let nodes = t.nodes @ List.rev nodes in
  let doc p = try Some (Hashtbl.find docs p) with Not_found -> t.doc p in
  let speed p = try Some (Hashtbl.find speeds p) with Not_found -> t.speed p in
  let max_label = !max_label in
  let max_doc = !max_doc in
  { t with nodes; tests; doc; speed; max_label; max_doc }

exception Test_error

let bool_of_env name =
  try match Sys.getenv name with
    | "" | "0" | "false" -> false
    | _ -> true
  with Not_found -> false

let apply fn t log_dir verbose show_errors quick json =
  let show_errors = show_errors || bool_of_env "ALCOTEST_SHOW_ERRORS" in
  let speed_level = if quick then `Quick else `Slow in
  let t = { t with verbose; log_dir; json; show_errors; speed_level } in
  fn t

let run_registred_tests t =
  let tests = OUnit.TestList t.tests in
  let result = result t tests in
  show_result t result;
  if result.failures > 0 then raise Test_error

let run_subtest t labels =
  let is_empty = filter_tests ~subst:false labels t.tests = [] in
  if is_empty then (
    Printf.printf "%s\n" (red "Invalid request!"); exit 1
  ) else
    let tests = filter_tests ~subst:true labels t.tests in
    let tests = OUnit.TestList tests in
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
