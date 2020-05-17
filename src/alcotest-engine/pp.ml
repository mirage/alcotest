(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019      Craig Ferguson    <craig@tarides.com>
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

let terminal_width = Fun.const 80

type path = [ `Path of string * int ]

type run_result =
  [ `Ok
  | `Exn of path * string * string
  | `Error of path * unit Fmt.t
  | `Skip
  | `Todo of string ]

let is_failure : run_result -> bool = function
  | `Ok | `Skip -> false
  | `Error _ | `Exn _ | `Todo _ -> true

type event = [ `Result of path * run_result | `Start of path ]

let rresult_error ppf = function
  | `Error (_, e) -> Fmt.pf ppf "%a@," e ()
  | `Exn (_, n, s) -> Fmt.pf ppf "[%s] %s@," n s
  | `Ok | `Todo _ | `Skip -> ()

(* Colours *)
let color c ppf fmt = Fmt.(styled c string) ppf fmt

let cyan_s fmt = color `Cyan fmt

let red_s fmt = color `Red fmt

let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt

let green_s fmt = color `Green fmt

let yellow_s fmt = color `Yellow fmt

let left_c = 14

let left nb pp ppf a =
  let s = Fmt.to_to_string pp a in
  let nb = nb - String.length s in
  if nb <= 0 then pp ppf a
  else (
    pp ppf a;
    Fmt.string ppf (String.v ~len:nb (fun _ -> ' ')) )

let pp_path ~max_label ppf (`Path (n, i)) =
  Fmt.pf ppf "%a%3d" (left (max_label + 8) cyan_s) n i

let info ~max_label ~doc_of_path ppf p =
  Fmt.pf ppf "%a   %s" (pp_path ~max_label) p (doc_of_path p)

let pp_result ppf = function
  | `Ok -> left left_c green_s ppf "[OK]"
  | `Exn _ -> left left_c red_s ppf "[FAIL]"
  | `Error _ -> left left_c red_s ppf "[ERROR]"
  | `Skip -> left left_c yellow_s ppf "[SKIP]"
  | `Todo _ -> left left_c yellow_s ppf "[TODO]"

let pp_result_compact ppf result =
  let char =
    match result with
    | `Ok -> '.'
    | `Exn _ -> 'F'
    | `Error _ -> 'E'
    | `Skip -> 'S'
    | `Todo _ -> 'T'
  in
  Fmt.char ppf char

let left_padding ~with_selector =
  let open Fmt in
  ( if with_selector then const (styled `Bold (styled `Red char)) '>'
  else const char ' ' )
  ++ const char ' '

let pp_result_full ~max_label ~doc_of_path ~selector_on_failure ppf
    (path, result) =
  let with_selector = selector_on_failure && is_failure result in
  Fmt.pf ppf "%a%a%a"
    (left_padding ~with_selector)
    () pp_result result
    (info ~max_label ~doc_of_path)
    path

let event_line ~max_label ~doc_of_path ppf = function
  | `Result (p, r) ->
      Fmt.pf ppf "%a%a" pp_result r (info ~max_label ~doc_of_path) p
  | _ -> assert false

let event ~compact ~max_label ~doc_of_path ~selector_on_failure ppf = function
  | `Start _ when compact -> ()
  | `Start p ->
      Fmt.pf ppf "%a"
        Fmt.(
          left_padding ~with_selector:false
          ++ const (left left_c yellow_s) "..."
          ++ const (info ~max_label ~doc_of_path) p)
        ()
  | `Result (_, r) when compact -> Fmt.pf ppf "%a" pp_result_compact r
  | `Result (p, r) ->
      Fmt.pf ppf "\r%a@,"
        (pp_result_full ~max_label ~doc_of_path ~selector_on_failure)
        (p, r)

type result = {
  success : int;
  failures : int;
  time : float;
  errors : unit Fmt.t list;
}

let pp_suite_errors ~show_all = function
  | [] -> Fmt.nop
  | x :: _ as xs -> (if show_all then xs else [ x ]) |> Fmt.concat

let pp_plural ppf x = Fmt.pf ppf (if x < 2 then "" else "s")

let quoted f = Fmt.(const char '`' ++ f ++ const char '\'')

let unicode_boxed (type a) (f : a Fmt.t) : a Fmt.t =
 fun ppf a ->
  (* Peek at the value being pretty-printed to determine the length of the box
     we're going to need. Fortunately, this will not include ANSII colour
     escapes. *)
  let true_width = Fmt.kstr String.length "| %a |" f a in
  let min_width = terminal_width () in
  let width = max min_width true_width in

  let right_padding = String.v ~len:(width - true_width) (fun _ -> ' ') in
  let s = Fmt.(const (styled `Faint string)) in
  let bars = List.init (width - 2) (fun _ -> "─") |> String.concat in
  let top = s ("┌" ^ bars ^ "┐")
  and mid = Fmt.(s "│ " ++ f ++ s (right_padding ^ " │"))
  and bottom = s ("└" ^ bars ^ "┘") in
  Fmt.pf ppf "%a" Fmt.(top ++ cut ++ mid ++ cut ++ bottom ++ cut) a

let pp_full_logs ppf log_dir =
  Fmt.pf ppf "Full test results in %a.@,"
    Fmt.(quoted (styled `Cyan string))
    log_dir

let pp_summary ppf r =
  let pp_failures ppf = function
    | 0 -> green_s ppf "Test Successful"
    | n -> red ppf "%d error%a!" n pp_plural n
  in
  Fmt.pf ppf "%a in %.3fs. %d test%a run.@," pp_failures r.failures r.time
    r.success pp_plural r.success

let suite_results ~verbose ~show_errors ~json ~compact ~log_dir ppf r =
  let print_summary = (not compact) || r.failures > 0 in
  match json with
  | true ->
      (* Return the json for the api, dirty out, to avoid new dependencies *)
      Fmt.pf ppf {|{
  "success": %i,
  "failures": %i,
  "time": %f
}
|}
        r.success r.failures r.time
  | false ->
      Fmt.pf ppf "\n%a%a%a%a"
        (if compact then Fmt.cut else Fmt.nop)
        ()
        (pp_suite_errors ~show_all:(verbose || show_errors) r.errors)
        ()
        (if print_summary && not verbose then pp_full_logs else Fmt.nop)
        log_dir
        (if print_summary then pp_summary else Fmt.nop)
        r
