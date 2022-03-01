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

include Pp_intf
include Pp_intf.Types
open! Import
open Model

let map_theta t ~f ppf = f (fun ppf () -> t ppf) ppf ()
let quoted f = Fmt.(const char '`' ++ f ++ const char '\'')
let plural ppf x = Fmt.pf ppf (if x < 2 then "" else "s")

module Tag : sig
  val pp : wrapped:bool -> tag Fmt.t
  val colour : tag -> Fmt.style
  val of_run_result : Run_result.t -> tag
end = struct
  let colour = function
    | `Ok -> `Green
    | `Fail -> `Red
    | `Skip | `Todo | `Assert -> `Yellow

  let string_of_tag : tag -> string = function
    | `Ok -> "PASS"
    | `Fail -> "FAIL"
    | `Skip -> "SKIP"
    | `Todo -> " —— "
    | `Assert -> "ASSERT"

  let of_run_result : Run_result.t -> tag = function
    | Ok -> `Ok
    | Exn _ | Error _ -> `Fail
    | Skip -> `Skip
    | Todo _ -> `Todo

  let pp ~wrapped ppf typ =
    let colour = colour typ in
    let tag = string_of_tag typ in
    let tag = if wrapped then "[" ^ tag ^ "]" else tag in
    Fmt.(styled colour string) ppf tag
end

let left_padding ~with_selector =
  let open Fmt in
  (if with_selector then const (styled `Bold (styled `Red char)) '>'
  else const char ' ')
  ++ const char ' '

let user_error fmt =
  Fmt.kstr
    (fun s ->
      Fmt.epr "%a: %s@." Fmt.(styled `Red string) "ERROR" s;
      exit 1)
    fmt

let tag = Tag.pp ~wrapped:false

module Make (P : Make_arg) = struct
  include Types

  let terminal_width =
    lazy (match P.stdout_columns () with Some w -> w | None -> 80)

  let rresult_error : Run_result.t Fmt.t =
   fun ppf -> function
    | Error (_, e) -> Fmt.pf ppf "%a@," e ()
    | Exn (_, n, e) -> Fmt.pf ppf "[%s] @[<v>%a@]" n e ()
    | Ok | Todo _ | Skip -> ()

  (* Colours *)
  let color c ppf fmt = Fmt.(styled c string) ppf fmt
  let red_s fmt = color `Red fmt
  let red ppf fmt = Fmt.kstr (fun str -> red_s ppf str) fmt
  let green_s fmt = color `Green fmt
  let left_gutter = 2
  let left_tag = 10
  let left_total = left_gutter + left_tag

  let left nb pp ppf a =
    let s = Fmt.to_to_string pp a in
    let nb = nb - String.length_utf8 s in
    if nb <= 0 then pp ppf a
    else (
      pp ppf a;
      Fmt.string ppf (String.v ~len:nb (fun _ -> ' ')))

  let info ?(available_width = Lazy.force terminal_width) ppf index =
    let name = Index.leaf_name index in
    let sep = Fmt.(const (styled `Faint string) " › ") in
    let sep_length = 3 in
    let rec aux available_width = function
      | [] ->
          if available_width < Safe_string.length name then (
            Safe_string.pp ppf
              (Safe_string.prefix (available_width - sep_length) name);
            Fmt.string ppf "...")
          else Safe_string.pp ppf name
      | x :: xs ->
          let len = Safe_string.length x in
          if available_width < len then (
            Safe_string.pp ppf (Safe_string.prefix (available_width - 3) x);
            Fmt.string ppf "...")
          else (
            Fmt.styled `Faint Safe_string.pp ppf x;
            sep ppf ();
            aux (available_width - len - sep_length) xs)
    in
    aux available_width (Index.parent_path index)

  let pp_result ppf result =
    let tag = Tag.of_run_result result in
    left left_tag (Tag.pp ~wrapped:true) ppf tag

  let pp_result_compact : Run_result.t Fmt.t =
   fun ppf result ->
    let colour = result |> Tag.of_run_result |> Tag.colour in
    let char =
      match result with
      | Ok -> '.'
      | Exn _ | Error _ -> 'F'
      | Skip -> 'S'
      | Todo _ -> 'T'
    in
    Fmt.(styled colour char) ppf char

  let pp_result_full ~selector_on_failure ppf (path, result) =
    let with_selector = selector_on_failure && Run_result.is_failure result in
    let available_width = Lazy.force terminal_width - left_total in
    (left_padding ~with_selector) ppf ();
    pp_result ppf result;
    info ~available_width ppf path

  let event_line ppf = function
    | { index; type_ = Result r } ->
        pp_result ppf r;
        info ppf index
    | _ -> assert false

  module Progress_reporter = struct
    type t = {
      ppf : Format.formatter;
      isatty : bool;
      compact : bool;
      selector_on_first_failure : bool;
      mutable prior_failure : bool;
      mutable tests_so_far : int;
    }

    let create ~ppf ~isatty ~compact ~selector_on_first_failure =
      {
        ppf;
        isatty;
        compact;
        selector_on_first_failure;
        prior_failure = false;
        tests_so_far = 0;
      }

    let event t event =
      match (t.compact, t.isatty, event.type_) with
      | true, _, Start | _, false, Start -> ()
      | false, true, Start ->
          left_padding ~with_selector:false t.ppf ();
          (left left_tag (Tag.pp ~wrapped:true)) t.ppf `Todo;
          info
            ~available_width:(Lazy.force terminal_width - left_total)
            t.ppf event.index
      | true, _, Result r ->
          t.tests_so_far <- succ t.tests_so_far;
          pp_result_compact t.ppf r;
          (* Wrap compact output to terminal width manually *)
          if t.tests_so_far mod Lazy.force terminal_width = 0 then
            Format.pp_force_newline t.ppf ();
          ()
      | false, _, Result r ->
          if t.isatty then Fmt.char t.ppf '\r';
          pp_result_full
            ~selector_on_failure:
              (t.selector_on_first_failure && not t.prior_failure)
            t.ppf (event.index, r);
          if Run_result.is_failure r then t.prior_failure <- true;
          Format.pp_force_newline t.ppf ()
  end

  let pp_suite_errors ~show_all = function
    | [] -> Fmt.nop
    | x :: _ as xs -> (if show_all then xs else [ x ]) |> Fmt.concat

  let quoted f = Fmt.(const char '`' ++ f ++ const char '\'')

  let with_surrounding_box (type a) (f : a Fmt.t) : a Fmt.t =
   fun ppf a ->
    (* Peek at the value being pretty-printed to determine the length of the box
       we're going to need. Fortunately, this will not include ANSII colour
       escapes. *)
    let true_width = Fmt.kstr String.length_utf8 "| %a |" f a in
    let min_width = Lazy.force terminal_width in
    let width = max min_width true_width in

    let right_padding = String.v ~len:(width - true_width) (fun _ -> ' ') in
    let s = Fmt.(const (styled `Faint string)) in
    let bars = List.init (width - 2) (fun _ -> "─") |> String.concat in
    let top = s ("┌" ^ bars ^ "┐")
    and mid = Fmt.(s "│ " ++ f ++ s (right_padding ^ " │"))
    and bottom = s ("└" ^ bars ^ "┘") in
    Fmt.(top ++ cut ++ mid ++ cut ++ bottom ++ cut) ppf a

  let horizontal_rule (type a) ppf (_ : a) =
    let open Fmt in
    (const string " "
    ++ const
         (styled `Faint string)
         (List.init (Lazy.force terminal_width - 2) (fun _ -> "─")
         |> String.concat)
    ++ cut)
      ppf ()

  let pp_full_logs ppf log_dir =
    Fmt.pf ppf "Full test results in %t.@,"
      (map_theta ~f:Fmt.(styled `Cyan >> quoted) log_dir)

  let pp_summary ppf r =
    let pp_failures ppf = function
      | 0 -> green_s ppf "Test Successful"
      | n -> red ppf "%d failure%a!" n plural n
    in
    Fmt.pf ppf "%a in %.3fs. %d test%a run.@," pp_failures r.failures r.time
      r.success plural r.success

  let suite_results ~log_dir cfg ppf r =
    let print_summary = (not cfg#compact) || r.failures > 0 in
    match cfg#json with
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
        Format.pp_force_newline ppf ();
        Format.pp_open_vbox ppf 0;
        if cfg#compact then Fmt.cut ppf ();
        (pp_suite_errors ~show_all:(cfg#verbose || cfg#show_errors) r.errors)
          ppf ();
        if print_summary then (
          if not cfg#verbose then pp_full_logs ppf log_dir;
          pp_summary ppf r);
        Format.pp_close_box ppf ()
end
