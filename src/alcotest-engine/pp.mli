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

val terminal_width : unit -> int

type path = [ `Path of string * int ]

val info : max_label:int -> doc_of_path:(path -> string) -> path Fmt.t

val tag : [ `Ok | `Fail | `Error | `Skip | `Todo | `Assert ] Fmt.t

type run_result =
  [ `Ok
  | `Exn of path * string * string
  | `Error of path * unit Fmt.t
  | `Skip
  | `Todo of string ]

val is_failure : run_result -> bool
(** [is_failure] holds for test results that are error states. *)

type event = [ `Result of path * run_result | `Start of path ]

type result = {
  success : int;
  failures : int;
  time : float;
  errors : unit Fmt.t list;
}

val rresult_error : run_result Fmt.t

val event_line : max_label:int -> doc_of_path:(path -> string) -> event Fmt.t

val event :
  compact:bool ->
  max_label:int ->
  doc_of_path:(path -> string) ->
  selector_on_failure:bool ->
  tests_so_far:int ->
  event Fmt.t

val suite_results :
  verbose:bool ->
  show_errors:bool ->
  json:bool ->
  compact:bool ->
  log_dir:string ->
  result Fmt.t

val quoted : 'a Fmt.t -> 'a Fmt.t
(** Wraps a formatter with `GNU-style quotation marks'. *)

val unicode_boxed : 'a Fmt.t -> 'a Fmt.t
(** Wraps a formatter with a Unicode box with width given by {!terminal_width}.
    This uses characters from code page 437, so should be fairly portable. *)

val pp_plural : int Fmt.t
(** This is for adding an 's' to words that should be pluralized, e.g.

    {[
      let n = List.length items in
      Fmt.pr "Found %i item%a." n pp_plural n
    ]} *)
