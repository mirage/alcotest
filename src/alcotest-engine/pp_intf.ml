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

open! Import
open Model

type theta = Format.formatter -> unit
(** Type corresponding to a [%t] placeholder. *)

module Types = struct
  type event_type = Start | Result of Run_result.t
  type event = { index : Index.t; type_ : event_type }

  type result = {
    success : int;
    failures : int;
    time : float;
    errors : unit Fmt.t list;
  }

  type tag = [ `Ok | `Fail | `Skip | `Todo | `Assert ]
end

module type Make_arg = sig
  val stdout_columns : unit -> int option
end

module type S = sig
  include module type of Types

  val info : ?available_width:int -> Index.t Fmt.t
  val rresult_error : Run_result.t Fmt.t
  val event_line : event Fmt.t

  module Progress_reporter : sig
    type t

    val create :
      ppf:Format.formatter ->
      isatty:bool ->
      compact:bool ->
      selector_on_first_failure:bool ->
      t

    val event : t -> event -> unit
  end

  val suite_results :
    log_dir:theta ->
    < verbose : bool ; show_errors : bool ; json : bool ; compact : bool ; .. > ->
    result Fmt.t

  val with_surrounding_box : 'a Fmt.t -> 'a Fmt.t
  (** Wraps a formatter with a Unicode box with width given by
      {!X.stdout_columns}. Uses box-drawing characters from code page 437. *)

  val horizontal_rule : _ Fmt.t
  (** Horizontal rule of length {!X.stdout_columns}. Uses box-drawing characters
      from code page 437. *)
end

module type Pp = sig
  include module type of Types

  val tag : tag Fmt.t

  val map_theta : theta -> f:(unit Fmt.t -> unit Fmt.t) -> theta
  (** Transform a stateless pretty-printer (for use with the [%t] conversion
      spec) at the level of [unit Fmt.t] values. *)

  val quoted : 'a Fmt.t -> 'a Fmt.t
  (** Wraps a formatter with GNU-style `quotation marks'. *)

  val plural : int Fmt.t
  (** This is for adding an 's' to words that should be pluralized, e.g.

      {[
        let n = List.length items in
        Fmt.pr "Found %i item%a." n pp_plural n
      ]} *)

  val user_error : ('a, Format.formatter, unit, _) format4 -> 'a
  (** Raise a user error, then fail. *)

  module Make (X : Make_arg) : S
end
