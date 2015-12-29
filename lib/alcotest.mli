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

(** A lightweight and colourful test framework. *)

type speed_level = [`Quick | `Slow]
(** Speed level for a test. *)

type test_case = string * speed_level * (unit -> unit)
(** A test case is an UTF-8 encoded documentation string, a speed
    level and a function to execute. *)

type test = string * test_case list
(** A test is an US-ASCII encoded name and a list of test cases. *)

exception Test_error
(** The exception return by {!run} in case of errors. *)

val run: ?and_exit:bool -> string -> test list -> unit
(** [run n t] runs the test suite [t]. [n] is is the name of the
    tested library. The optional argument [and_exit] controls what
    happens when the function ends. By default, [and_exit] is set,
    which makes the function exit with [0] if everything is fine or
    [1] if there is an issue. If [and_exit] then the function raises
    [Test_error] on error. *)

(** {2 Display} *)

val line: out_channel -> ?color:[`Blue|`Yellow] -> char -> unit
(** Draw a line on the given channel *)

(** {2 Assert functions} *)

module type TESTABLE = sig

  type t
  (** The type to test. *)

  val pp: Format.formatter -> t -> unit
  (** A way to pretty-print the value. *)

  val equal: t -> t -> bool
  (** Test for equality between two values. *)

end

type 'a testable = (module TESTABLE with type t = 'a)
(** The type for testable values. *)

val bool: bool testable
(** [bool] tests booleans. *)

val int: int testable
(** [int] tests integers. *)

val char: char testable
(** [char] tests characters. *)

val string: string testable
(** [string] tests OCaml strings. *)

val list: 'a testable -> 'a list testable
(** [list t] tests lists of [t]s. *)

val slist: 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t comp] tests sorted lists of [t]s. The list are sorted
    using [comp]. *)

val option: 'a testable -> 'a option testable
(** [option t] tests optional [t]s. *)

val result : 'a testable -> 'e testable -> ('a, 'e) Result.result testable
(** [result t e] tests [t]s on success and [e]s on failure. *)

val pair: 'a testable -> 'b testable -> ('a * 'b) testable
(** [pair a b] tests pairs of [a]s and [b]s. *)

val of_pp: (Format.formatter -> 'a -> unit) -> 'a testable
(** [of_pp pp] tests values which can be printed using [pp] and
    compared using {!Pervasives.compare} *)

val check: 'a testable -> string -> 'a -> 'a -> unit
(** Check that two values are equal. *)

val fail: string -> 'a
(** Simply fail. *)

val check_raises: string -> exn -> (unit -> unit) -> unit
(** Check that an exception is raised. *)
