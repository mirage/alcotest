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

(** A lightweight and colourful test framework.

    [Alcotest] provides a simple interface to perform unit tests. It
    exposes a simple {{!TESTABLE}TESTABLE} module type, a
    {{!check}check function} to assert test predicates and a
    {{!run}run} function to perform a list of [unit -> unit] test
    callbacks.

    From these descriptions, [Alcotest] builds a quiet and colorful
    output where only faulty runs are fully displayed at the end of
    the run (with the full logs ready to inspect), with a simple (yet
    expressive) query language to select the tests to run.

    {e Release %%VERSION%% } *)

type speed_level = [`Quick | `Slow]
(** Speed level for a test. *)

type 'a test_case = string * speed_level * ('a -> unit)
(** A test case is an UTF-8 encoded documentation string, a speed
    level and a function to execute. *)

val test_case: string -> speed_level -> ('a -> unit) -> 'a test_case
(** [test_case n s f] is the test case [n] running at speed [s] using
    the function [f]. *)

type 'a test = string * 'a test_case list
(** A test is an US-ASCII encoded name and a list of test cases. *)

exception Test_error
(** The exception return by {!run} in case of errors. *)

val run: ?and_exit:bool -> ?argv:string array ->
  string -> unit test list -> unit
(** [run n t] runs the test suite [t]. [n] is the name of the
    tested library.

    The optional argument [and_exit] controls what happens when the
    function ends. By default, [and_exit] is set, which makes the
    function exit with [0] if everything is fine or [1] if there is an
    issue. If [and_exit] is [false], then the function raises
    [Test_error] on error.

    The optional argument [argv] specifies the argument sent to
    alcotest like ["--json"], ["--verbose"], etc. (at least one
    argument is required).*)

val run_with_args: ?and_exit:bool -> ?argv:string array ->
  string -> 'a Cmdliner.Term.t -> 'a test list -> unit
(** [run_with_args n a t] Similar to [run a t] but take an extra
    argument [a]. Every test function will receive as arguement the
    evaluation of the [Cdmliner] term [a]: this is useful to configure
    the test behaviors using the CLI. *)

(** {1 Assert functions} *)

(** [TESTABLE] provides an abstract description for testable
    values. *)
module type TESTABLE = sig

  type t
  (** The type to test. *)

  val pp: t Fmt.t
  (** A way to pretty-print the value. *)

  val equal: t -> t -> bool
  (** Test for equality between two values. *)

end

type 'a testable = (module TESTABLE with type t = 'a)
(** The type for testable values. *)

val testable : 'a Fmt.t -> ('a -> 'a -> bool) -> 'a testable
(** [testable pp eq] is a new {!testable} with the pretty-printer [pp] and
    equality [eq]. *)

val pp : 'a testable -> 'a Fmt.t
(** [pp t] is [t]'s pretty-printer. *)

val equal : 'a testable -> 'a -> 'a -> bool
(** [equal t] is [t]'s equality. *)

val bool: bool testable
(** [bool] tests booleans. *)

val int: int testable
(** [int] tests integers. *)

val int32: int32 testable
(** [int32] tests 32-bit integers. *)

val int64: int64 testable
(** [int64] tests 64-bit integers. *)

val float: float -> float testable
(** [float] tests floats with specified absolute error. *)

val char: char testable
(** [char] tests characters. *)

val string: string testable
(** [string] tests OCaml strings. *)

val unit: unit testable
(** [unit] tests unit values (useful for functions with side-effects). *)

val list: 'a testable -> 'a list testable
(** [list t] tests lists of [t]s. *)

val slist: 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t comp] tests sorted lists of [t]s. The list are sorted
    using [comp]. *)

val array : 'a testable -> 'a array testable
(** [array t] tests arrays of [t]s. *)

val option: 'a testable -> 'a option testable
(** [option t] tests optional [t]s. *)

val result : 'a testable -> 'e testable -> ('a, 'e) Result.result testable
(** [result t e] tests [t]s on success and [e]s on failure. *)

val pair: 'a testable -> 'b testable -> ('a * 'b) testable
(** [pair a b] tests pairs of [a]s and [b]s. *)

val of_pp: 'a Fmt.t -> 'a testable
(** [of_pp pp] tests values which can be printed using [pp] and
    compared using {!Pervasives.compare} *)

val pass: 'a testable
(** [pass] tests values of any type and always succeeds. *)

val reject: 'a testable
(** [reject] tests values of any type and always fails. *)

val check: 'a testable -> string -> 'a -> 'a -> unit
(** Check that two values are equal. *)

val fail: string -> 'a
(** Simply fail. *)

val failf: ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Simply fail with a formatted message. *)

val neg: 'a testable -> 'a testable
(** [neg t] is [t]'s negation: it is [true] when [t] is [false] and it
    is [false] when [t] is [true]. *)

val check_raises: string -> exn -> (unit -> unit) -> unit
(** Check that an exception is raised. *)

(** {1 Deprecated} *)

val line: out_channel -> ?color:[`Blue|`Yellow] -> char -> unit
(** @deprecated
    You should write your own line function. For instance:
{[
let line ppf ?color c =
  let line = String.v ~len:terminal_columns (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string)  line
  | None   -> Fmt.pf ppf "%s\n%!"line
]}
*)
