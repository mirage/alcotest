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

    [Alcotest] provides a simple interface to perform unit tests. It exposes a
    simple {{!TESTABLE} TESTABLE} module type, a {{!check} check function} to
    assert test predicates and a {{!run} run} function to perform a list of
    [unit -> unit] test callbacks.

    From these descriptions, [Alcotest] builds a quiet and colorful output where
    only faulty runs are fully displayed at the end of the run (with the full
    logs ready to inspect).

    {e Release %%VERSION%%} *)

include Cli.S with type return = unit

(** {2 Assert functions} *)

(** [TESTABLE] provides an abstract description for testable values. *)
module type TESTABLE = sig
  (** The type to test. *)
  type t

  val pp : t Fmt.t
  (** A way to pretty-print the value. *)

  val equal : t -> t -> bool
  (** Test for equality between two values. *)
end

(** The type for testable values. *)
type 'a testable = (module TESTABLE with type t = 'a)

val testable : 'a Fmt.t -> ('a -> 'a -> bool) -> 'a testable
(** [testable pp eq] is a new {!testable} with the pretty-printer [pp] and
    equality [eq]. *)

val pp : 'a testable -> 'a Fmt.t
(** [pp t] is [t]'s pretty-printer. *)

val equal : 'a testable -> 'a -> 'a -> bool
(** [equal t] is [t]'s equality. *)

val bool : bool testable
(** [bool] tests booleans. *)

val int : int testable
(** [int] tests integers. *)

val int32 : int32 testable
(** [int32] tests 32-bit integers. *)

val int64 : int64 testable
(** [int64] tests 64-bit integers. *)

val float : float -> float testable
(** [float] tests floats with specified absolute error. *)

val char : char testable
(** [char] tests characters. *)

val string : string testable
(** [string] tests OCaml strings. *)

val unit : unit testable
(** [unit] tests unit values (useful for functions with side-effects). *)

val list : 'a testable -> 'a list testable
(** [list t] tests lists of [t]s. *)

val slist : 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t comp] tests sorted lists of [t]s. The list are sorted using [comp]. *)

val array : 'a testable -> 'a array testable
(** [array t] tests arrays of [t]s. *)

val option : 'a testable -> 'a option testable
(** [option t] tests optional [t]s. *)

val result : 'a testable -> 'e testable -> ('a, 'e) result testable
(** [result t e] tests [t]s on success and [e]s on failure. *)

val pair : 'a testable -> 'b testable -> ('a * 'b) testable
(** [pair a b] tests pairs of [a]s and [b]s. *)

val of_pp : 'a Fmt.t -> 'a testable
(** [of_pp pp] tests values which can be printed using [pp] and compared using
    {!Pervasives.compare} *)

val pass : 'a testable
(** [pass] tests values of any type and always succeeds. *)

val reject : 'a testable
(** [reject] tests values of any type and always fails. *)

val check : 'a testable -> string -> 'a -> 'a -> unit
(** Check that two values are equal. *)

val fail : string -> 'a
(** Simply fail. *)

val failf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Simply fail with a formatted message. *)

val neg : 'a testable -> 'a testable
(** [neg t] is [t]'s negation: it is [true] when [t] is [false] and it is
    [false] when [t] is [true]. *)

val check_raises : string -> exn -> (unit -> unit) -> unit
(** Check that an exception is raised. *)

(** {1 Monadic test runners} *)

(** These modules provide the ability to run tests inside a concurrency monad:
    that is, to sequence test cases of type ['a -> unit m] into a computation of
    type ['a -> unit m] (for some concurrency monad [m]) with can then be
    scheduled in a main event loop. For tests using [Lwt.t] or
    [Async_kernel.Deferred.t], use the [Alcotest_lwt] and [Alcotest_async]
    packages directly. *)

(** Defines monadic test runners {i without} command-line interfaces. *)
module Core : module type of Core

(** Wraps {!Core} to provide a command-line interface. *)
module Cli : module type of Cli

(** Monad signatures for use with {!Core} and {!Cli}. *)
module Monad : module type of Monad
