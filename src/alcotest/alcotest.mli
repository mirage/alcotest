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
    simple {!TESTABLE} module type, a {{!check} check function} to assert test
    predicates and a {{!run} run} function to perform a list of [unit -> unit]
    test callbacks.

    {e Release %%VERSION%%} *)

include Alcotest_engine.V1.Cli.S with type return := unit
(** @inline *)

(** {1 Testable values}

    The following combinators represent types that can be used with the {!check}
    functions below. *)

include module type of Alcotest_engine.V1.Test
(** @inline *)

(** {1 Versioned APIs} *)

(** An alias of the above API that provides a stability guarantees over major
    version changes. *)
module V1 : sig
  include Alcotest_engine.V1.Cli.S with type return = unit

  include module type of Alcotest_engine.V1.Test
  (** @inline *)
end

module Unstable : sig
  open Alcotest_engine.Unstable

  (** The latest, cutting-edge API of Alcotest.

      {b Note:} this API is still under active development, and provides no
      stability guarantee.

      - {{!defining} defining} a test suite;
      - {{!implementing} asserting} test properties;
      - {{!running} running} the suite.

      From these descriptions, [Alcotest] builds a quiet and colorful output
      where only faulty runs are fully displayed at the end of the run (with the
      full logs ready to inspect).

      {[
        let test_foo () =
          (* ... make test assertions ... *)

        let test_bar () = (* ... *)

        let () =
          Alcotest.run ~name:__FILE__ [
            Alcotest.test ~name:"foo" test_foo;
            Alcotest.test ~name:"bar" test_bar
          ]
      ]} *)

  (** {1:defining Defining tests} *)

  type 'a test
  (** An {i ['a] test} is a program that, when executed with arguments of type
      ['a], may either pass or fail with some exception. *)

  (** A {i tag} is an attribute that is attached to a test. *)
  module Tag : sig
    include module type of Tag with type t = Tag.t
    (** @inline *)
  end

  module Source_code_position : sig
    type t = string * int * int * int
    (** Location information passed via a [~pos] argument, intended for use with
        the [__POS__] macro provided by the standard library. See the
        documentation of [__POS__] for more information. *)
  end

  (** @inline *)
  include
    Core.Test_constructors
      with type 'a test := 'a test
       and type 'a m := 'a
       and type 'a test_args := 'a
       and type source_code_position := Source_code_position.t
       and type tag_set := Tag.Set.t

  (** {1:implementing Implementing tests} *)

  include module type of Alcotest_engine.Unstable.Test
  (** @inline *)

  (** {1:running Running tests} *)

  module Config : sig
    type t

    val v :
      ?and_exit:bool ->
      ?verbose:bool ->
      ?compact:bool ->
      ?tail_errors:[ `Unlimited | `Limit of int ] ->
      ?quick_only:bool ->
      ?show_errors:bool ->
      ?json:bool ->
      ?filter:Filter.t ->
      ?bail:bool ->
      ?log_dir:string ->
      unit ->
      t

    (** The various options taken by the tests runners {!run} and
        {!run_with_args}:

        - [and_exit] (default [true]). Once the tests have completed, exit with
          return code [0] if all tests passed, otherwise [1].
        - [verbose] (default [false]). Display the test std.out and std.err
          (rather than redirecting to a log file).
        - [compact] (default [false]). Compact the output of the tests.
        - [tail_errors] (default unlimited). Show only the last N lines of
          output of failed tests.
        - [quick_only] (default [false]). Don't run tests with the
          {{!Core.speed_level} [`Slow] speed level}.
        - [show_errors] (default [false]). Display the test errors.
        - [json] (default [false]). Print test results in a JSON-compatible
          format.
        - [log_dir] (default ["$PWD/_build/_tests/"]). The directory in which to
          log the output of the tests (if [verbose] is not set). *)

    val merge : t -> t -> t
  end

  (** @inline *)
  include
    Core.Test_runners
      with type 'a test := 'a test
       and type 'a m := 'a
       and type source_code_position := Source_code_position.t
       and type config := Config.t
end

(** {1 Unix-specific engine constructors}

    The [Alcotest_engine] package provides the most general form of the Alcotest
    API, parameterised over the thread implementation and the platform. This
    package provides the [Unix] platform implementation. *)

module Unix_platform : Alcotest_engine.Platform.MAKER
