(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
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

module V1_types = struct
  module type S = sig
    type return
    (** The return type of each test case run by Alcotest. For the standard
        {!Alcotest} module, [return = unit]. The concurrent backends
        [Alcotest_lwt] and [Alcotest_async] set [return = unit Lwt.t] and
        [return = Async_kernel.Deferred.t] respectively. *)

    type speed_level = [ `Quick | `Slow ]
    (** Speed level of a test. Tests marked as [`Quick] are always run. Tests
        marked as [`Slow] are skipped when the `-q` flag is passed. *)

    type 'a test_case = string * speed_level * ('a -> return)
    (** A test case is a UTF-8 encoded documentation string, a speed level and a
        function to execute. Typically, the testing function calls the helper
        functions provided below (such as [check] and [fail]). *)

    exception Test_error
    (** The exception return by {!run} in case of errors. *)

    val test_case : string -> speed_level -> ('a -> return) -> 'a test_case
    (** [test_case n s f] is the test case [n] running at speed [s] using the
        function [f]. *)

    type 'a test = string * 'a test_case list
    (** A test is a UTF-8 encoded name and a list of test cases. The name can be
        used for filtering which tests to run on the CLI. *)

    type 'a with_options =
      ?and_exit:bool ->
      ?verbose:bool ->
      ?compact:bool ->
      ?tail_errors:[ `Unlimited | `Limit of int ] ->
      ?quick_only:bool ->
      ?show_errors:bool ->
      ?json:bool ->
      ?filter:(name:string -> index:int -> [ `Run | `Skip ]) ->
      ?log_dir:string ->
      ?bail:bool ->
      ?record_backtrace:bool ->
      'a
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
          {{!speed_level} [`Slow] speed level}.
        - [show_errors] (default [false]). Display the test errors.
        - [json] (default [false]). Print test results in a JSON-compatible
          format.
        - [filter]. Filter tests according to [~name], the name of the test, and
          [~index], the number of the test case.
        - [log_dir] (default ["$PWD/_build/_tests/"]). The directory in which to
          log the output of the tests (if [verbose] is not set).
        - [bail] (default [false]). If true, stop running the tests after the
          first failure.
        - [record_backtrace] (default [true]). Enable backtrace recording before
          beginning testing. *)

    val run : (string -> unit test list -> return) with_options
    val run_with_args : (string -> 'a -> 'a test list -> return) with_options
  end

  module type MAKER = functor (P : Platform.MAKER) (M : Monad.S) -> sig
    include S with type return = unit M.t

    val list_tests : string -> 'a test list -> return
    (** Print all of the test cases in a human-readable form *)

    val run' : Config.User.t -> string -> unit test list -> return
    (** Variant of {!run} that consumes a config value. *)

    val run_with_args' : Config.User.t -> string -> 'a -> 'a test list -> return
    (** Variant of {!run_with_args} that consumes a config value. *)
  end
end

module Types = struct
  exception Check_error of unit Fmt.t

  (* TODO(4.08): replace with local type substitution *)
  type 'a identified =
    ?pos:Source_code_position.pos -> ?tags:Tag.Set.t -> name:string -> 'a
  (** A test suite is a tree of named test cases, with named internal nodes.
      This type defines the metadata associated with each node in the tree: *)
end

module Unstable_types = struct
  module type Test_constructors = sig
    (* These types are intended to be destructively-substituted by various
       backends. *)

    type 'a m
    type 'a test_args
    type tag_set
    type source_code_position

    type 'a test
    (** The type of unit tests. *)

    val test :
      ?pos:source_code_position ->
      ?tags:tag_set ->
      name:string ->
      ('a -> unit m) test_args ->
      'a test
    (** [test ~name f] is a named test that runs the function [f]. If [f] makes
        a failing assertion (e.g. via [Alcotest.check]) or raises an exception,
        then the test case will fail. Otherwise, the test case passes. *)

    val group :
      ?pos:source_code_position ->
      ?tags:tag_set ->
      name:string ->
      'a test list ->
      'a test
    (** [group ~name tests] is a group of tests with the given name.*)
  end

  module type Test_runners = sig
    type 'a m
    type 'a test
    type config
    type source_code_position

    val run :
      ?pos:source_code_position ->
      ?config:config ->
      ?name:string ->
      ?__FILE__:string ->
      unit test list ->
      unit m

    val run_with_args :
      ?pos:source_code_position ->
      ?config:config ->
      ?name:string ->
      ?__FILE__:string ->
      'a ->
      'a test list ->
      unit m

    (** {2 Log capturing}

        Whenever a test is executed, its standard output / error streams are
        redirected to a log file on disk. If the test fails, this log file is
        printed to the *)

    (** {2 Command-line interface}

        The test suite runners can be configured at run-time via an
        auto-generated command line interface, in addition to the [~config]
        argument.

        Run the binary with the [--help] option to see the full list of
        supported command-line options (e.g. using
        [dune exec ./path/to/text.exe -- --help]). *)
  end

  (** Extensions to {!S} for use by backends. *)
  module type S = sig
    include Test_constructors

    include
      Test_runners
        with type 'a m := 'a m
         and type 'a test := 'a test
         and type source_code_position := source_code_position

    val list_tests :
      ?pos:source_code_position ->
      ?config:config ->
      name:string option ->
      _ test list ->
      unit m
  end

  module type MAKER = functor (P : Platform.MAKER) (M : Monad.S) ->
    S
      with type 'a m := 'a M.t
       and type 'a test_args := 'a
       and type config := Config.User.t
       and type source_code_position := Source_code_position.pos
       and type tag_set := Tag.Set.t
end

module type Core = sig
  include module type of Types

  module V1 : sig
    module type S = V1_types.S
    module type MAKER = V1_types.MAKER

    module Make : MAKER
    (** Functor for building a tester that sequences tests of type
        [('a -> unit M.t)] within a given concurrency monad [M.t]. The [run] and
        [run_with_args] functions must be scheduled in a global event loop.
        Intended for use by the {!Alcotest_lwt} and {!Alcotest_async} backends. *)
  end

  module Unstable : sig
    type nonrec 'a identified = 'a identified

    include module type of Unstable_types
    module Make : MAKER
  end
end
