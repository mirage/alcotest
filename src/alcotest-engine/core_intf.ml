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
        [return = unit Async.Deferred.t] respectively. *)

    type tag =
      [ `Quick
      | `Slow
      | `Always
      | `Tag of string
      | `And of tag list
      | `Or of tag list
      | `Not of tag ]
    (** When a test runs. The command line sets up a context, a set of tags that
        starts with [quick] and [slow]: [-q] removes [slow], and [--tags] adds
        or removes tags. [`Quick], [`Slow] and [`Tag s] hold when [quick],
        [slow] and [s] are in the context, [`And], [`Or] and [`Not] combine
        them, and [`Always] always holds. *)

    type speed_level = tag
    (** Deprecated alias of {!tag}. *)

    (** Combinators for {!tag}, as in [Tag.(slow && v "release")]. *)
    module Tag : sig
      val v : string -> tag
      (** [v s] is [`Tag s]. *)

      val quick : tag
      val slow : tag
      val always : tag

      val ( && ) : tag -> tag -> tag
      (** [a && b] is [`And [ a; b ]]. *)

      val ( || ) : tag -> tag -> tag
      (** [a || b] is [`Or [ a; b ]]. *)

      val not : tag -> tag
      (** [not a] is [`Not a]. *)
    end

    type 'a test_case = string * tag * ('a -> return)
    (** A test case is a UTF-8 encoded documentation string, when it runs and a
        function to execute. Typically, the testing function calls the helper
        functions provided below (such as [check] and [fail]). *)

    exception Test_error
    (** The exception return by {!run} in case of errors. *)

    val test_case : string -> tag -> ('a -> return) -> 'a test_case
    (** [test_case n t f] is the test case [n] using the function [f], which
        runs when [t] holds. *)

    val test : ?tags:tag -> string -> ('a -> return) -> 'a test_case
    (** [test name f] is the test case [name] using the function [f]. It only
        runs when [tags] hold, which defaults to [`Always]. *)

    val group : ?tags:tag -> string -> 'a test_case list -> 'a test_case
    (** [group name cases] is a test case that stands for [cases], which run in
        place under the test name [test.name], so that they can be selected
        together. Groups nest. [name] must not be empty. [cases] only run when
        [tags] hold as well as their own, and [tags] defaults to [`Always].

        A group is recognised by physical equality on the returned tuple, so it
        can appear several times and under different parents. A new tuple built
        from it is a plain test case, whose function runs all of [cases].
        Functions on a list of test cases, like [List.filter], see the group and
        not its [cases].

        The [test] subcommand matches names as a regular expression, so
        [test top] also selects [top.name]. Use [test '^top$'] to select only
        the cases outside of groups. A test called [top.name] next to a group
        [name] in [top] is a duplicate test path if their indices overlap. *)

    type 'a test = string * 'a test_case list
    (** A test is a UTF-8 encoded name and a list of test cases. The name can be
        used for filtering which tests to run on the CLI. *)

    val suite : string -> 'a test_case list -> 'a test
    (** [suite name cases] is the test [name] with the test cases [cases]. *)

    type 'a with_options =
      ?stdout:Formatters.stdout ->
      ?stderr:Formatters.stderr ->
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
      ?ci:Config.ci ->
      'a
    (** The various options taken by the tests runners {!run} and
        {!run_with_args}:

        - [stdout] (default to [Fmt.stdout]). The formatter used to print on the
          standard output.
        - [stderr] (default to [Fmt.stderr]). The formatter used to print ont
          the standard error output.
        - [and_exit] (default [true]). Once the tests have completed, exit with
          return code [0] if all tests passed, otherwise [1].
        - [verbose] (default [false]). Display the test std.out and std.err
          (rather than redirecting to a log file).
        - [compact] (default [false]). Compact the output of the tests.
        - [tail_errors] (default unlimited). Show only the last N lines of
          output of failed tests.
        - [quick_only] (default [false]). Do not run the [`Slow] tests, unless
          [--tags] selects them.
        - [show_errors] (default [false], or [true] when a CI is detected).
          Display the test errors.
        - [json] (default [false]). Print test results in a JSON-compatible
          format.
        - [filter]. Filter tests according to [~name], the name of the test, and
          [~index], the number of the test case.
        - [log_dir] (default ["$PWD/_build/_tests/"], or
          ["$DUNE_SOURCEROOT/_build/_tests/"] under dune). The directory in
          which to log the output of the tests (if [verbose] is not set).
        - [bail] (default [false]). If true, stop running the tests after the
          first failure.
        - [record_backtrace] (default [true]). Enable backtrace recording before
          beginning testing.
        - [ci] (default auto-detected). Whether to enable specific logging for a
          CI system. *)

    val run : (string -> unit test list -> return) with_options
    val run_with_args : (string -> 'a -> 'a test list -> return) with_options
  end

  module type MAKER = functor (_ : Platform.MAKER) (M : Monad.S) -> sig
    include S with type return = unit M.t

    val list_tests : 'a test list -> return
    (** Print all of the test cases in a human-readable form *)

    val list_tags : 'a test list -> return
    (** Print [quick], [slow] and the tags used by the conditions of test cases,
        one per line. *)

    val run' : Config.User.t -> string -> unit test list -> return
    (** Variant of {!run} that consumes a config value. *)

    val run_with_args' : Config.User.t -> string -> 'a -> 'a test list -> return
    (** Variant of {!run_with_args} that consumes a config value. *)
  end
end

module type Core = sig
  exception Check_error of unit Fmt.t

  module V1 : sig
    module type S = V1_types.S
    module type MAKER = V1_types.MAKER

    module Make : MAKER
    (** Functor for building a tester that sequences tests of type
        [('a -> unit M.t)] within a given concurrency monad [M.t]. The [run] and
        [run_with_args] functions must be scheduled in a global event loop.
        Backends such as [Alcotest_lwt] and [Alcotest_async] use it through
        [Cli.V1.Make], which adds the command line. *)

    exception Skip
  end
end
