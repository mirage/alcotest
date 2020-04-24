(** {1 Assert functions} *)

module Test : module type of Test

(** {1 Monadic test runners} *)

(** These modules provide the ability to run tests inside a concurrency monad:
    that is, to sequence test cases of type ['a -> unit m] into a computation of
    type ['a -> unit m] (for some concurrency monad [m]) with can then be
    scheduled in a main event loop. For tests using [Lwt.t] or
    [Async_kernel.Deferred.t], use the [Alcotest_lwt] and [Alcotest_async]
    packages directly. *)

module Core : module type of Core
(** Defines monadic test runners {i without} command-line interfaces. *)

module Cli : module type of Cli
(** Wraps {!Core} to provide a command-line interface. *)

module Monad : module type of Monad
(** Monad signatures for use with {!Core} and {!Cli}. *)

module Platform : module type of Platform
(** Defines platform-dependent functions. *)

module Fmt : module type of Utils.Fmt
(** Extends the Fmt library for backwards compatibility. *)

module Pp : module type of Pp
(** Defines pretty-printing utilities. *)
