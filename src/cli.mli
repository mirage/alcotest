(** This module provides a wrapper around {!Core} that produces an executable with
    Cmdliner. *)

module type S = sig
  include Core.S

  val run :
    (?argv:string array -> string -> unit test list -> return) with_options
  (** [run n t] runs the test suite [t]. [n] is the name of the
      tested library.

      The optional argument [and_exit] controls what happens when the
      function ends. By default, [and_exit] is set, which makes the
      function exit with [0] if everything is fine or [1] if there is an
      issue. If [and_exit] is [false], then the function raises
      [Test_error] on error.

      The optional argument [argv] specifies command line arguments sent to
      alcotest like ["--json"], ["--verbose"], etc. Note that this array will be
      treated like a regular [Sys.argv], so the array must have at least one
      element, and the first element will be treated as if it was the command name
      and thus ignored for the purposes of option processing. So [~argv:[||]] is
      an error, [~argv:[| "--verbose" |]] will have no effect, and [~argv:[|
      "ignored"; "--verbose" |]] will successfully pass the verbose option. *)

  val run_with_args :
    (?argv:string array ->
    string ->
    'a Cmdliner.Term.t ->
    'a test list ->
    return)
    with_options
  (** [run_with_args n a t] Similar to [run a t] but take an extra
      argument [a]. Every test function will receive as arguement the
      evaluation of the [Cdmliner] term [a]: this is useful to configure
      the test behaviors using the CLI. *)
end

module Make (M : Monad.S) : S with type return = unit M.t

include S with type return = unit
