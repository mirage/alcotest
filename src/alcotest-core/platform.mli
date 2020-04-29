module type S = sig
  type return
  (** The return type of each test case run by Alcotest. For the standard
      {!Alcotest} module, [return = unit]. The concurrent backends
      [Alcotest_lwt] and [Alcotest_async] set [return = unit Lwt.t] and
      [return = Async_kernel.Deferred.t] respectively. *)

  val time : unit -> float
  (** [time ()] returns the current timestamp, used to measure the duration of a
      testrun. *)

  val getcwd : unit -> string
  (** [getcwd ()] returns the current working directory. *)

  val prepare : base:string -> dir:string -> name:string -> unit
  (** [prepare ~base ~dir ~name] is called before test suite execution. [base]
      is the parent of the log directory, [dir] the log directory (including
      unique testrun ID), and [name] is the test name. On Unix, this function
      creates the log directory [dir] for the test output, and sets up the
      symlink [latest] to the latest result. *)

  val with_redirect : string -> (unit -> return) -> return
  (** [with_redirect output_file f] is called for each test. On Unix, it it
      deals with redirection of standard streams to the [output_file]. The
      implementation of [with_redirect] has to make sure to call [f] in order to
      run the test case. *)

  val setup_std_outputs :
    ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
  (** [setup_std_outputs ~style_renderer ~utf_8 ()] is called at startup of
      alcotest and sets up the standard streams for colored output. *)
end

module type MAKER = functor (M : Monad.S) ->
  S with type return = Pp.run_result M.t
