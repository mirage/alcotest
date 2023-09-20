module type V1 = sig
  include Alcotest_engine.V1.Cli.S with type return = unit Lwt.t

  type 'a case = ?speed:speed_level -> string -> (Lwt_switch.t -> 'a -> unit Lwt.t) -> unit
  type 'a group = string -> ('a case -> unit) -> unit

  val test_case :
    string ->
    Alcotest.speed_level ->
    (Lwt_switch.t -> 'a -> unit Lwt.t) ->
    'a test_case

  val test_case_sync :
    string -> Alcotest.speed_level -> ('a -> unit) -> 'a test_case

  val suite_testlist : ('a group -> unit) -> 'a test list

  val suite :
    (?argv:string array -> string -> (unit group -> unit) -> unit Lwt.t) with_options

  val suite_with_args :
    (?argv:string array -> string -> 'a Cmdliner.Term.t -> ('a group -> unit) -> unit Lwt.t) with_options
end

module type Alcotest_lwt = sig
  include V1

  (** {1 Versioned APIs} *)

  module V1 : V1
  (** An alias of the above API that provides a stability guarantees over major
      version changes. *)
end
