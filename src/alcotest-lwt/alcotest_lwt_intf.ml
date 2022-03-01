module type V1 = sig
  include Alcotest_engine.V1.Cli.S with type return = unit Lwt.t

  val test_case :
    string ->
    Alcotest.speed_level ->
    (Lwt_switch.t -> 'a -> unit Lwt.t) ->
    'a test_case

  val test_case_sync :
    string -> Alcotest.speed_level -> ('a -> unit) -> 'a test_case
end

module type Alcotest_lwt = sig
  include V1

  (** {1 Versioned APIs} *)

  module V1 : V1
  (** An alias of the above API that provides a stability guarantees over major
      version changes. *)

  module Unstable : sig
    open Alcotest_engine.Unstable

    include
      Core.S
        with type 'a m := 'a Lwt.t
         and type 'a test_args := Lwt_switch.t -> 'a
         and type config := Config.User.t
         and type source_code_position := Source_code_position.pos
         and type tag_set := Tag.Set.t

    val test_sync : (('a -> unit) -> 'a test) Core.identified
  end
end
