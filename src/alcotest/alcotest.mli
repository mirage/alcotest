include module type of Alcotest_core.Test
(** @inline *)

module Unix : Alcotest_core.Platform.MAKER

include Alcotest_core.Cli.S with type return = unit
(** @inline *)
