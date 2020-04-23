include module type of Alcotest_core.Test

module Unix : Alcotest_core.Platform.MAKER

include Alcotest_core.Cli.S with type return = unit
