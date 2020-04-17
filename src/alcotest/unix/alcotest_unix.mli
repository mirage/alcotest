module Unix : Alcotest.Platform.MAKER

include Alcotest.Cli.S with type return = unit
