let () =
  let open Alcotest in
  let id () = () in
  let doc =
    "A description long enough to be truncated at the end of the line"
  in
  run __FILE__
    [
      ( "new-api",
        [
          test_case "no tags" `Quick id;
          test_case "everything" `Always id;
          test_case "either" (`Or [ `Tag "net"; `Tag "crypto" ]) id;
          test_case "not ci" (`Not (`Tag "ci")) id;
          test_case "nested" (`And [ `Slow; `Or [ `Tag "a"; `Tag "b" ] ]) id;
          test_case "slow" `Slow id;
          test_case "slow db x" (`And [ `Slow; `Tag "db"; `Tag "x" ]) id;
          test_case doc (`Tag "db") id;
        ] );
    ]
