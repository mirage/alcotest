let () =
  Alcotest.run "suite-name"
    [
      ( "🔥",
        [
          Alcotest.test_case "Non ASCII unicode character" `Quick (fun () -> ());
        ] );
      ( "🔥a-b",
        [
          Alcotest.test_case "Non ASCII and ASCII characters" `Quick (fun () ->
              ());
        ] );
    ]
