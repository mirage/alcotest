let () =
  Alcotest.run "suite-name"
    [
      ( "with/separator",
        [ Alcotest.test_case "First test case" `Quick (fun () -> ()) ] );
    ]
