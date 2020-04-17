let () =
  Alcotest_unix.run "suite-name"
    [ ("🔥", [ Alcotest.test_case "First test case" `Quick (fun () -> ()) ]) ]
