let () =
  Alcotest.run "suite-name"
    [ ("🔥", [ Alcotest.test_case "First test case" `Quick (fun () -> ()) ]) ]
