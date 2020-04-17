let () =
  Alcotest_unix.run "unknown_option"
    [ ("alpha", [ Alcotest.test_case "1" `Quick (fun () -> ()) ]) ]
