let () =
  Alcotest.run "unknown_option"
    [ ("alpha", [ Alcotest.test_case "1" `Quick (fun () -> ()) ]) ]
