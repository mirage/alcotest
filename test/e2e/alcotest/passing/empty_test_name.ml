let () =
  Alcotest.run "<suite-name>"
    [ ("", [ Alcotest.test_case "1" `Quick (fun () -> ()) ]) ]
