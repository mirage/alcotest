let () =
  let open Alcotest in
  let id () = () in
  Alcotest_unix.run ~argv:[| ""; "--quick" |] "suite-name"
    [
      ("test-a", [ test_case "Quick" `Quick id; test_case "Slow" `Slow id ]);
      ("test-b", [ test_case "Slow" `Slow id; test_case "Quick" `Quick id ]);
    ]
