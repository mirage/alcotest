let start =
  let open Alcotest_mirage in
  let id () = () in
  run "suite-name"
    [
      ( "test-a",
        [
          test_case_sync "First test case" `Quick id;
          test_case_sync "Second test case" `Quick id;
        ] );
      ("test-b", [ test_case_sync "Third test case" `Quick id ]);
      ("test-c", [ test_case_sync "Fourth test case" `Slow id ]);
    ]
