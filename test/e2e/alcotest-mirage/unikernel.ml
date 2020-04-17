let start =
  let open Alcotest_mirage in
  let id () = () in
  run "suite-name"
    [
      ("test", [ test_case_sync "A test case for alcotest with mirage" `Quick id ]);
    ]
