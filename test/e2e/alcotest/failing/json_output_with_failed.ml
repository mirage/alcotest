let () =
  let open Alcotest in
  let id () = () in
  let always_fail () = assert false in
  run ~argv:[| ""; "--json" |] __FILE__
    [
      ( "test-a",
        [
          test_case "First test case" `Quick id;
          test_case "Second test case always fails" `Quick always_fail;
        ] );
      ("test-b", [ test_case "Third test case" `Quick id ]);
    ]
