exception Foo of string

let () =
  let open Alcotest in
  Alcotest_unix.run "suite-with-failures"
    [
      ( "test-a",
        [
          test_case "Passing" `Quick (fun () -> ());
          test_case "Failing" `Quick (fun () -> raise (Foo "message"));
        ] );
      ("test-b", [ test_case "Another pass" `Quick (fun () -> ()) ]);
    ]
