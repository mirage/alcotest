let passing () =
  print_endline "MARK-stdout-passing";
  prerr_endline "MARK-stderr-passing";
  Alcotest.(check int) "same" 1 1

let failing () =
  print_endline "MARK-stdout-failing";
  prerr_endline "MARK-stderr-failing";
  Alcotest.(check int) "different" 1 2

let () =
  Alcotest.run "downstream"
    [
      ( "capture",
        [
          Alcotest.test_case "passing" `Quick passing;
          Alcotest.test_case "failing" `Quick failing;
        ] );
    ]
