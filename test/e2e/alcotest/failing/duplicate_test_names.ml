(** Ensure that suites with duplicate test names are rejected. *)

let () =
  Alcotest_unix.run "suite-with-duplicate-names"
    [
      ("duped", [ Alcotest.test_case "1" `Quick (fun () -> assert false) ]);
      ("duped", [ Alcotest.test_case "2" `Quick (fun () -> assert false) ]);
    ]
