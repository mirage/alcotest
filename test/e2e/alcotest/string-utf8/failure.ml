let () =
  let open Alcotest in
  run __FILE__
    [
      ( "failure",
        [
          test_case "symbols" `Quick (fun () -> check string "symbols" "∀" "∃");
        ] );
    ]
