(** Reproduction for the issue for which
    https://github.com/mirage/alcotest/pull/215 is an attempted fix. ASSERT
    lines should only be printed if either `--verbose` is set or the tests fail. *)

let () =
  let open Alcotest in
  run "assert-not-printed"
    [
      ( "alpha",
        [
          Alcotest.test_case "tc0" `Quick (fun () ->
              Alcotest.(check unit) "alpha-0 check" () ());
          Alcotest.test_case "tc1" `Quick (fun () ->
              Alcotest.(check unit) "alpha-1 check" () ());
        ] );
      ( "beta",
        [
          Alcotest.test_case "tc0" `Quick (fun () ->
              Alcotest.(check unit) "beta-0 check" () ());
        ] );
    ]
