(** Regression test for an issue in which stderr was not captured into the test
    logs due to Format buffers not being flushed. See
    https://github.com/mirage/alcotest/pull/228 for details. *)

let () =
  let s tc = tc ^ ": SHOULD NOT BE PRINTED" in
  Alcotest_unix.run "assert-not-printed"
    [
      ( "alpha",
        [
          Alcotest.test_case "0" `Quick (fun () ->
              Alcotest.(check unit) (s "0") () ());
          Alcotest.test_case "1" `Quick (fun () ->
              Format.eprintf "%s" (s "1");
              Alcotest.(check unit) (s "1") () ());
        ] );
      ( "beta",
        [
          Alcotest.test_case "2" `Quick (fun () ->
              Alcotest.(check unit) "2" () ();
              Format.eprintf "%s" (s "2"));
        ] );
    ]
