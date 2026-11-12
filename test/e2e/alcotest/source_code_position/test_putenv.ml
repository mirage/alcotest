(* A program can turn the source code position off before calling [run]. *)
let () =
  Unix.putenv "ALCOTEST_SOURCE_CODE_POSITION" "false";
  Alcotest.run __FILE__
    [
      ( "putenv",
        [
          Alcotest.test_case "check" `Quick (fun () ->
              Alcotest.(check int) "different" 1 2);
        ] );
    ]
