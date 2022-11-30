let () =
  let open Alcotest_lwt in
  Alcotest_test_helper.wakeup_until_resolved
  @@ run "Results should be in JSON, since --json is passed"
       [ ("test", [ test_case "alpha" `Quick (fun _ () -> Lwt.return_unit) ]) ]
