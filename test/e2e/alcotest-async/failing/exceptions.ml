open Async

let passing () = return ()
let failed_check () = return (Alcotest.(check int) "1 = 2" 1 2)
let skipped () = return (Alcotest.skip ())
let user_exception () = raise Exit

let () =
  let open Alcotest_async in
  don't_wait_for
    (run ~argv:[| ""; "--json" |] __FILE__
       [
         ( "all",
           [
             test_case "passing" `Quick passing;
             test_case "failed check" `Quick failed_check;
             test_case "skipped" `Quick skipped;
             test_case "user exception" `Quick user_exception;
           ] );
       ]);
  Core.never_returns (Scheduler.go ())
