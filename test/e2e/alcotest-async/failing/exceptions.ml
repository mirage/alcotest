open! Core
open! Async

let test_failed_check () =
  Alcotest.(check int) "1 = 2" 1 2;
  Deferred.return ()

let test_internal_exception () =
  let exception Internal in
  raise Internal

let () =
  let open Alcotest_async in
  ignore
    (run ~verbose:true __FILE__
       [
         ( "all",
           [
             test_case "failed check" `Quick test_failed_check;
             test_case "internal exception" `Quick test_internal_exception;
           ] );
       ]);
  never_returns (Scheduler.go ())
