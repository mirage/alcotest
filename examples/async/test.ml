open Async
open Async_unix

let plain_test () = Alcotest.(check int) "same int" 42 42

let async_test () =
  (* Use timeout of 1.0 as by default, Alcotest timeout after 2.0 *)
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 1.0) in
  Alcotest.(check int) "same int" 42 42;
  return ()

let () =
  Async.Thread_safe.block_on_async_exn (fun () ->
      let open Alcotest_async in
      run "Async tests"
        [
          ( "basic",
            [
              test_case "Async" `Quick async_test;
              test_case_sync "Plain" `Quick plain_test;
            ] );
        ])
