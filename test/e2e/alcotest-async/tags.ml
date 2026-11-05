open Async

let ran = ref []

let f name () =
  ran := name :: !ran;
  return ()

let () =
  let open Alcotest_async in
  let timeout = Time_float_unix.Span.of_sec 0.01 in
  let hangs () = Deferred.never () in
  let rename (n, s, f) = ("renamed " ^ n, s, f) in
  let suite =
    [
      ( "tagged",
        [
          test_case "db" (`Tag "db") (f "db");
          test_case "no tags" `Quick (f "no tags");
          test_case ~timeout "hangs" (`Tag "hangs") hangs;
          test_case_sync "sync"
            (`And [ `Slow; `Tag "db" ])
            (fun () -> ran := "sync" :: !ran);
        ] );
      ("rebuilt", List.map rename [ test_case "r" (`Tag "db") (f "r") ]);
    ]
  in
  let run tags =
    run ~and_exit:false ~argv:[| ""; "--tags=" ^ tags |] __FILE__ suite
  in
  Thread_safe.block_on_async_exn @@ fun () ->
  run "db" >>= fun () ->
  Alcotest.(check (list string))
    "selected"
    [ "db"; "no tags"; "sync"; "r" ]
    (List.rev !ran);
  Monitor.try_with (fun () -> run "hangs") >>| function
  | Error _ -> ()
  | Ok () -> Alcotest.fail "the timeout did not apply"
