open Alcotest

let () =
  run "crash"
    [ suite "t" [ test "done" ignore; test "stops" (fun () -> exit 3) ] ]
