open Alcotest

let ok () = ()
let todo reason () = failwith ("TODO: " ^ reason)

let () =
  run "incomplete"
    [
      suite "outcomes"
        [
          test "pass" ok;
          test "reason" (todo "write this test");
          test "no reason" (fun () -> failwith "TODO:");
          test "lines" (todo "first\nsecond");
          (* The result of a skipped test is printed without a flush. *)
          test ~tags:(`Tag "off") "skipped" ok;
          test "last" (todo "x");
        ];
    ]
