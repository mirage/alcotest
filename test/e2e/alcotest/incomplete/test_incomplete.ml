open Alcotest

let ok () = ()
let todo reason () = failwith ("TODO: " ^ reason)
let hole = todo "reject negative numbers"

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
      suite "raises"
        [
          test "expected" (fun () ->
              check_raises "hole" (Failure "TODO: reject negative numbers") hole);
          test "check_raises" (fun () ->
              check_raises "hole" (Invalid_argument "negative") hole);
          test "match_raises" (fun () ->
              match_raises "hole" (fun _ -> true) hole);
        ];
    ]
