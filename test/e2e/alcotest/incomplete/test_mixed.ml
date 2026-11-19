(* A run with a failure and an incomplete test fails. *)
let () =
  let open Alcotest in
  run ~record_backtrace:false "mixed"
    [
      suite "s"
        [
          test "todo" (fun () -> failwith "TODO: later");
          test "fail" (fun () -> failwith "broken");
        ];
    ]
