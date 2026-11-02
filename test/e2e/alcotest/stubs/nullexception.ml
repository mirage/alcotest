external segfault_call : unit -> unit = "caml_segfault_call" 

(* This test should fail *)
let () =
  let open Alcotest in
  let call_seg () = 
    try
      segfault_call ();
      (check pass) "Should get segfault exception" () ()
    with 
    | SegFault _ ->
      fail "Got segfault"
    | _ -> 
      fail "Got uncategorized exception"
  in
  run __FILE__
    [
      ("segfault", [ test_case "nullexcept" `Quick (function _ -> call_seg ())]);
    ]


