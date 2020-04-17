let () =
  let open Alcotest in
  let passing s = test_case s `Quick (fun () -> ()) in
  let failing s = test_case s `Quick (fun () -> failwith "Error") in
  Alcotest_unix.run ~compact:true "failing compact output"
    [
      ("alpha", [ passing "1"; passing "2"; failing "3"; passing "4" ]);
      ("beta", [ passing "1" ]);
      ("gamma", [ passing "1" ]);
    ]
