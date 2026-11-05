(* A test case must not keep what its function captures alive once it is gone:
   a program can build many suites. *)
let collected = ref false

let case () =
  let data = Bytes.create 8 in
  Gc.finalise (fun _ -> collected := true) data;
  Alcotest.test_case "x" `Quick (fun () -> ignore (Bytes.length data))

let () =
  ignore (case ());
  Gc.full_major ();
  if not !collected then failwith "the test case is still alive"
