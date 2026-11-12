(* Where a failure is reported, for each way a test can fail. *)

let fail () = Alcotest.(check int) "different" 1 2

let check_in_body () =
  Alcotest.(check int) "different" 1 2;
  ()

let check_as_tail_call () = Alcotest.(check int) "different" 1 2

let check_in_helper () =
  fail ();
  ()

let check_in_iter () =
  List.iter (fun x -> Alcotest.(check int) "different" 1 x) [ 2 ];
  ()

let raise_as_tail_call () = failwith "boom"

let raise_in_body () =
  ignore (failwith "boom" : int);
  ()

let given_position () =
  Alcotest.check ~pos:__POS__ Alcotest.int "different" 1 2;
  ()

let () =
  Alcotest.run __FILE__
    [
      ( "shapes",
        [
          Alcotest.test_case "check in body" `Quick check_in_body;
          Alcotest.test_case "check as tail call" `Quick check_as_tail_call;
          Alcotest.test_case "check in helper" `Quick check_in_helper;
          Alcotest.test_case "check in iter" `Quick check_in_iter;
          Alcotest.test_case "raise as tail call" `Quick raise_as_tail_call;
          Alcotest.test_case "raise in body" `Quick raise_in_body;
          Alcotest.test_case "given position" `Quick given_position;
          Alcotest.test_case "check in scheduler" `Quick (fun () ->
              Eio_sim.run check_in_body);
        ] );
    ]
