let t =
  Alcotest.testable
    Fmt.(if_utf_8 (any "utf-8") (any "not utf-8"))
    (fun _ _ -> false)

let () =
  let open Alcotest in
  (try
     run ~and_exit:false __FILE__
       [ ("locale", [ test_case "flag" `Quick (fun () -> check t "" () ()) ]) ]
   with Test_error -> ());
  (* The printer for failures raised outside of a run. *)
  try check t "" () () with e -> print_endline (Printexc.to_string e)
