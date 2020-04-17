let () =
  let open Alcotest in
  let id () = () in
  Alcotest_unix.run ~and_exit:true "suite-name"
    [ ("test-a", [ test_case "Test case" `Quick id ]) ];
  Printf.printf "\n Should never be printed!%!";
  assert false
