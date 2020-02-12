let free () =
  print_endline "freeing all resources";
  Lwt.return ()

let test_lwt switch () =
  Lwt_switch.add_hook (Some switch) free;
  Lwt.async (fun () -> failwith "All is broken");
  Lwt_unix.sleep 10.

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run ~verbose:true "foo"
       [ ("all", [ Alcotest_lwt.test_case "one" `Quick test_lwt ]) ]
