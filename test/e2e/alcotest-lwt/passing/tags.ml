let () =
  let open Alcotest_lwt in
  let check_switch sw () =
    Alcotest.(check bool) "switch is on" true (Lwt_switch.is_on sw);
    Lwt.return_unit
  in
  let id _ () = Lwt.return_unit in
  let rename (n, s, f) = ("renamed " ^ n, s, f) in
  Lwt_main.run
  @@ run __FILE__
       [
         ( "tagged",
           [
             test_case "db" (`Tag "db") check_switch;
             test_case "no tags" `Quick id;
             test_case_sync "sync slow db" (`And [ `Slow; `Tag "db" ]) ignore;
             test_case_sync "sync" `Quick ignore;
           ] );
         ("rebuilt", List.map rename [ test_case "db" (`Tag "db") id ]);
       ]
