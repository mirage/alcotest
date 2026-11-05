let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "tags-command",
        [
          test_case "plain" `Quick id;
          test_case "always" `Always id;
          test_case "db" (`Tag "db") id;
          test_case "either" Tag.(v "net" || v "crypto") id;
          test_case "not ci" Tag.(not (v "ci")) id;
          ("tuple", `Tag "db", id);
        ] );
    ]
