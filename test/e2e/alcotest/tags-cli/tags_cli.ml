let () =
  let open Alcotest in
  let id () = () in
  run "tags-cli"
    [
      ( "new-api",
        [
          test_case "q" `Quick id; test_case "sd" (`And [ `Slow; `Tag "db" ]) id;
        ] );
      ("old-api", [ test_case "os" `Slow id ]);
      ("grouped", [ group "g" [ test_case "gn" (`Tag "net") id ] ]);
    ]
