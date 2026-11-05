let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ("tags-suite", [ test_case "db" (`Tag "db") id; test_case "s" `Slow id ]);
    ]
