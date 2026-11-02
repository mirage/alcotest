let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "tags-refused",
        [ test_case "db" (`Tag "db") id; ("net", `Tag "net", id) ] );
    ]
