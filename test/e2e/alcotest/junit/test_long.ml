(* Only the end of a long log goes in the report. *)
let () =
  Alcotest.run "long"
    [
      Alcotest.suite "t"
        [
          Alcotest.test "long output" (fun () ->
              for i = 1 to 20_000 do
                Printf.printf "line %d of a long output\n" i
              done;
              Alcotest.fail "long");
        ];
    ]
