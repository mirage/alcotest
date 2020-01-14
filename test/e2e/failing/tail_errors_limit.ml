let test_error_output () =
  for i = 1 to 100 do
    Printf.printf "output line %i\n" i
  done;
  Alcotest.fail "Logs above should be 10 lines long (omitting 91)."

let () =
  let open Alcotest in
  run ~tail_errors:(`Limit 10) "tail_errors_limit"
    [ ("failing", [ test_case "test" `Quick test_error_output ]) ]
