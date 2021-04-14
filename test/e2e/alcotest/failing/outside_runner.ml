(** Capture the behaviour of the assertion functions when not running inside
    [Alcotest.run]. *)

let () =
  Alcotest.(check int) "Passing assertion" 1 1;
  Alcotest.(check int) "Failing assertion" 1 2
