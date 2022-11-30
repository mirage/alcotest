(executables
 (names
   cli_options
 )
 (libraries alcotest alcotest.stdlib_ext alcotest-lwt lwt alcotest_test_helper)
 (modes native js)
 (modules
   cli_options
 )
)

(rule
 (target cli_options.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 124 125)
    (run %{dep:cli_options.exe} --json)))))

(rule
 (target cli_options.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_options.actual}))))

(rule
 (alias runtest)
 (package alcotest-lwt)
 (action
   (diff cli_options.expected cli_options.processed)))

(rule
 (target cli_options-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:cli_options.bc.js} --json)))))

(rule
 (target cli_options-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_options-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest-lwt)
 (action
   (diff cli_options-js.expected cli_options-js.processed)))
