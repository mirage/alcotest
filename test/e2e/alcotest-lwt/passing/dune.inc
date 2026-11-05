(executables
 (names
   cli_options
   tags
 )
 (libraries alcotest alcotest.stdlib_ext alcotest-lwt lwt lwt.unix)
 (modes exe)
 (modules
   cli_options
   tags
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
 (target tags.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 124 125)
    (run %{dep:tags.exe} --tags=db)))))

(rule
 (target tags.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tags.actual}))))

(rule
 (alias runtest)
 (package alcotest-lwt)
 (action
   (diff tags.expected tags.processed)))
