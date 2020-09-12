(executables
 (names
   exceptions
 )
 (libraries alcotest alcotest-async async async_kernel async_unix core)
 (modules
   exceptions
 )
)

(rule
 (target exceptions.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 125)
    (run %{dep:exceptions.exe})))))

(rule
 (target exceptions.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:exceptions.actual}))))

(rule
 (alias runtest)
 (package alcotest-async)
 (action
   (diff exceptions.expected exceptions.processed)))
