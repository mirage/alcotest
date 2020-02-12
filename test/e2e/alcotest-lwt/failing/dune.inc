(executables
 (names
   async_failure
 )
 (libraries alcotest alcotest-lwt lwt lwt.unix)
 (modules
   async_failure
 )
)

(rule
 (target async_failure.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:async_failure.exe})
  )
 )
)

(rule
 (target async_failure.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:async_failure.actual})
  )
 )
)


(alias
 (name runtest)
 (package alcotest-lwt)
 (action
   (diff async_failure.expected async_failure.processed)
 )
)
