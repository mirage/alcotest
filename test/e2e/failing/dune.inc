(executables
 (names
   unicode_testname
 )
 (libraries alcotest)
 (modules
   unicode_testname
 )
)

(rule
 (target unicode_testname.actual)
 (action
  (with-outputs-to %{target}
   (run ../expect_failure.exe %{dep:unicode_testname.exe})
  )
 )
)

(rule
 (target unicode_testname.processed)
 (action
  (with-outputs-to %{target}
   (run ../strip_randomness.exe %{dep:unicode_testname.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff unicode_testname.expected unicode_testname.processed)
 )
)
