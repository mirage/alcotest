(executables
 (names
   exception_in_test
   unicode_testname
 )
 (libraries alcotest alcotest.cli)
 (modules
   exception_in_test
   unicode_testname
 )
)

(rule
 (target exception_in_test.actual)
 (action
  (with-outputs-to %{target}
   (run ../expect_failure.exe %{dep:exception_in_test.exe})
  )
 )
)

(rule
 (target exception_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../strip_randomness.exe %{dep:exception_in_test.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff exception_in_test.expected exception_in_test.processed)
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
