(executables
 (names
   check_basic
   compact
   exception_in_test
   invalid_arg_in_test
   unicode_testname
 )
 (libraries alcotest)
 (modules
   check_basic
   compact
   exception_in_test
   invalid_arg_in_test
   unicode_testname
 )
)

(rule
 (target check_basic.actual)
 (action
  (with-outputs-to %{target}
   (run ../expect_failure.exe %{dep:check_basic.exe})
  )
 )
)

(rule
 (target check_basic.processed)
 (action
  (with-outputs-to %{target}
   (run ../strip_randomness.exe %{dep:check_basic.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff check_basic.expected check_basic.processed)
 )
)

(rule
 (target compact.actual)
 (action
  (with-outputs-to %{target}
   (run ../expect_failure.exe %{dep:compact.exe})
  )
 )
)

(rule
 (target compact.processed)
 (action
  (with-outputs-to %{target}
   (run ../strip_randomness.exe %{dep:compact.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff compact.expected compact.processed)
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
 (target invalid_arg_in_test.actual)
 (action
  (with-outputs-to %{target}
   (run ../expect_failure.exe %{dep:invalid_arg_in_test.exe})
  )
 )
)

(rule
 (target invalid_arg_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../strip_randomness.exe %{dep:invalid_arg_in_test.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff invalid_arg_in_test.expected invalid_arg_in_test.processed)
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
