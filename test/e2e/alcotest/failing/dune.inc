(executables
 (names
   check_basic
   compact
   duplicate_test_names
   exception_in_test
   filter_all_tests
   invalid_arg_in_test
   tail_errors_limit
   tail_errors_unlimited
   unicode_testname
   unknown_option
 )
 (libraries alcotest)
 (modules
   check_basic
   compact
   duplicate_test_names
   exception_in_test
   filter_all_tests
   invalid_arg_in_test
   tail_errors_limit
   tail_errors_unlimited
   unicode_testname
   unknown_option
 )
)

(rule
 (target check_basic.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:check_basic.exe})
  )
 )
)

(rule
 (target check_basic.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_basic.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff check_basic.expected check_basic.processed)))

(rule
 (target compact.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:compact.exe})
  )
 )
)

(rule
 (target compact.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:compact.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff compact.expected compact.processed)))

(rule
 (target duplicate_test_names.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:duplicate_test_names.exe})
  )
 )
)

(rule
 (target duplicate_test_names.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:duplicate_test_names.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff duplicate_test_names.expected duplicate_test_names.processed)))

(rule
 (target exception_in_test.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:exception_in_test.exe})
  )
 )
)

(rule
 (target exception_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:exception_in_test.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff exception_in_test.expected exception_in_test.processed)))

(rule
 (target filter_all_tests.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:filter_all_tests.exe} test bar 1)
  )
 )
)

(rule
 (target filter_all_tests.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_all_tests.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_all_tests.expected filter_all_tests.processed)))

(rule
 (target invalid_arg_in_test.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:invalid_arg_in_test.exe})
  )
 )
)

(rule
 (target invalid_arg_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:invalid_arg_in_test.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff invalid_arg_in_test.expected invalid_arg_in_test.processed)))

(rule
 (target tail_errors_limit.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:tail_errors_limit.exe})
  )
 )
)

(rule
 (target tail_errors_limit.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_limit.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff tail_errors_limit.expected tail_errors_limit.processed)))

(rule
 (target tail_errors_unlimited.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:tail_errors_unlimited.exe})
  )
 )
)

(rule
 (target tail_errors_unlimited.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_unlimited.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff tail_errors_unlimited.expected tail_errors_unlimited.processed)))

(rule
 (target unicode_testname.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:unicode_testname.exe})
  )
 )
)

(rule
 (target unicode_testname.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unicode_testname.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff unicode_testname.expected unicode_testname.processed)))

(rule
 (target unknown_option.actual)
 (action
  (with-outputs-to %{target}
   (run ../../expect_failure.exe %{dep:unknown_option.exe} --dry-runn)
  )
 )
)

(rule
 (target unknown_option.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unknown_option.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff unknown_option.expected unknown_option.processed)))
