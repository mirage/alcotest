(executables
 (names
   and_exit_false
   and_exit_true
   assert_and_verbose
   assert_not_printed
   basic
   check_basic
   cli_verbose
   filter_name
   filter_name_regex
   json_output
   list_tests
   only_monadic_effects
   quick_only
   quick_only_regex
   verbose_newlines
 )
 (libraries alcotest alcotest.unix)
 (modules
   and_exit_false
   and_exit_true
   assert_and_verbose
   assert_not_printed
   basic
   check_basic
   cli_verbose
   filter_name
   filter_name_regex
   json_output
   list_tests
   only_monadic_effects
   quick_only
   quick_only_regex
   verbose_newlines
 )
)

(rule
 (target and_exit_false.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:and_exit_false.exe})
  )
 )
)

(rule
 (target and_exit_false.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_false.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff and_exit_false.expected and_exit_false.processed)))

(rule
 (target and_exit_true.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:and_exit_true.exe})
  )
 )
)

(rule
 (target and_exit_true.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_true.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff and_exit_true.expected and_exit_true.processed)))

(rule
 (target assert_and_verbose.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:assert_and_verbose.exe})
  )
 )
)

(rule
 (target assert_and_verbose.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_and_verbose.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff assert_and_verbose.expected assert_and_verbose.processed)))

(rule
 (target assert_not_printed.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:assert_not_printed.exe})
  )
 )
)

(rule
 (target assert_not_printed.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_not_printed.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff assert_not_printed.expected assert_not_printed.processed)))

(rule
 (target basic.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:basic.exe})
  )
 )
)

(rule
 (target basic.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:basic.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff basic.expected basic.processed)))

(rule
 (target check_basic.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:check_basic.exe})
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
 (target cli_verbose.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:cli_verbose.exe} --verbose)
  )
 )
)

(rule
 (target cli_verbose.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_verbose.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff cli_verbose.expected cli_verbose.processed)))

(rule
 (target filter_name.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:filter_name.exe})
  )
 )
)

(rule
 (target filter_name.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_name.expected filter_name.processed)))

(rule
 (target filter_name_regex.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:filter_name_regex.exe})
  )
 )
)

(rule
 (target filter_name_regex.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name_regex.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_name_regex.expected filter_name_regex.processed)))

(rule
 (target json_output.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:json_output.exe})
  )
 )
)

(rule
 (target json_output.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:json_output.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff json_output.expected json_output.processed)))

(rule
 (target list_tests.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:list_tests.exe})
  )
 )
)

(rule
 (target list_tests.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:list_tests.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff list_tests.expected list_tests.processed)))

(rule
 (target only_monadic_effects.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:only_monadic_effects.exe})
  )
 )
)

(rule
 (target only_monadic_effects.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:only_monadic_effects.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff only_monadic_effects.expected only_monadic_effects.processed)))

(rule
 (target quick_only.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:quick_only.exe})
  )
 )
)

(rule
 (target quick_only.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff quick_only.expected quick_only.processed)))

(rule
 (target quick_only_regex.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:quick_only_regex.exe})
  )
 )
)

(rule
 (target quick_only_regex.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only_regex.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff quick_only_regex.expected quick_only_regex.processed)))

(rule
 (target verbose_newlines.actual)
 (action
  (with-outputs-to %{target}
   (run %{dep:verbose_newlines.exe})
  )
 )
)

(rule
 (target verbose_newlines.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:verbose_newlines.actual})
  )
 )
)


(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff verbose_newlines.expected verbose_newlines.processed)))
