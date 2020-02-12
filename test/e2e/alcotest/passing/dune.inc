(executables
 (names
   and_exit_false
   and_exit_true
   basic
   check_basic
   filter_name
   filter_name_regex
   json_output
   list_tests
   quick_only
   quick_only_regex
 )
 (libraries alcotest)
 (modules
   and_exit_false
   and_exit_true
   basic
   check_basic
   filter_name
   filter_name_regex
   json_output
   list_tests
   quick_only
   quick_only_regex
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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff and_exit_false.expected and_exit_false.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff and_exit_true.expected and_exit_true.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff basic.expected basic.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff check_basic.expected check_basic.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff filter_name.expected filter_name.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff filter_name_regex.expected filter_name_regex.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff json_output.expected json_output.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff list_tests.expected list_tests.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff quick_only.expected quick_only.processed)
 )
)

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


(alias
 (name runtest)
 (package alcotest)
 (action
   (diff quick_only_regex.expected quick_only_regex.processed)
 )
)
