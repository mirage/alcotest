(executables
 (names
   basic
 )
 (libraries alcotest)
 (modules
   basic
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
   (run ../strip_randomness.exe %{dep:basic.actual})
  )
 )
)


(alias
 (name runtest)
 (action
   (diff basic.expected basic.processed)
 )
)
