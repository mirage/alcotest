;; WASM stubs for alcotest runtime.
;; Adapted from https://github.com/mirage/alcotest/blob/main/src/alcotest/runtime.js
;; Channel redirection is not available in WASM, so before/after are no-ops.

(module
   (func (export "alcotest_before_test")
      (param $voutput (ref eq)) (param $vstdout (ref eq)) (param $vstderr (ref eq))
      (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "alcotest_after_test")
      (param $vstdout (ref eq)) (param $vstderr (ref eq))
      (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "ocaml_alcotest_get_terminal_dimensions")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
