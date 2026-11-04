;; The channel primitives come from out-channel-redirect by Hugo Heuzard:
;; src/runtime.wat in https://github.com/hhugo/out-channel-redirect, MIT.

(module
   (import "env" "caml_ml_get_channel_fd"
      (func $get_fd (param (ref eq)) (result i32)))
   (import "env" "caml_ml_set_channel_fd"
      (func $set_fd (param (ref eq)) (param i32)))

   (global $saved_stdout (mut i32) (i32.const 0))
   (global $saved_stderr (mut i32) (i32.const 0))

   (func (export "alcotest_before_test")
      (param $output (ref eq)) (param $stdout (ref eq)) (param $stderr (ref eq))
      (result (ref eq))
      (global.set $saved_stdout (call $get_fd (local.get $stdout)))
      (global.set $saved_stderr (call $get_fd (local.get $stderr)))
      (call $set_fd (local.get $stdout) (call $get_fd (local.get $output)))
      (call $set_fd (local.get $stderr) (call $get_fd (local.get $output)))
      (ref.i31 (i32.const 0)))

   (func (export "alcotest_after_test")
      (param $stdout (ref eq)) (param $stderr (ref eq)) (result (ref eq))
      (call $set_fd (local.get $stdout) (global.get $saved_stdout))
      (call $set_fd (local.get $stderr) (global.get $saved_stderr))
      (ref.i31 (i32.const 0)))

   (func (export "ocaml_alcotest_get_terminal_dimensions")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0))))
