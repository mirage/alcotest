open Core
open Async_kernel
open Async_unix

let run timeout name fn args =
  Thread_safe.block_on_async_exn (fun () ->
      Clock.with_timeout timeout (fn args) >>| function
      | `Result x -> x
      | `Timeout -> Alcotest.fail (
          Printf.sprintf "%s timed out after %s"
            name (Time.Span.to_string_hum timeout)
        )
    )

let test_case ?(timeout=sec 2.) name s f =
  Alcotest.test_case name s (run timeout name f)
