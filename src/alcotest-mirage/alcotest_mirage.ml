module Tester = Alcotest.Cli.Make (Lwt)
include Tester

let test_case_sync n s f = test_case n s (fun x -> Lwt.return (f x))

let run_test fn args =
  let async_ex, async_waker = Lwt.wait () in
  let handle_exn ex =
    Logs.debug (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
    if Lwt.state async_ex = Lwt.Sleep then Lwt.wakeup_exn async_waker ex
  in
  Lwt.async_exception_hook := handle_exn;
  Lwt_switch.with_switch (fun sw -> Lwt.pick [ fn sw args; async_ex ])

let test_case n s f = test_case n s (run_test f)




(*

let src = Logs.Src.create "Alcotest_mirage" ~doc:"Alcotest framework for MirageOS"
module Log = (val Logs.src_log src : Logs.LOG)

type u = unit Lwt.t

type 'a run = 'a -> u

type 'a test_case = string * speed_level * 'a run

type 'a test = string * 'a test_case list

let show_line msg =
  Log.err (fun f -> f "ASSERT %s" msg)

let fail = Alcotest.Common.fail ~show_line
let failf fmt = Alcotest.Common.failf ~show_line fmt
let check = Alcotest.Common.check ~show_line
let check_raises = Alcotest.Common.check_raises ~show_line

let run name (tests : 'a test list) =
  let open Lwt.Infix in
  let _t = empty ~name ~test_dir:"" ~run_id:"" () in
  Lwt_list.iter_s (fun (suite_name, (suite_tests : 'a test_case list) ) ->
      (* type 'a test_case = string * speed_level * ('a -> u) *)
      Lwt_list.iter_s (fun (name, speed, (f : unit -> unit Lwt.t)) ->
          f () >>= fun _ -> Lwt.return_unit
        ) suite_tests
    ) tests
   *)
