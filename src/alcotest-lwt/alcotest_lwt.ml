(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let run_test fn args =
  let async_ex, async_waker = Lwt.wait () in
  let handle_exn ex =
    Logs.debug (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
    if Lwt.state async_ex = Lwt.Sleep then Lwt.wakeup_exn async_waker ex
  in
  Lwt.async_exception_hook := handle_exn;
  Lwt_switch.with_switch (fun sw -> Lwt.pick [ fn sw args; async_ex ])

module V1 = struct
  module Tester = Alcotest_engine.V1.Cli.Make (Alcotest.Unix_platform) (Lwt)
  include Tester

  type 'a case = ?speed:speed_level -> string -> (Lwt_switch.t -> 'a -> unit Lwt.t) -> unit
  type 'a group = string -> ('a case -> unit) -> unit

  let test_case_sync n s f = test_case n s (fun x -> Lwt.return (f x))
  let test_case n s f = test_case n s (run_test f)

  let suite_testlist register =
    let groups = ref [] in
    let each_group name register =
      let cases = ref [] in
      let each_case ?(speed = `Quick) name func =
        cases := test_case name speed func :: !cases
      in
      register each_case;
      groups := (name, List.rev !cases) :: !groups
    in
    register each_group;
    List.rev !groups

  let suite ?and_exit ?verbose ?compact ?tail_errors ?quick_only
    ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace ?ci ?argv name
    register =
    run ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace ?ci ?argv name
      (suite_testlist register)

  let suite_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
    ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace ?ci ?argv name
    term register =
    run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace ?ci ?argv name
      term (suite_testlist register)
end

include V1
