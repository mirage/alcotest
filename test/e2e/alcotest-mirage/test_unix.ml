(* Mirage does not keep logs on disk, so a run must neither try to nor print
   where they are. *)
module Clock = struct
  (* mirage-clock 2 also requires the signature of a device, which later
     versions do not use. *)
  [@@@warning "-32-34"]

  type t = unit
  type 'a io = 'a Lwt.t

  let disconnect () = Lwt.return_unit
  let elapsed_ns () = Int64.of_float (Unix.gettimeofday () *. 1e9)
  let period_ns () = None
end

module Alcotest = Alcotest_mirage.Make (Clock)

let () =
  Lwt_main.run
    (Alcotest.run ~argv:[| "test" |] "mirage"
       [ ("t", [ Alcotest.test_case_sync "ok" `Quick ignore ]) ])
