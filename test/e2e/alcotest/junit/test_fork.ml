(* A test forks a process that exits after the run has written its report and
   exited: the report stays the run's. The main process returns once that
   process has exited, which closes the last copy of [done_w]. *)

let read_to_eof fd =
  let (_ : int) = Unix.read fd (Bytes.create 1) 0 1 in
  ()

let run ~done_w =
  (* The run holds [run_w] until it exits. *)
  let run_r, run_w = Unix.pipe () in
  Alcotest.run "fork"
    [
      ( "t",
        [
          Alcotest.test "fork" (fun () ->
              match Unix.fork () with
              | 0 ->
                  Unix.close run_w;
                  read_to_eof run_r;
                  exit 0
              | _ -> Unix.close done_w);
        ] );
    ]

let () =
  let done_r, done_w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      Unix.close done_r;
      run ~done_w
  | _ ->
      Unix.close done_w;
      read_to_eof done_r;
      let (_ : int * Unix.process_status) = Unix.wait () in
      ()
