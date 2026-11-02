(* Runs [p] to completion without [Lwt_main.run], which needs [lwt.unix] and
   is unavailable under js_of_ocaml. The tests only wait on [Lwt.pause], so
   waking paused promises until [p] resolves is enough. *)
let rec wakeup_until_resolved p =
  Lwt.wakeup_paused ();
  match Lwt.poll p with
  | Some x -> x
  | None ->
      if Lwt.paused_count () > 0 then wakeup_until_resolved p
      else failwith "unresolved promise"
