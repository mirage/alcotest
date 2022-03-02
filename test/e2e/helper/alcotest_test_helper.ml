let rec wakeup_until_resolved p =
  Lwt.wakeup_paused ();
  match Lwt.poll p with
  | Some x -> x
  | None ->
      if Lwt.paused_count () > 0 then wakeup_until_resolved p
      else failwith "unresolved promise"
