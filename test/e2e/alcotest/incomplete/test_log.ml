(* The log of an incomplete test ends with its reason. *)
let todo () =
  print_endline "output";
  failwith "TODO: the reason"

let () =
  let root = Filename.temp_file "alcotest" "" in
  Sys.remove root;
  Alcotest.run ~and_exit:false ~log_dir:root "log"
    [ ("t", [ Alcotest.test_case "todo" `Quick todo ]) ];
  let runs = List.fold_left Filename.concat root [ "run"; "log" ] in
  let run = Filename.concat runs (Sys.readdir runs).(0) in
  let log =
    In_channel.with_open_text
      (Filename.concat run "t.000.output")
      In_channel.input_all
  in
  if log <> "output\n[incomplete] the reason" then
    failwith ("unexpected log: " ^ String.escaped log)
