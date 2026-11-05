(* The location of a failure is on its own line in the log of the test. *)

let rec output_files path =
  if Sys.is_directory path then
    Sys.readdir path
    |> Array.to_list
    |> List.concat_map (fun f -> output_files (Filename.concat path f))
  else if Filename.check_suffix path ".output" then [ path ]
  else []

let () =
  let log_dir = Filename.concat (Sys.getcwd ()) "log-location" in
  (try
     Alcotest.run ~and_exit:false ~log_dir "log"
       [
         ( "t",
           [
             Alcotest.test_case "fail" `Quick (fun () ->
                 Alcotest.fail ~pos:__POS__ "boom");
           ] );
       ]
   with Alcotest.Test_error -> ());
  match output_files log_dir with
  | [] -> failwith "no log"
  | file :: _ ->
      let log = In_channel.with_open_bin file In_channel.input_all in
      if not (List.mem "FAIL boom" (String.split_on_char '\n' log)) then
        failwith ("FAIL is not on its own line in: " ^ log)
