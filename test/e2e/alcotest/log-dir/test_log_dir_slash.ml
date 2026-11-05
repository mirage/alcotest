(* A log directory written with [/] is created on every platform, as [-o a/b]
   would be. On Windows, [/] is a separator but not [Filename.dir_sep]. *)
let () =
  let root = Filename.temp_file "alcotest" "" in
  Sys.remove root;
  Sys.mkdir root 0o755;
  Alcotest.run ~and_exit:false ~log_dir:(root ^ "/a/b") "log dir"
    [ ("t", [ Alcotest.test_case "ok" `Quick ignore ]) ]
