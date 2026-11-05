(* Runs a test in a fresh log directory, and prints the report of that run
   without its durations and date. *)

let rec find_report path =
  if Sys.is_directory path then
    List.find_map
      (fun f -> find_report (Filename.concat path f))
      (Array.to_list (Sys.readdir path))
  else if
    Filename.basename (Filename.dirname path) = "junit"
    && Filename.extension path = ".xml"
  then Some path
  else None

let time =
  Re.(compile (seq [ str "time=\""; rep1 (alt [ digit; char '.' ]); char '"' ]))

let timestamp =
  Re.(compile (seq [ str "timestamp=\""; rep (compl [ char '"' ]); char '"' ]))

let output =
  Re.(
    compile
      (seq
         [ str "<system-out>"; group (shortest (rep any)); str "</system-out>" ]))

let () =
  let dir = Filename.temp_file "alcotest" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  (* Without a directory, the shell would look the executable up in [PATH]. *)
  let exe =
    if Filename.is_implicit Sys.argv.(1) then
      Filename.concat Filename.current_dir_name Sys.argv.(1)
    else Sys.argv.(1)
  in
  let command =
    Filename.quote_command ~stdout:Filename.null ~stderr:Filename.null exe
      [ "-o"; dir ]
  in
  let (_ : int) = Sys.command command in
  let report =
    In_channel.with_open_bin (Option.get (find_report dir)) In_channel.input_all
  in
  (* [--sizes] prints the size of the output of each test instead. *)
  if Array.length Sys.argv > 2 && Sys.argv.(2) = "--sizes" then
    Re.all output report
    |> List.iter (fun g ->
        Printf.printf "%d\n" (String.length (Re.Group.get g 1)))
  else
    report
    |> Re.replace_string ~all:true time ~by:"time=\"<time>\""
    |> Re.replace_string ~all:true timestamp ~by:"timestamp=\"<date>\""
    |> print_string
