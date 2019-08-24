(* Run an executable and negate its return status *)
let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let command = Fmt.strf "%a" Fmt.(list ~sep:(const string " ") string) args in
  let rel_command = Filename.(concat current_dir_name) command in
  match Unix.system rel_command with
  | WEXITED 0 -> exit 1
  | WEXITED _ -> exit 0
  | WSIGNALED _ | WSTOPPED _ -> failwith "Process terminated by a signal"
