let standardise_filesep =
  let re = Re.compile (Re.str Filename.dir_sep) in
  Re.replace_string ~all:true re ~by:"/"

let build_context_replace =
  let open Re in
  let t = seq [ char '`'; rep any; str "_build"; group (rep any); char '`' ] in
  let re = compile t in
  replace ~all:true re ~f:(fun g ->
      let test_dir = standardise_filesep (Group.get g 1) in
      "`<build-context>/_build" ^ test_dir ^ "`")

let uuid_replace =
  let open Re in
  let hex n = repn (alt [ rg 'A' 'F'; digit ]) n (Some n) in
  let segmented_hex ns =
    let segments = List.map (fun n -> [ char '-'; hex n ]) ns in
    List.flatten segments |> List.tl |> seq
  in
  let t = segmented_hex [ 8; 4; 4; 4; 12 ] in
  let re = compile t in
  replace_string ~all:true re ~by:"<uuid>"

let time_replace =
  let open Re in
  let t =
    seq
      [
        group
          (alt
             [
               str "Test Successful in ";
               seq [ rep1 digit; str " error! in " ];
               seq [ rep1 digit; str " errors! in " ];
             ]);
        rep1 digit;
        char '.';
        rep1 digit;
        char 's';
      ]
  in
  let re = compile t in
  replace ~all:true re ~f:(fun g -> Group.get g 1 ^ "<test-duration>s")

(* Remove all non-deterministic output in a given Alcotest log and write
   the result to std.out *)
let () =
  let in_channel = open_in Sys.argv.(1) in
  try
    let rec loop () =
      let sanitized_line =
        input_line in_channel
        |> uuid_replace
        |> build_context_replace
        |> time_replace
      in
      Printf.printf "%s\n" sanitized_line;
      loop ()
    in
    loop ()
  with End_of_file -> close_in in_channel
