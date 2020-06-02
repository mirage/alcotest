let standardise_filesep =
  let re = Re.compile (Re.str Filename.dir_sep) in
  Re.replace_string ~all:true re ~by:"/"

let build_context_replace =
  let open Re in
  let lterm, rterm =
    (* Contexts in which directories are printed (tests, manpage output etc.). *)
    ( group (alt [ char '`'; str "(absent=" ]),
      group (alt [ char '`'; char ')' ]) )
  in

  let t = seq [ lterm; rep any; str "_build"; group (rep any); rterm ] in
  let re = compile t in
  replace ~all:true re ~f:(fun g ->
      let test_dir = standardise_filesep (Group.get g 2) in
      Group.get g 1 ^ "<build-context>/_build" ^ test_dir ^ Group.get g 3)

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
               opt cntrl (* Maybe ANSII escape, depending on [--color] *);
               str "Test Successful";
               opt cntrl;
               str " in ";
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

let exception_name_replace =
  let open Re in
  let t = str "Alcotest_engine__Core.Registration_error" in
  let re = compile t in
  replace_string ~all:true re ~by:"Alcotest_engine.Core.Registration_error"

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
        |> exception_name_replace
      in
      Printf.printf "%s\n" sanitized_line;
      loop ()
    in
    loop ()
  with End_of_file -> close_in in_channel
