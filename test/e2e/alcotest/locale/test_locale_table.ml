(* Whether Alcotest treats the output as UTF-8, for each environment. An empty
   value means unset, as it does in POSIX. *)

let detected ~lc_all ~lc_ctype ~lang =
  Unix.putenv "LC_ALL" lc_all;
  Unix.putenv "LC_CTYPE" lc_ctype;
  Unix.putenv "LANG" lang;
  let seen = ref None in
  let flag () =
    let stdout = Alcotest_engine.Formatters.get_stdout () in
    seen := Some (Fmt.utf_8 (stdout :> Format.formatter))
  in
  Alcotest.run ~argv:[| "test" |] ~and_exit:false "locale"
    [ ("locale", [ Alcotest.test_case "flag" `Quick flag ]) ];
  Option.get !seen

let is_utf_8 locale = (locale, "", "", true)
let not_utf_8 locale = (locale, "", "", false)

let cases =
  [
    (* The spellings of the codeset. *)
    is_utf_8 "en_US.UTF-8";
    is_utf_8 "en_US.utf-8";
    is_utf_8 "en_US.UTF8";
    is_utf_8 "en_US.utf8";
    is_utf_8 "C.UTF-8";
    is_utf_8 "C.utf8";
    is_utf_8 "UTF-8";
    (* A modifier follows the codeset. *)
    is_utf_8 "sr_RS.UTF-8@latin";
    is_utf_8 "sr_RS.utf8@latin";
    (* Other locales and codesets. *)
    not_utf_8 "C";
    not_utf_8 "POSIX";
    not_utf_8 "en_US";
    not_utf_8 "en_US.ISO-8859-1";
    not_utf_8 "ja_JP.eucJP";
    not_utf_8 "en_US.UTF-16";
    not_utf_8 "UTF";
    not_utf_8 "UTF-";
    (* Nothing set. *)
    ("", "", "", false);
    (* LC_ALL, then LC_CTYPE, then LANG: the first one set decides. *)
    ("", "", "en_US.UTF-8", true);
    ("", "en_US.UTF-8", "", true);
    ("C", "", "en_US.UTF-8", false);
    ("C", "en_US.UTF-8", "", false);
    ("", "C", "en_US.UTF-8", false);
    ("en_US.UTF-8", "C", "C", true);
    ("", "en_US.UTF-8", "C", true);
  ]

let () =
  let failures =
    List.filter
      (fun (lc_all, lc_ctype, lang, want) ->
        detected ~lc_all ~lc_ctype ~lang <> want)
      cases
  in
  List.iter
    (fun (lc_all, lc_ctype, lang, want) ->
      Printf.eprintf "FAIL LC_ALL=%S LC_CTYPE=%S LANG=%S: want %b\n" lc_all
        lc_ctype lang want)
    failures;
  if failures <> [] then exit 1
