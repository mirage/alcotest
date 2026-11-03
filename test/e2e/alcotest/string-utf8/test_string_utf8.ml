let printed ~utf_8 s =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_utf_8 ppf utf_8;
  Fmt.pf ppf "%a@?" (Alcotest.pp Alcotest.string) s;
  Buffer.contents buf

let check_utf_8 (name, s, expected) =
  Alcotest.(check string) name expected (printed ~utf_8:true s)

let test_utf_8 () =
  List.iter check_utf_8
    [
      ("ascii", "abc", {|"abc"|});
      ("empty", "", {|""|});
      ("symbols", "∀∃", {|"∀∃"|});
      ("accent", "café", {|"café"|});
      ("astral plane", "𝄞", {|"𝄞"|});
      ("controls", "a\n\t\000b", {|"a\n\t\000b"|});
      ("quotes", {|a"b\c|}, {|"a\"b\\c"|});
      ("invalid byte", "a\xffb", {|"a\255b"|});
      ("truncated", "\xe2\x88", {|"\226\136"|});
      ("surrogate", "\xed\xa0\x80", {|"\237\160\128"|});
      ("delete", "\x7f", {|"\127"|});
      ("valid and invalid", "\xe2\x88\x80\xff", {|"\226\136\128\255"|});
    ]

let test_not_utf_8 () =
  Alcotest.(check string)
    "escaped as before" {|"\226\136\128\n"|}
    (printed ~utf_8:false "∀\n")

let () =
  Alcotest.run "string-utf8"
    [
      ( "pp",
        [
          Alcotest.test_case "utf-8" `Quick test_utf_8;
          Alcotest.test_case "not utf-8" `Quick test_not_utf_8;
        ] );
    ]
