let check name ~sep path expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string))
        path expected
        (Alcotest_stdlib_ext.Path.ancestors ~sep path))

let () =
  Alcotest.run __FILE__
    [
      ( "absolute",
        [
          check "root" ~sep:"/" "/" [ "/" ];
          check "double separator" ~sep:"/" "//a" [ "/"; "//a" ];
          check "one level" ~sep:"/" "/a" [ "/a" ];
          check "nested" ~sep:"/" "/a/b/c" [ "/a"; "/a/b"; "/a/b/c" ];
          check "trailing separator" ~sep:"/" "/a/" [ "/a"; "/a/" ];
          check "cygdrive" ~sep:"/" "/cygdrive/c/a"
            [ "/cygdrive"; "/cygdrive/c"; "/cygdrive/c/a" ];
        ] );
      ( "relative",
        [
          check "one level" ~sep:"/" "a" [ "./a" ];
          check "nested" ~sep:"/" "a/b" [ "./a"; "./a/b" ];
          check "trailing separator" ~sep:"/" "a/" [ "./a"; "./a/" ];
        ] );
      ( "windows",
        [
          check "drive letter" ~sep:"\\" "C:\\a" [ "C:\\a" ];
          check "nested" ~sep:"\\" "C:\\a\\b" [ "C:\\a"; "C:\\a\\b" ];
          check "rooted" ~sep:"\\" "\\a" [ "\\a" ];
          check "relative" ~sep:"\\" "a\\b" [ ".\\a"; ".\\a\\b" ];
        ] );
    ]
