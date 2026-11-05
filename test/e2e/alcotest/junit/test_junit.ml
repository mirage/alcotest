open Alcotest

let ok () = ()

(* With [~pos], the report gives the position of the check, not of the test. *)
let fail_check () = check ~pos:__POS__ int "different" 1 2

let fail_with_output () =
  print_endline "a line of output";
  fail "boom"

let fail_raise () = failwith "raised"

let check_same x =
  check int "same" 1 x;
  ()

let () =
  run ~record_backtrace:false "junit"
    [
      suite "outcomes"
        [
          test "pass" ok;
          test "skip" (fun () -> skip ());
          test ~tags:(`Tag "off") "not selected" ok;
          test "check" fail_check;
          test "output" fail_with_output;
          test "exception" fail_raise;
        ];
      suite "names"
        [
          test "same" ok;
          test "same" ok;
          test "" ok;
          test "a&b<c>\"d'\te" ok;
          test "control \x01 and invalid \xff" ok;
          group "nested" [ group "deep" [ test "grouped" ok ] ];
          (* A tuple has no registered location. *)
          ( "tuple",
            `Quick,
            fun () ->
              check_same 1;
              () );
        ];
      (* Markup and characters that the report must escape or replace. *)
      suite "</testsuite><x a=\"&bogus;\">"
        [
          test "]]><![CDATA[ &#0; &#xD800; \x00\x07\xff" (fun () ->
              print_string "</system-out></testcase><testcase name=\"x\"/>";
              print_string
                "&amp; &bogus; &#0; ]]> \x00\x1b \xc3\x28 \xed\xa0\x80\n";
              fail "</failure><error>&lt;&#1;\x0b");
        ];
    ]
