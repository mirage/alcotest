(*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
*)

(* Run with [dune exec ./examples/bad/bad.exe] *)

(* A module with functions to test *)
module To_test = struct
  let capitalise = Astring.String.Ascii.uppercase
  let double_all = List.map (fun a -> a + a)
end

(* Run both suites completely, even if the first contains failures *)
let () =
  try
    Alcotest.suite ~and_exit:false "First suite" begin fun group ->
      group "to_test" begin fun case ->
        case "capitalise" begin fun () ->
          To_test.capitalise "b" |> Alcotest.(check string) "strings" "A"
        end;

        case ~speed:`Slow "double all" begin fun () ->
          To_test.double_all [ 1; 1; 2; 3 ]
          |> Alcotest.(check (list int)) "int lists" [ 1 ]
        end;
      end;
    end
  with Alcotest.Test_error ->
    Printf.printf "Forging ahead regardless!\n%!";
    Alcotest.suite ~and_exit:false "Second suite" begin fun group ->
      group "Ωèone" begin fun case ->
        case "Passing test 1" ignore;

        case "Failing test" begin fun () ->
          Alcotest.fail "This was never going to work..."
        end;

        case "Passing test 2" ignore;
      end;
    end;
    Printf.printf "Finally done."
