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

(* Build with
 * ocamlbuild -pkg alcotest bad.byte *)

(* A module with functions to test *)
module To_test = struct
  let capit letter = Astring.String.Ascii.uppercase letter
  let plus int_list = List.map (fun a -> a + a) int_list
end

(* The tests *)
let capit () =
  Alcotest.(check string) "strings" "A" (To_test.capit "b")

let plus () =
  Alcotest.(check (list int)) "int lists" [1] (To_test.plus [1;1;2;3])

let test_one = [
  "Capitalize" , `Quick, capit;
  "Add entries", `Slow , plus ;
]

let test_two = [
  "ok"         , `Quick, (fun () -> ());
  "Capitalize" , `Quick, capit;
  "ok"         , `Quick, (fun () -> ());
]

(* Run it *)
let one () =
  try Alcotest.run ~and_exit:false "My first test" ["Ωèone", test_one]
  with Alcotest.Test_error -> Printf.printf "Continue!!\n%!"

let two () =
  Alcotest.run ~and_exit:true "Hoho" ["two", test_two]

let () =
  one ();
  two ()
