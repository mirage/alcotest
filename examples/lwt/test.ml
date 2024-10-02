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

open Lwt.Syntax

exception Library_exception

module To_test = struct
  let lowercase = String.lowercase_ascii
  let lowercase_lwt s = Lwt.return (lowercase s)
  let exn () = raise Library_exception
  let exn_lwt_toplevel () : unit Lwt.t = raise Library_exception
  let exn_lwt_internal () : unit Lwt.t = Lwt.fail Library_exception
end

(* Helper *)

let lwt_check_raises f =
  let+ res = Lwt.catch
    (fun () -> Lwt.map (fun () -> Ok ()) @@ f ())
    (function e -> Lwt.return @@ Error e)
  in
  match res with
  | Ok () -> Alcotest.fail "No exception was thrown"
  | Error Library_exception -> Alcotest.(check pass) "Correct exception" () ()
  | Error _ -> Alcotest.fail "Incorrect exception was thrown"

let switch = ref None

(* The tests *)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.suite "LwtUtils" begin fun group ->
    group "basic" begin fun case ->
      case "Plain" begin fun _ () ->
        Lwt.return (Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!"))
      end;

      case "Lwt" begin fun _ () ->
        To_test.lowercase_lwt "hELLO!"
        |> Lwt.map (Alcotest.(check string) "same string" "hello!")
      end;
    end;

    group "exceptions" begin fun case ->
      case "Plain" begin fun _ () ->
        Lwt.return (Alcotest.check_raises "custom exception" Library_exception To_test.exn)
      end;

      case "Lwt toplevel" begin fun _ () ->
        lwt_check_raises To_test.exn_lwt_toplevel
      end;

      case "Lwt internal" begin fun _ () ->
        lwt_check_raises To_test.exn_lwt_internal
      end;
    end;

    group "switches" begin fun case ->
      case "Allocate resource" begin fun s () ->
        let+ () = Lwt.return_unit in
        switch := Some s;
        Alcotest.(check bool)
          "Passed switch is initially on" (Lwt_switch.is_on s) true
      end;

      case "Check resource deallocated" begin fun _ () ->
        let+ () = Lwt.return_unit in
        match !switch with
        | None -> Alcotest.fail "No switch left by `test_switch_alloc` test"
        | Some s ->
            Alcotest.(check bool)
              "Switch is disabled after test" (Lwt_switch.is_on s) false
      end;
    end;
  end
