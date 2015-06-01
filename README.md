## ![Alcotest logo](https://raw.githubusercontent.com/leowzukw/alcotest/logo/alcotest-logo.png)

Alcotest is a lightweight and colourful test framework, based on OUnit.

Alcotest exposes a much more restricted interface than OUnit, as you can
only pass to `Alcotest.run` a tree of callbacks of depth 2, and the
callbacks are `unit -> unit` functions that you can build using the
usual `OUnit.assert_*` functions or any other means (including
Quickcheck-like test generators).

This limitation enables Alcotest to provide a quiet and colorful
output where only faulty runs are fully displayed at the end of the
run (with the full logs ready to inspect), with a simple (yet
expressive) query language to select the tests to run.

### Examples

A simple example:

```ocaml
(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* A module with functions to test *)
module To_test = struct
  let capit letter = Char.uppercase letter
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

(* The tests *)
let capit () =
  OUnit.assert_equal 'A' (To_test.capit 'a')

let plus () =
  OUnit.assert_equal 7 (To_test.plus [1;1;2;3])

let test_set = [
  "Capitalize" , `Quick, capit;
  "Add entries", `Slow , plus ;
]

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "test_set", test_set;
  ]
```

The result is a self-contained binary which displays the test results. Use
`./simple.byte --help` to see the runtime options.

```shell
$ ./simple.byte
test_set  0   Capitalize.                                                   [OK]
test_set  1   Add entries.                                                  [OK]
Test Successful in 0.001s. 2 tests run.
```

See the [examples](https://github.com/samoht/alcotest/tree/master/examples)
folder for more examples.
