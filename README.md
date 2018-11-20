## ![Alcotest logo](https://raw.githubusercontent.com/mirage/alcotest/master/alcotest-logo.png)

Alcotest is a lightweight and colourful test framework.

Alcotest exposes simple interface to perform unit tests. It exposes
a simple `TESTABLE` module type, a `check` function to assert test
predicates and a `run` function to perform a list of `unit -> unit`
test callbacks.

Alcotest provides a quiet and colorful output where only faulty runs
are fully displayed at the end of the run (with the full logs ready to
inspect), with a simple (yet expressive) query language to select the
tests to run.

[![Build Status](https://travis-ci.org/mirage/alcotest.svg)](https://travis-ci.org/mirage/alcotest)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/alcotest/alcotest/index.html)

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
  Alcotest.(check char) "same chars"  'A' (To_test.capit 'a')

let plus () =
  Alcotest.(check int) "same ints" 7 (To_test.plus [1;1;2;3])

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
$ ./simple.native
[OK]        test_set  0   Capitalize.
[OK]        test_set  1   Add entries.
Test Successful in 0.001s. 2 tests run.
```

You can filter which tests to run by supplying either the exact test name
(which would run all testcases with that name), or the exact test name
and test case number (which would run just that single test):
```shell
$ ./simple.native test test_set
Testing My first test.
[OK]              test_set          0   Capitalize.
[OK]              test_set          1   Add entries.
The full test results are available in `_build/_tests`.
Test Successful in 0.000s. 2 test run.
$ ./simple.native test test_set 1
Testing My first test.
[SKIP]              test_set          0   Capitalize.
[OK]                test_set          1   Add entries.
The full test results are available in `_build/_tests`.
Test Successful in 0.000s. 1 test run.
```

Note that you cannot filter by test case name (i.e. `Capitalize` or `Add entries`), you have to use
the test case number instead.

See the [examples](https://github.com/mirage/alcotest/tree/master/examples)
folder for more examples.

### Quick and Slow tests

In general you should use `` `Quick`` tests: tests that are ran on any
invocations of the test suite. You should only use `` `Slow`` tests for stress
tests that are ran only on occasion (typically before a release or after a major
change). These slow tests can be suppressed by passing the `-q` flag on the
command line, e.g.:

```
$ ./test.exe -q # run only the quick tests
$ ./test.exe    # run quick and slow tests
```

### Passing custom options to the tests

In most cases, the base tests are `unit -> unit` functions. However,
it is also possible to pass an extra option to all the test functions
by using `'a -> unit`, where `'a` is the type of the extra parameter.

In order to do this, you need to specify how this extra parameter is
read on the command-line, by providing a [Cmdliner term for
command-line
arguments](http://erratique.ch/software/cmdliner/doc/Cmdliner.Term.html)
which explains how to parse and serialize values of type `'a` (*note:* do not
use positional arguments, only optional arguments are supported).

For instance:

```ocaml
let test_nice i = Alcotest.(check int) "Is it a nice integer?" i 42

let int =
  let doc = "What is your prefered number?" in
  Cmdliner.Arg.(required & opt (some int) None & info ["n"] ~doc ~docv:"NUM")

let () =
  Alcotest.run_with_args "foo" int [
    "all", ["nice", `Quick, test_nice]
  ]
```

Will generate `test.exe` such that:

```
$ test.exe test
test.exe: required option -n is missing

$ test.exe test -n 42
Testing foo.
[OK]                all          0   int.
```

### Lwt

Alcotest provides an `Alcotest_lwt` module that you could use to wrap
Lwt test cases. The basic idea is that instead of providing a test
function in the form `unit -> unit`, you provide one with the type
`unit -> unit Lwt.t` and alcotest-lwt calls `Lwt_main.run` for you.

However, there are a couple of extra features:

- If an async exception occurs, it will cancel your test case for you
  and fail it (rather than exiting the process).

- You get given a switch, which will be turned off when the test case
  finishes (or fails). You can use that to free up any resources.

For instance:

```ocaml
let free () = print_endline "freeing all resources"; Lwt.return ()

let test_lwt switch () =
  Lwt_switch.add_hook (Some switch) free;
  Lwt.async (fun () -> failwith "All is broken");
  Lwt_unix.sleep 10.

let () =
  Alcotest.run "foo" [
    "all", [
      Alcotest_lwt.test_case "one" `Quick test_lwt
    ]
  ]
```

Will generate:

```
$ test.exe
Testing foo.
[ERROR]             all          0   one.
-- all.000 [one.] Failed --
in _build/_tests/all.000.output:
freeing all resources
[failure] All is broken
```

### Screenshots

The following screenshots demonstrate the HTML testing output from the [odoc](https://github.com/ocaml/odoc) project.

| All tests passed                   | Some tests failed                    | Failed test with custom diffing          |
|------------------------------------|--------------------------------------|------------------------------------------|
| ![ok](screenshots/ok.png?raw=true) | ![err](screenshots/err.png?raw=true) | ![diff](screenshots/diff.png?raw=true)   |


### Comparison with other testing frameworks

The README is pretty clear about that:

> Alcotest is a lightweight and colourful test framework.

Alcotest is the only testing framework using colors!

More seriously, Alcotest is similar to [ounit](http://ounit.forge.ocamlcore.org/)
but it fixes a few of the problems found in that library:

- Alcotest has a nicer output, it is easier to see what failed and what
  succeeded and to read the log outputs of the failed tests;

- Alcotest uses combinators to define pretty-printers and
  comparators between the things to test.

Other nice tools doing different kind of testing also exist:

- [qcheck](https://github.com/c-cube/qcheck) qcheck does random
  generation and property testing (e.g. Quick Check)

- [crowbar](https://github.com/stedolan/crowbar)
  and [bun](https://github.com/yomimono/ocaml-bun)
  are similar to qcheck, but use compiler-directed randomness,
  e.g. it takes advantage of the AFL support  the OCaml compiler.

- [`ppx_inline_tests`](https://github.com/janestreet/ppx_inline_test)
  allows to write tests in the same file as your source-code; they
  will be run only in a special mode of compilation.
