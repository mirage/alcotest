### 0.8.3 (2018-03-25)

- Show one failure when multiple tests fail (#117, @aantron)

### 0.8.2 (2017-08-21)

- add `Async` support: there is a new `alcotest-async` package containing
  an `Alcotest_async` module (#104, @rgrinberg)

### 0.8.1 (2017-08-03)

- Add `failf` (#105, @hcarty)
- Relax the `float` combinator to compare its epsilon using `<=` instead
  of `<`. This allows to use `float 0.` for "exact" float comparison
  (#107, @samoht, @talex5)
- Fix outdated displayed information when using `--verbose`.
  Be clearer that no new output logs are actually created and
  do not try to display outdated information (#108, @samoht)


### 0.8.0 (2017-06-22)

- Format "got" and "expected" values in the same way (#86, @talex5)
- Change the `float` combinator to take a mandatory 'epsilon' parameter
  (#89, @superbobry)
- Switch to jbuilder (#92, @rgrinberg)
- Add a `test_case` function (#94, @samoht)
- Add an `alcotest-lwt` package, containing an `Alcotet_lwt` module with
  an `Alcotest_lwt.test_case` function to better deal with lwt tests
  (#94, @talex5, @samoht)
- Add `Alcotest.neg` to negate test results (#95, @samoht)
- Change the `test_case` type from `unit -> unit` to `'a -> unit`. The `'a`
  parameter can be built using as a `Cmdliner` term using the new
  `run_with_args` function. This is useful to configure the tests using the CLI
  (#96, @samoht)

### 0.7.2 (2016-11-10)

- Clean up handling of env variables (#83, @samoht)

### 0.7.1 (2016-11-03)

- Store tests output to `_build/_tests` by default (#77, @pqwy)

### 0.7.0 (2016-10-25)

- Add a `unit` testable (useful for functions with side-effects) (#79, @avsm)
- Add a `testable` combinator to easily build `'a testable` values (#75, @pqwy)
- Add `pp` and `equal` to extract the pretty-printer and equality functions
  from an `'a testable` (#75, @pqwy)
- Add an `array` testable (#75, @pqwy)

### 0.6.0 (2016-06-28)

- Add int32,int64,float testables (#71, @hcarty)

### 0.5.0 (2016-06-27)

* Use `topkg` (#68, @samoht)
* Add `Alcotest.reject` to always fail tests (#64, @talex5)
* Fix pretty-printing of `Alcotest.list` (#53, #65, @talex5)
* Add an `argv` optional argument to `run` to use custom command-line arguments
  (#63, @dinosaure)
* Fix typo in JSON output (#67, @fxfactorial)
* Use `Astring` for the unit tests (#62, @hannesm)

### 0.4.11 (2016-05-11)

* Fix regression introduced in 0.4.8 about alignment of [ERROR] (#60, @samoht)

### 0.4.10 (2016-05-03)

* Fix support for 4.03 (#58, by @hannesm)

### 0.4.9 (2016-02-25)

* Add `Alcotest.pass` a testable combinator which always pass (#50, @seliopou)
* Fix `index out of bounds` for empty test doc string (#51, @dariusf)
* Display the log directory (@samoht)
* Add missing newline in display header (#53, #54, @samoht)
* Add a `--color` flag to tweak color usage on the command-line and use `Fmt`
  (#52, #55, @samoht)

### 0.4.8 (2016-03-12)

* Fix `check_raises` (#48, by @yallop)
* Use Astring (this drops support for 4.00) (@samoht)
* Simplify the build system (@samoht)

### 0.4.7 (2016-02-22)

* Minimal fix to ensure windows support (#46, by @samoht)

### 0.4.6 (2015-12-29)

* Add missing newline to verbose output (#36, by @seliopou)
* Result: add Result.result combinator (#37, by @seliopou)
* When redirecting stdout/stderr, use a single fd to share the seek offset
  (#39, by @dsheets)
* If redirecting output, print error results as well (#39, by @dsheets)

### 0.4.5 (2015-09.16)

* Add boolean assert: `Alcotest.bool` (#33, by @zjhmale)
* Add sorted list assert: `Alcotest.slist` (#34, by @samoht)
* Add pair assert: `Alcotest.pair` (#34, by @samoht)
* Add simple assert, built using `Pervasive.compare` and a pretty-printing
  function: `Alcotest.of_pp` (#34, by @samoht)

### 0.4.4 (2015-07-31)

* Fix of the format of log filenames
* Fix a regression in 0.4.* which were hiding error messages when using wrong
  command-line arguments

### 0.4.3 (2015-07-22)

* Flush formatter for `Alcotest.check` (#27, by @edwintorok)
* Handle UTF8 for test documentation strings (#5)

### 0.4.2 (2015-07-03)

* Improve the result outputs

### 0.4.1 (2015-07-03)

* Fix regression introduced in 0.4.0: display the error if there is only
  one error
* Add a testable combinator for options.

### 0.4.0 (2015-06-29)

* Simplify the use of the library by removing global states -- now calling
  the `run` function multiple times is much more consistent.
* Remove the direct dependency to `OUnit`. Programs using `OUnit` and `Alcotest`
  should continue to work.
* Add a `TESTABLE` signature and a `check` function to check invariants in
  the tested libraries.

### 0.3.3 (2015-06-19)

* Control `--show-errors` using the ALCOTEST_SHOW_ERRORS env variable (#9)
* Add an `and_exit` optional argument to `Alcotest.run` to control
  the exit behavior of the main test function (#4)
* Fix the output of `--version`
* Add a `--json` argument to show the test results as a JSON object
  (#14, by @eowzukw)
* Expose `Alcotest.result` to turn a test into a result

### 0.3.2: (2015-06-08)

* Do not fail if the output file does not exist
* Add a simple example (#10, by @leowzukw)
* Add a logo (#12, by @leowzukw)

### 0.3.1 (2015-04-18)

* Fix OCaml 4.01.0 and earlier support (regressed in 0.3.0).
* Add Travis CI tests.

### 0.3.0 (2015-04-13)

* Fix backtrace handling (#2 by @dsheets)
* Use `Bytes` module instead of `String`

### 0.2.0 (2012-12-19)

* Fix issues with redirections
* Display the full errors when only one test is selected

### 0.1.0 (2012-12-12)

* Initial release
