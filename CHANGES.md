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
