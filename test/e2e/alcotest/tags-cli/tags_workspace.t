  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ mkdir db quick
  $ cat > db/a.ml <<EOF
  > let () =
  >   let open Alcotest in
  >   run "a" [ ("t", [ test_case "db" (\`Tag "db") ignore ]) ]
  > EOF
  $ echo '(test (name a) (libraries alcotest))' > db/dune
  $ cat > quick/b.ml <<EOF
  > let () =
  >   let open Alcotest in
  >   run "b" [ ("t", [ test_case "quick" \`Quick ignore ]) ]
  > EOF
  $ echo '(test (name b) (libraries alcotest))' > quick/dune

  $ export ALCOTEST_COLOR=never
  $ ALCOTEST_TAGS=db dune test --root . --force > out 2>&1
  $ echo $?
  0
  $ grep -E '\[(OK|SKIP)\]|WARNING' out | sed 's/^ *//' | sort
  [OK]          t          0   db.
  [OK]          t          0   quick.
