#!/bin/sh
# Runs a wasm test suite the way a downstream project does: against an installed
# alcotest, so that the wasm stubs have to be installed too. The cache is off
# because dune can reuse the result of an identical test binary.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
prefix=$(mktemp -d)
logs=$(mktemp -d)

dune build --root "$here/../.." --cache=disabled --only-packages alcotest @install
dune install --root "$here/../.." --prefix "$prefix" alcotest

OCAMLPATH="$prefix/lib" dune build --root "$here" --cache=disabled ./t.bc.wasm.js

status=0
output=$(node "$here/_build/default/t.bc.wasm.js" -o "$logs" 2>&1) || status=$?
printf '%s\n' "$output"

fail() {
  echo "$1" >&2
  exit 1
}

[ "$status" = 1 ] || fail "expected exit code 1, got $status"

case "$output" in
*MARK-*-passing*) fail "the output of a passing test was not captured" ;;
esac
case "$output" in
*MARK-stdout-failing*) ;;
*) fail "the output of a failing test was not shown" ;;
esac

for mark in MARK-stdout-passing MARK-stderr-passing; do
  grep -rq "$mark" "$logs" || fail "$mark is missing from the logs"
done
