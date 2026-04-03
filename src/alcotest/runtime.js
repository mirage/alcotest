//Provides: ocaml_alcotest_get_terminal_dimensions
function ocaml_alcotest_get_terminal_dimensions(unit) {
  var p = globalThis.process
  if(p && p.stdout && p.stdout.columns && p.stdout.rows) {
    return [0, [0, p.stdout.rows, p.stdout.columns]];
  }
  return 0;
}
