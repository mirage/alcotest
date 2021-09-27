
  //Provides: js_print_stderr (const)
  //Requires: caml_utf16_of_utf8
  function js_print_stderr(s) {
    var s = caml_utf16_of_utf8(s);
    var g = joo_global_object;
    if (g.process && g.process.stdout && g.process.stdout.write) {
      g.process.stderr.write(s)
    } else {
      // Do not output the last \n if present
      // as console logging display a newline at the end
      if(s.charCodeAt(s.length - 1) == 10)
        s = s.substr(0,s.length - 1 );
      var v = g.console;
      // redirecting to log rather than warn   
      v && v.error && v.log(s);
    }
  }

//Provides: caml_sys_exit
//Requires: caml_invalid_argument
function caml_sys_exit (code) {
    joo_global_object.console.log("Exit Code: ", code);
}