#include <caml/mlvalues.h>
#include <caml/memory.h>

#if !defined(_WIN32)
#include <signal.h>
#endif

value caml_segfault_call(void) {
  CAMLparam0();
  volatile int *p = (volatile int *)0;
  *p = 0xDEADBEEF;
  #if !defined(_WIN32)
  // in case mac won't call segfault
  raise(SIGSEGV);
  #endif
  CAMLreturn(Int_val(0));
}