#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

// Detect platform
#if defined(_WIN32)
#define OCAML_ALCOTEST_WINDOWS
#elif defined(__unix__) || defined(__unix)
#include <unistd.h>
#if defined(_POSIX_VERSION)
#define OCAML_ALCOTEST_POSIX
#endif
#endif

// Windows support
#if defined(OCAML_ALCOTEST_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <windows.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);

	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int success = GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
	if (success)
	{
		result = caml_alloc(1, 0);
		pair = caml_alloc(2, 0);
		Store_field(result, 0, pair);
		Store_field(pair, 0, Val_int((int)(csbi.dwSize.Y)));
		Store_field(pair, 1, Val_int((int)(csbi.dwSize.X)));
	}
	else
	{
		result = Val_int(0);
	}

	CAMLreturn(result);
}

// POSIX support
#elif defined(OCAML_ALCOTEST_POSIX)
#include <sys/ioctl.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);
	struct winsize ws;
	int z = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
	if (z == 0)
	{
		result = caml_alloc(1, 0);
		pair = caml_alloc(2, 0);
		Store_field(result, 0, pair);
		Store_field(pair, 0, Val_int(ws.ws_row));
		Store_field(pair, 1, Val_int(ws.ws_col));
	}
	else
	{
		result = Val_int(0);
	}

	CAMLreturn(result);
}

// Unsupported platform
#else

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);

	result = Val_int(0);
	CAMLreturn(result);
}

#endif


/* The definition of channel should be kept in sync with upstream ocaml  */
/* Start of duplicated code from caml/io.h */
#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#elif defined(HAS_OFF_T)
#include <sys/types.h>
typedef off_t file_offset;
#else
typedef long file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
  char * name;                  /* Optional name (to report fd leaks) */
};

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* End of duplicated code from caml/io.h */

/* Start of duplicated code from caml/sys.h */
#define NO_ARG Val_int(0)
CAMLextern void caml_sys_error (value);
/* End of duplicated code from caml/sys.h */

static int alcotest_saved_stdout;
static int alcotest_saved_stderr;

CAMLprim value alcotest_before_test (value voutput, value vstdout, value vstderr) {
  struct channel* output = Channel(voutput);
  struct channel* cstdout = Channel(vstdout);
  struct channel* cstderr = Channel(vstderr);
  int fd, ret;
  fd = dup(cstdout->fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stdout = fd;
  fd = dup(cstderr->fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stderr = fd;
  ret = dup2(output->fd, cstdout->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(output->fd, cstderr->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value alcotest_after_test (value vstdout, value vstderr) {
  struct channel* cstdout = Channel(vstdout);
  struct channel* cstderr = Channel(vstderr);
  int ret;
  ret = dup2(alcotest_saved_stdout, cstdout->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(alcotest_saved_stderr, cstderr->fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stdout);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stderr);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}
