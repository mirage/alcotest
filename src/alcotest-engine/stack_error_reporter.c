// caml headers
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdalign.h>
#include <stdbool.h>

#define CRASH_BUFFER_SIZE 10240
static char crash_buffer[CRASH_BUFFER_SIZE];

typedef struct {
  char *buffer;
  size_t capacity;
  size_t offset;
} StackTraceBuffer;

static bool finit_stack_trace_buffer(StackTraceBuffer *pStackTraceBuffer,
                                     size_t size) {
  pStackTraceBuffer->capacity = size;
  pStackTraceBuffer->offset = 0;
  pStackTraceBuffer->buffer = crash_buffer;
  pStackTraceBuffer->buffer[0] = '\0';
  return true;
}

static void append_to_buffer(StackTraceBuffer *sb, const char *format, ...) {
  if (sb->offset >= sb->capacity)
    return; // Buffer full

  va_list args;
  va_start(args, format);

  size_t remaining = sb->capacity - sb->offset;
  int written = vsnprintf(sb->buffer + sb->offset, remaining, format, args);

  va_end(args);

  if (written > 0) {
    if ((size_t)written < remaining) {
      sb->offset += written;
    } else {
      // Truncated or filled exactly; ensure null termination at the end
      sb->offset = sb->capacity - 1;
      sb->buffer[sb->offset] = '\0';
    }
  }
}

static const char *CAML_ERROR_ID = "segfault exception";

#if defined(_WIN32)
// clang-format off
#include <windows.h>
#include <dbghelp.h>
#include <excpt.h>
// clang-format on

// Stacktrace collection inspired by
// https://smhk.net/note/2025/03/c-stack-trace-in-windows/
static void create_stacktrace(StackTraceBuffer *pStackTraceBuffer) {
  HANDLE process = GetCurrentProcess();
  HANDLE thread = GetCurrentThread();
  CONTEXT context;
  STACKFRAME64 stack;
  DWORD machine_type;

  RtlCaptureContext(&context);

  ZeroMemory(&stack, sizeof(STACKFRAME64));

#if defined(_M_IX86) || defined(__i386__)
  machine_type = IMAGE_FILE_MACHINE_I386;
  stack.AddrPC.Offset = context.Eip;
  stack.AddrFrame.Offset = context.Ebp;
  stack.AddrStack.Offset = context.Esp;
#elif defined(_M_X64) || defined(__x86_64__)
  machine_type = IMAGE_FILE_MACHINE_AMD64;
  stack.AddrPC.Offset = context.Rip;
  stack.AddrFrame.Offset = context.Rsp;
  stack.AddrStack.Offset = context.Rsp;
#elif defined(_M_ARM64) || defined(__aarch64__)
  machine_type = IMAGE_FILE_MACHINE_ARM64;
  stack.AddrPC.Offset = context.Pc;
  stack.AddrFrame.Offset = context.Fp;
  stack.AddrStack.Offset = context.Sp;
#else
#error "Unsupported platform"
#endif

  stack.AddrPC.Mode = AddrModeFlat;
  stack.AddrFrame.Mode = AddrModeFlat;
  stack.AddrStack.Mode = AddrModeFlat;

  SymInitialize(process, NULL, TRUE);
  SymSetOptions(SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);

  append_to_buffer(pStackTraceBuffer, "Stack trace:\n");
  append_to_buffer(pStackTraceBuffer, "    %-40s %-18s %s\n", "Function",
                   "Address", "Line");
  append_to_buffer(pStackTraceBuffer, "    %-40s %-18s %s\n", "--------",
                   "-------", "----");

  while (StackWalk64(machine_type, process, thread, &stack, &context, NULL,
                     SymFunctionTableAccess64, SymGetModuleBase64, NULL)) {
    if (stack.AddrPC.Offset == 0)
      break;

    DWORD64 symbol_addr = stack.AddrPC.Offset;
    DWORD64 displacement = 0;
    alignas(SYMBOL_INFO *) char
        symbol_buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)] = {0};
    SYMBOL_INFO *symbol = (SYMBOL_INFO *)symbol_buffer;
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
    symbol->MaxNameLen = MAX_SYM_NAME;

    // Get line information
    IMAGEHLP_LINE64 line = {0};
    line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
    DWORD line_displacement = 0;
    BOOL has_line =
        SymGetLineFromAddr64(process, symbol_addr, &line_displacement, &line);

    char function_name[MAX_SYM_NAME] = "Unknown";
    if (SymFromAddr(process, symbol_addr, &displacement, symbol)) {
      strncpy(function_name, symbol->Name, MAX_SYM_NAME - 1);
      function_name[MAX_SYM_NAME - 1] = '\0'; // Ensure null termination
    }
    // Format line information
    char line_info[256] = "Unknown";
    if (has_line) {
      snprintf(line_info, sizeof(line_info), "%s:%lu", line.FileName,
               line.LineNumber);
    }

    // Print with better alignment using format specifiers
    append_to_buffer(pStackTraceBuffer, "    %-40.40s 0x%016llX %s\n",
                     function_name, symbol_addr, line_info);
    append_to_buffer(pStackTraceBuffer, "\0");
  }

  SymCleanup(process);
}

static LONG WINAPI
windows_exception_handler(EXCEPTION_POINTERS *pExceptionInfo) {
  const DWORD exceptionCode = pExceptionInfo->ExceptionRecord->ExceptionCode;
  switch (exceptionCode) {
  case EXCEPTION_ACCESS_VIOLATION: {
    void *faulting_address =
        (void *)pExceptionInfo->ExceptionRecord->ExceptionInformation[1];
    StackTraceBuffer stack_trace_buffer;
    if (!finit_stack_trace_buffer(&stack_trace_buffer, CRASH_BUFFER_SIZE)) {
      caml_failwith("Can't create stack trace buffer");
      return EXCEPTION_CONTINUE_SEARCH;
    }
    create_stacktrace(&stack_trace_buffer);

    caml_raise_with_string(*caml_named_value(CAML_ERROR_ID),
                           stack_trace_buffer.buffer);
    free(stack_trace_buffer.buffer);
    ExitProcess(STATUS_ACCESS_VIOLATION);
  }
  default:
    break;
  }
  return EXCEPTION_CONTINUE_SEARCH;
}
#else
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#define STACK_TRACE_LENGTH 20

static void unix_signal_handler(int sig, siginfo_t *si, void *unused) {

  StackTraceBuffer stack_trace_buffer;
  if (!finit_stack_trace_buffer(&stack_trace_buffer, CRASH_BUFFER_SIZE)) {
    caml_failwith("Can't create stack trace buffer");
    return;
  }

  void *trace[STACK_TRACE_LENGTH];
  size_t trace_size = backtrace(trace, STACK_TRACE_LENGTH);

  if (trace_size == 0) {
    caml_failwith("Couldn't get backtrace");
    return;
  }

  append_to_buffer(&stack_trace_buffer,
                   "Access violation caught, stacktrace:\n");

  char **pSymbols = backtrace_symbols(trace, trace_size);
  for (int i = 0; i < trace_size; ++i) {
    append_to_buffer(&stack_trace_buffer, "%s\n", pSymbols[i]);
  }
  free(pSymbols);

  caml_raise_with_string(*caml_named_value(CAML_ERROR_ID),
                         stack_trace_buffer.buffer);
  exit(WEXITED);
}
#endif

CAMLprim value caml_setup_stub_exception_handler(void) {
  CAMLparam0();
#if defined(_WIN32)
  AddVectoredExceptionHandler(1, windows_exception_handler);
#else
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = unix_signal_handler;
  sigaction(SIGSEGV, &sa, NULL);
  sigaction(SIGBUS, &sa, NULL); // Catch SIGBUS as well for macOS
#endif
  CAMLreturn(Val_unit);
}