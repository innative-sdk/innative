// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "../innative-env/internal.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"

HMODULE GetCurrentModule()
{
  HMODULE hModule = NULL;
  GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, (LPCTSTR)GetCurrentModule, &hModule);
  return hModule;
}

void* _loadDLL(const char* name)
{
  HMODULE dll = GetCurrentModule();
  void* p     = GetProcAddress(dll, name);
  FreeLibrary(dll);
  return p;
}
#elif defined(IN_PLATFORM_POSIX)
  #include <unistd.h>
  #include <sys/mman.h>
  #include <dlfcn.h>

void* _loadDLL(const char* name)
{
  void* dll = dlopen(NULL, RTLD_LAZY);
  if(!dll)
    return NULL;

  void* p = dlsym(dll, name);
  dlclose(dll);
  return p;
}
#else
  #error unknown platform!
#endif

void _innative_dump_int(int64_t x)
{
  if(x == 0)
  {
    _innative_internal_write_out("0", 1);
    return;
  }

  char buf[25];
  buf[24] = 0;
  int i   = 24;

  while(x != 0)
  {
    buf[--i] = '0' + (x % 10);
    x /= 10;
  }

  _innative_internal_write_out(buf + i, 24 - i);
}

void _innative_dump_double(double d)
{
  _innative_dump_int((int64_t)d);
  _innative_internal_write_out(".", 1);
  _innative_dump_int((int64_t)((d - (int64_t)d) * 1000000.0));
}

IN_COMPILER_DLLEXPORT extern void env_WASM_trace(uint32_t message, int32_t n, double a0, double a1, double a2, double a3,
                                                 double a4)
{
  INModuleMetadata* p = (INModuleMetadata*)_loadDLL("$_innative_module#0");
  if(!p || p->n_memories < 1)
    return;

  _innative_internal_write_out((char*)p->memories[0] + message, n);
  if(n > 1)
    _innative_internal_write_out(" ", 1);
  _innative_dump_double(a0);
  _innative_internal_write_out(", ", 2);
  _innative_dump_double(a1);
  _innative_internal_write_out(", ", 2);
  _innative_dump_double(a2);
  _innative_internal_write_out(", ", 2);
  _innative_dump_double(a3);
  _innative_internal_write_out(", ", 2);
  _innative_dump_double(a4);
  _innative_internal_write_out("\n", 1);
}

IN_COMPILER_DLLEXPORT extern void env_WASM_abort(uint32_t message, uint32_t file, uint32_t line, uint32_t column)
{
  env_WASM_trace(message, 0, 0, 0, 0, 0, 0);
  env_WASM_trace(file, 2, line, column, 0, 0, 0);
  _innative_internal_abort();
}
