// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "../innative-env/internal.h"

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

extern INModuleMetadata _innative_internal_zero_module__;

IN_COMPILER_DLLEXPORT extern void env_WASM_trace(uint32_t message, int32_t n, double a0, double a1, double a2, double a3,
                                                 double a4)
{
  if(_innative_internal_zero_module__.n_memories < 1)
    return;

  _innative_internal_write_out((char*)_innative_internal_zero_module__.memories[0] + message, n);
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
