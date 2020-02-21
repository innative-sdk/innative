// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "../innative-env/internal.h"

IN_COMPILER_DLLEXPORT extern void env_WASM_trace(uint32_t message, int32_t n, double a0, double a1, double a2, double a3,
                                                 double a4)
{}

IN_COMPILER_DLLEXPORT extern void env_WASM_abort(uint32_t message, uint32_t file, uint32_t line, uint32_t column)
{
  env_WASM_trace(message, 0, 0, 0, 0, 0, 0);
  env_WASM_trace(file, 2, line, column, 0, 0, 0);
  _innative_internal_abort();
}
