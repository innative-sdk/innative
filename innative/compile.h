// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include <vector>
#include <string>

namespace innative {
  enum class LLD_FORMAT : unsigned char
  {
    COFF,
    ELF,
    WASM,
  };

  IN_ERROR CompileEnvironment(const Environment* env, const char* file);
  void DeleteCache(const Environment& env, Module& m);
  void DeleteContext(Environment& env, bool shutdown = false);
  std::vector<std::string> GetSymbols(const char* file, FILE* log, LLD_FORMAT format);
  void AppendIntrinsics(Environment& env);
}

#endif
