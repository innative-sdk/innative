// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include <vector>
#include <string>

namespace innative {
  IN_ERROR CompileEnvironment(const Environment* env, const char* file);
  void DeleteCache(const Environment& env, Module& m);
  void DeleteContext(Environment& env, bool shutdown = false);
  std::vector<std::string> GetSymbols(const char* file, FILE* log);
  void AppendIntrinsics(Environment& env);
}

#endif
