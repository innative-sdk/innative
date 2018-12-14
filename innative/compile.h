// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __COMPILE_H__IR__
#define __COMPILE_H__IR__

#include "innative/schema.h"
#include <vector>
#include <string>

namespace innative {
  IR_ERROR CompileEnvironment(const Environment* env, const char* file);
  std::vector<std::string> GetSymbols(const char* file);
  void AppendIntrinsics(Environment& env);
}

#endif
