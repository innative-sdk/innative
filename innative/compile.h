// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include "constants.h"
#include <vector>
#include <string>

namespace innative {
  IN_ERROR CompileEnvironment(const Environment* env, const char* file);
  int GetCallingConvention(const Import& imp);
}

#endif
