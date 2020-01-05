// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__OPTIMIZE_H
#define IN__OPTIMIZE_H

#include "llvm.h"
#include "innative/schema.h"

namespace innative {
  IN_ERROR OptimizeModules(const Environment* env);
}

#endif
