// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__OPTIMIZE_H
#define IN__OPTIMIZE_H

#include "llvm.h"
#include "innative/schema.h"

namespace innative {
  IN_ERROR OptimizeModules(const Environment* env);
}

#endif
