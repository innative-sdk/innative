// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __OPTIMIZE_H__IR__
#define __OPTIMIZE_H__IR__

#include "intrinsic.h"

namespace innative {
  IR_ERROR OptimizeModules(const Environment* env, code::Context* context);
}

#endif
