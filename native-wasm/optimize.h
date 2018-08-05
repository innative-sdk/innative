// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __OPTIMIZE_H__IR__
#define __OPTIMIZE_H__IR__

#include "intrinsic.h"

namespace innative {
  IR_ERROR AnnotateFunctions(Environment* env, code::Context* contexts);
}

#endif
