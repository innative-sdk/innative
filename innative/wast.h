// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __WAST_H__IN__
#define __WAST_H__IN__

#include "wat.h"

namespace innative {
  namespace wat {
    int ParseWast(Environment& env, const uint8_t* data, size_t sz, const char* path, bool always_compile);
  }
}

#endif
