// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __WAST_H__IR__
#define __WAST_H__IR__

#include "wat.h"

namespace innative {
  namespace wat {
    int ParseWast(Environment& env, uint8_t* data, size_t sz);
  }
}

#endif
