// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__WAST_H
#define IN__WAST_H

#include "wat.h"

namespace innative {
  int ParseWast(Environment& env, const uint8_t* data, size_t sz, const path& file, bool always_compile,
                const path& output);
}

#endif
