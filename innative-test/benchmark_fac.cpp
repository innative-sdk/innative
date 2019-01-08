// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "benchmark.h"

int64_t Benchmarks::fac(int64_t n)
{
  if(n < 0)
    return 0;

  return n + fac(n - 1) + fac(n - 2);
}
