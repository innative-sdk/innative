// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "benchmark.h"

int64_t Benchmarks::vectorloop(int64_t n)
{
  int64_t r[4] = { 0, 0, 0, 0 };
  for(int64_t i = 0; i < n; ++i)
  {
    r[0] += n;
    r[1] += n * i;
    r[2] += n * n;
    r[3] += n + i;
  }

  return r[0] - r[1] + r[2] - r[3];
}
