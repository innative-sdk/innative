// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifdef TESTING_WASM
  #include "benchmark.h"

int64_t Benchmarks::fib(int64_t n)
#else
extern "C" __attribute__((visibility("default"))) long long fib(long long n)
#endif
{
  if(n < 0)
    return 0;

  return n + fib(n - 1) + fib(n - 2);
}
