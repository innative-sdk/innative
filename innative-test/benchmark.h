// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __BENCHMARK_H__IR__
#define __BENCHMARK_H__IR__

#include "innative/export.h"
#include <utility>
#include <stdint.h>
#include <stdio.h>
#include <vector>
#include <string.h>
#include <chrono>

class Benchmarks
{
  struct Timing
  {
    int64_t c;
    int64_t debug;
    int64_t strict;
    int64_t sandbox;
    int64_t native;
  };

public:
  Benchmarks(const IRExports& exports, const char* arg0, int loglevel);
  static int nbody(int n);
  static int64_t vectorloop(int64_t n);
  static int64_t fac(int64_t n);

  template<typename R, typename... Args>
  Timing DoBenchmark(const char* wasm, const char* func, R(*f)(Args...), Args&&... args)
  {
    Timing timing;

    timing.c = MeasureFunction<R, Args...>(f, std::forward<Args>(args)...);
    timing.debug = MeasureWASM<R, Args...>(wasm, func, ENV_DEBUG|ENV_STRICT, ENV_OPTIMIZE_O0, std::forward<Args>(args)...);
    timing.strict = MeasureWASM<R, Args...>(wasm, func, ENV_STRICT, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);
    timing.sandbox = MeasureWASM<R, Args...>(wasm, func, ENV_SANDBOX, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);
    timing.native = MeasureWASM<R, Args...>(wasm, func, 0, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);

    return timing;
  }

  template<typename R, typename... Args>
  int64_t MeasureWASM(const char* wasm, const char* func, int flags, int optimize, Args&&... args)
  {
    void* m = LoadWASM(wasm, flags, optimize);
    R(*f)(Args...) = (R(*)(Args...))(*_exports.LoadFunction)(m, wasm, func);
    assert(f != nullptr);
    int64_t t = MeasureFunction(f, std::forward<Args>(args)...);
    (*_exports.FreeAssembly)(m);
    return t;
  }

  template<typename R, typename... Args>
  int64_t MeasureFunction(R(*f)(Args...), Args&&... args)
  {
    auto t = start();
    f(std::forward<Args>(args)...);
    return end(t);
  }

protected:
  void* LoadWASM(const char* wasm, int flags, int optimize);
  std::chrono::high_resolution_clock::time_point start();
  int64_t end(std::chrono::high_resolution_clock::time_point start);

  const IRExports& _exports;
  const char* _arg0;
  int _loglevel;
};

#endif