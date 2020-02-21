// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__BENCHMARK_H
#define IN__BENCHMARK_H

#include "test.h"
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
  Benchmarks(const INExports& exports, const char* arg0, int loglevel, const path& folder);
  ~Benchmarks();
  void Run(FILE* out);
  static int64_t fib(int64_t n);
  static int nbody(int n);
  static int fannkuch_redux(int n);
  static int debug(int n);
  static int minimum(int n);

  template<typename R, typename... Args>
  Timing DoBenchmark(FILE* out, const path& wasm, const char* func, const int (&COLUMNS)[6], R (*f)(Args...),
                     Args&&... args)
  {
    Timing timing;

    fprintf(out, "%-*s ", COLUMNS[0], func);
    timing.c = MeasureFunction<R, Args...>(f, std::forward<Args>(args)...);
#ifndef IN_DEBUG
    timing.c = MeasureFunction<R, Args...>(f, std::forward<Args>(args)...); // Do it again to account for CPU caching
#endif
    fprintf(out, "%-*lli ", COLUMNS[1], timing.c);
    timing.debug =
      MeasureWASM<R, Args...>(wasm, func, ENV_DEBUG | ENV_STRICT, ENV_OPTIMIZE_O0, std::forward<Args>(args)...);
    fprintf(out, "%-*lli ", COLUMNS[2], timing.debug);
    timing.strict = MeasureWASM<R, Args...>(wasm, func, ENV_STRICT, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);
    fprintf(out, "%-*lli ", COLUMNS[3], timing.strict);
    timing.sandbox = MeasureWASM<R, Args...>(wasm, func, ENV_SANDBOX, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);
    fprintf(out, "%-*lli ", COLUMNS[4], timing.sandbox);
    timing.native = MeasureWASM<R, Args...>(wasm, func, 0, ENV_OPTIMIZE_O3, std::forward<Args>(args)...);
    fprintf(out, "%-*lli ", COLUMNS[5], timing.native);
    fprintf(out, "\n%-*s %-*.2f %-*.2f %-*.2f %-*.2f %-*.2f\n", COLUMNS[0], "", COLUMNS[1], double(timing.c) / timing.c,
            COLUMNS[2], double(timing.c) / timing.debug, COLUMNS[3], double(timing.c) / timing.strict, COLUMNS[4],
            double(timing.c) / timing.sandbox, COLUMNS[5], double(timing.c) / timing.native);

    return timing;
  }

  template<typename R, typename... Args>
  int64_t MeasureWASM(const path& wasm, const char* func, int flags, int optimize, Args&&... args)
  {
    auto name = wasm.filename().replace_extension().string();
    void* m   = LoadWASM(wasm, name.c_str(), flags, optimize);
    assert(m != nullptr);
    R (*f)(Args...) = (R(*)(Args...))(*_exports.LoadFunction)(m, name.c_str(), func);
    assert(f != nullptr);
    int64_t t = MeasureFunction(f, std::forward<Args>(args)...);
    (*_exports.FreeAssembly)(m);
    return t;
  }

  template<typename R, typename... Args> int64_t MeasureFunction(R (*f)(Args...), Args&&... args)
  {
    auto t = start();
    f(std::forward<Args>(args)...);
    return end(t);
  }

protected:
  void* LoadWASM(const path& wasm, const char* name, int flags, int optimize);
  std::chrono::high_resolution_clock::time_point start();
  int64_t end(std::chrono::high_resolution_clock::time_point start);

  const INExports& _exports;
  const char* _arg0;
  int _loglevel;
  std::vector<path> _garbage;
  path _folder;
};

#endif