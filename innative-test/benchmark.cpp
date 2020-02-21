// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "benchmark.h"
#include <chrono>

Benchmarks::Benchmarks(const INExports& exports, const char* arg0, int loglevel, const path& folder) :
  _exports(exports), _arg0(arg0), _loglevel(loglevel), _folder(folder)
{}
Benchmarks::~Benchmarks()
{
  // Clean up all the files we just produced
  for(auto f : _garbage)
    remove(f.c_str());
}

void Benchmarks::Run(FILE* out)
{
  static constexpr int COLUMNS[6] = { 24, 11, 11, 11, 11, 11 };
  fprintf(out, "%-*s %-*s %-*s %-*s %-*s %-*s\n", COLUMNS[0], "Benchmark", COLUMNS[1], "C/C++", COLUMNS[2], "Debug",
          COLUMNS[3], "Strict", COLUMNS[4], "Sandbox", COLUMNS[5], "Native");
  fprintf(out, "%-*s %-*s %-*s %-*s %-*s %-*s\n", COLUMNS[0], "---------", COLUMNS[1], "-----", COLUMNS[2], "-----",
          COLUMNS[3], "------", COLUMNS[4], "-------", COLUMNS[5], "------");

  DoBenchmark<int, int>(out, "../scripts/benchmark_n-body.wasm", "nbody", COLUMNS, &Benchmarks::nbody, 11);
  DoBenchmark<int64_t, int64_t>(out, "../scripts/benchmark_fib.wasm", "fib", COLUMNS, &Benchmarks::fib, 37);
  DoBenchmark<int, int>(out, "../scripts/benchmark_fannkuch-redux.wasm", "fannkuch_redux", COLUMNS,
                        &Benchmarks::fannkuch_redux, 11);
}

void* Benchmarks::LoadWASM(const path& wasm, const char* name, int flags, int optimize)
{
  static int counter = 0; // We must gaurantee all file names are unique because windows never unloads DLLs properly
  ++counter;
  Environment* env = (*_exports.CreateEnvironment)(1, 0, _arg0);
  if(!env)
    return 0;

  env->flags    = flags | ENV_ENABLE_WAT | ENV_LIBRARY;
  env->optimize = optimize;
  env->features = ENV_FEATURE_ALL;
  env->log      = stdout;
  env->loglevel = _loglevel;

  int err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);

  if(err < 0)
  {
    (*_exports.DestroyEnvironment)(env);
    return 0;
  }

  (*_exports.AddModule)(env, wasm.u8string().c_str(), 0, name, &err);
  if(err < 0)
  {
    (*_exports.DestroyEnvironment)(env);
    return 0;
  }

  (*_exports.FinalizeEnvironment)(env);
  path base = _folder / wasm.stem();
  base += std::to_string(counter);
  path out = base;
  out.replace_extension(IN_LIBRARY_EXTENSION);

  err = (*_exports.Compile)(env, out.u8string().c_str());
  (*_exports.DestroyEnvironment)(env);

  if(err < 0)
    return 0;

  _garbage.push_back(out);
#ifdef IN_PLATFORM_WIN32
  base.replace_extension(".lib");
  _garbage.push_back(base);
  if(flags & ENV_DEBUG)
    base.replace_extension(".pdb");
  _garbage.push_back(base);
#endif
  void* m = (*_exports.LoadAssembly)(out.u8string().c_str());

  return m;
}

std::chrono::high_resolution_clock::time_point Benchmarks::start() { return std::chrono::high_resolution_clock::now(); }

int64_t Benchmarks::end(std::chrono::high_resolution_clock::time_point start)
{
  return std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - start).count();
}