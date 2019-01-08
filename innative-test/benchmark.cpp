// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "benchmark.h"
#include <chrono>

Benchmarks::Benchmarks(const IRExports& exports, const char* arg0, int loglevel) : _exports(exports), _arg0(arg0), _loglevel(loglevel) {}

void* Benchmarks::LoadWASM(const char* wasm, int flags, int optimize)
{
  Environment* env = (*_exports.CreateEnvironment)(1, 0, _arg0);
  env->flags = flags | ENV_ENABLE_WAT | ENV_LIBRARY;
  env->optimize = optimize;
  env->features = ENV_FEATURE_ALL;
  env->log = stdout;
  env->loglevel = _loglevel;
  int err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);
  if(err < 0)
    return 0;

  (*_exports.AddModule)(env, wasm, 0, wasm, &err);
  if(err < 0)
    return 0;

  (*_exports.WaitForLoad)(env);
  std::string out(wasm);
  out += ".dll";
  err = (*_exports.Compile)(env, out.c_str());
  if(err < 0)
    return 0;

  std::remove((std::string(wasm) + ".pdb").c_str()); // Don't allow visual studio to break itself
  void* m = (*_exports.LoadAssembly)(out.c_str());
  (*_exports.DestroyEnvironment)(env);

  return m;
}
std::chrono::high_resolution_clock::time_point Benchmarks::start()
{
  return std::chrono::high_resolution_clock::now();
}
int64_t Benchmarks::end(std::chrono::high_resolution_clock::time_point start)
{
  return std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - start).count();
}