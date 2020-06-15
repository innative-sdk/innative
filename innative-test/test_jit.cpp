// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"

using namespace innative;
extern void JIT_DUMP(Environment* env);

void TestHarness::test_jit()
{
  {
    // JIT works like a standard environment, but can't load static libraries and uses a different compilation function.
    auto env = (*_exports.CreateEnvironment)(1, 0, 0);
    TEST(env);
    if(!env)
      return;

    env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY;
    env->optimize = ENV_OPTIMIZE_O3;
    env->features = ENV_FEATURE_ALL;
    env->log      = stdout;
    env->loglevel = _loglevel;

#ifdef IN_DEBUG
    env->flags |= ENV_DEBUG;
    env->optimize = ENV_OPTIMIZE_O0;
#endif

    // Add our default environment just like normal, but we have to explicitly tag it so the JIT knows how to load it.
    int err = (*_exports.AddEmbedding)(env, IN_TAG_STATIC, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);

    path file = "../scripts/debugging.wasm";
    if(err >= 0)
      (*_exports.AddModule)(env, file.u8string().c_str(), 0, file.stem().u8string().c_str(), &err);

    if(err >= 0)
      err = (*_exports.FinalizeEnvironment)(env);

    // JIT compile our script using the standard static library
    if(err >= 0)
      err = (*_exports.CompileJIT)(env, false);

    constexpr int n = 8;
    auto test       = (int (*)(int))(*_exports.LoadFunctionJIT)(env, "debugging", "debug");
    TEST(test != nullptr);

    if(test)
      TEST((*test)(n) == 83);

    (*_exports.DestroyEnvironment)(env);
  }

  {
    // Normally, the JIT initializes everything when you call CompilerJIT and cleans up during DestroyEnvironment.
    // If you use ENV_NO_INIT, you are responsible for initialization and cleanup instead.
    auto env = (*_exports.CreateEnvironment)(1, 0, 0);
    TEST(env);
    if(!env)
      return;

    env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY | ENV_NO_INIT;
    env->optimize = ENV_OPTIMIZE_O3;
    env->features = ENV_FEATURE_ALL;
    env->log      = stdout;
    env->loglevel = _loglevel;

#ifdef IN_DEBUG
    env->flags |= ENV_DEBUG;
    env->optimize = ENV_OPTIMIZE_O0;
#endif

    int err   = (*_exports.AddEmbedding)(env, IN_TAG_STATIC, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);

    path file = "../scripts/debugging.wasm";
    if(err >= 0)
      (*_exports.AddModule)(env, file.u8string().c_str(), 0, file.stem().u8string().c_str(), &err);

    if(err >= 0)
      err = (*_exports.FinalizeEnvironment)(env);

    // JIT compile our script, exposing this process to it to it can access the static library
    if(err >= 0)
      err = (*_exports.CompileJIT)(env, false);

    constexpr int n = 8;
    auto test       = (int (*)(int))(*_exports.LoadFunctionJIT)(env, "debugging", "debug");
    auto init       = (void (*)())(*_exports.LoadFunctionJIT)(env, 0, IN_INIT_FUNCTION);
    auto exit       = (void (*)())(*_exports.LoadFunctionJIT)(env, 0, IN_EXIT_FUNCTION);
    TEST(test != nullptr);

    init();

    if(test)
      TEST((*test)(n) == 83);

    // Then clean up before destroying the environment
    exit();

    (*_exports.DestroyEnvironment)(env);
  }
}