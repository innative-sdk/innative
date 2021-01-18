// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"

#ifdef IN_PLATFORM_WIN32
  #pragma pack(push)
  #pragma pack(8)
  #define WINVER        0x0600 // Vista
  #define _WIN32_WINNT  0x0600
  #define NTDDI_VERSION 0x06000000 // Vista
  #define WIN32_LEAN_AND_MEAN
  #ifndef NOMINMAX // Some compilers enable this by default
    #define NOMINMAX
  #endif
  #define NODRAWTEXT
  #define NOBITMAP
  #define NOMCX
  #define NOSERVICE
  #define NOHELP
  #define NOGDI
  #include <windows.h>
  #pragma pack(pop)
#endif

using namespace innative;
extern void JIT_DUMP(Environment* env);

#ifdef IN_DEBUG
  #define TEST_EMBEDDING "innative-test-embedding-d" IN_STATIC_EXTENSION
#else
  #define TEST_EMBEDDING "innative-test-embedding" IN_STATIC_EXTENSION
#endif

void TestHarness::test_jit()
{
  // JIT not supported on 32-bit windows
#if !(defined(IN_PLATFORM_WIN32) && defined(IN_32BIT))
  {
    // JIT works like a standard environment, but uses a different compilation function.
    auto env = (*_exports.CreateEnvironment)(1, 0, 0);
    TEST(env);
    if(!env)
      return;

    env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY;
    env->optimize = ENV_OPTIMIZE_O3;
    env->features = ENV_FEATURE_ALL;
    env->loghook  = &TestHarness::Log;
    env->loglevel = _loglevel;

  #ifdef IN_DEBUG
    env->flags |= ENV_DEBUG;
    env->optimize = ENV_OPTIMIZE_O0;
  #endif

    int err;

    // Add our default environment just like normal, but we have to explicitly tag it so the JIT knows how to load it.
    err = (*_exports.AddEmbedding)(env, IN_TAG_STATIC, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
    TESTERR(err, ERR_SUCCESS);

  #ifdef IN_PLATFORM_WIN32
    HMODULE handle = GetModuleHandleA("kernel32.dll");
    wchar_t sysdll[MAX_PATH];
    GetModuleFileNameW(handle, sysdll, MAX_PATH);
    auto syspath = path(sysdll).u8string();
    // Add kernel32
    err = (*_exports.AddEmbedding)(env, IN_TAG_DYNAMIC, (void*)syspath.c_str(), 0, 0);
    TESTERR(err, ERR_SUCCESS);
  #endif

    path file = "../scripts/debugging.wasm";
    if(err >= 0)
      (*_exports.AddModule)(env, file.u8string().c_str(), 0, file.stem().u8string().c_str(), &err);

    if(err >= 0)
      err = (*_exports.FinalizeEnvironment)(env);

    // JIT compile our script using the standard static library
    if(err >= 0)
      err = (*_exports.CompileJIT)(env, false);
    TESTERR(err, ERR_SUCCESS);

    constexpr int n = 8;
    auto test       = (int (*)(int))(*_exports.LoadFunctionJIT)(env, "debugging", "debug");
    TEST(test != nullptr);

    if(test)
      TEST((*test)(n) == 84);

    (*_exports.DestroyEnvironment)(env);
  }

  // This doesn't make sense on linux
  #ifdef IN_PLATFORM_WIN32
  {
    // Test building a DLL and loading it directly via JIT
    TEST(CompileWASM(
           "../scripts/embedded.wat", 0, "env",
           [&](Environment* env) -> int {
             int err = (*_exports.AddWhitelist)(env, "env", "my_factorial");
             TEST(!err);

             err = (*_exports.AddEmbedding)(env, 0, TEST_EMBEDDING, 0, 0);
             TEST(!err);

             return err;
           },
           "embedded", "jit_embedded") == ERR_SUCCESS);

    _garbage.pop_back(); // We can't delete the JIT loaded DLL
    _garbage.pop_back();

    auto env = (*_exports.CreateEnvironment)(1, 0, 0);
    TEST(env);
    if(!env)
      return;

    env->flags    = ENV_ENABLE_WAT | ENV_LIBRARY;
    env->optimize = ENV_OPTIMIZE_O3;
    env->features = ENV_FEATURE_ALL;
    env->loghook  = &TestHarness::Log;
    env->loglevel = _loglevel;
    env->system   = "env"; // Make sure we set this to env

    auto lib = _out.generic_u8string();
    int err  = (*_exports.AddEmbedding)(env, IN_TAG_STATIC, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);

    if(err >= 0)
      err = (*_exports.AddEmbedding)(env, IN_TAG_DYNAMIC, lib.c_str(), 0, 0);

    if(err >= 0)
      (*_exports.AddModule)(env, "(module)", 9, "testmod999", &err);

    if(err >= 0)
      err = (*_exports.FinalizeEnvironment)(env);

    if(err >= 0)
      err = (*_exports.CompileJIT)(env, false);

    TEST(!err);
    if(err >= 0)
    {
      int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunctionJIT)(env, "embedded", "test");
      TEST(test != nullptr);

      if(test)
        TEST((*test)(4, 2) == 26);
    }

    (*_exports.DestroyEnvironment)(env);
  }
  #endif

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
    env->loghook  = &TestHarness::Log;
    env->loglevel = _loglevel;

  #ifdef IN_DEBUG
    env->flags |= ENV_DEBUG;
    env->optimize = ENV_OPTIMIZE_O0;
  #endif

    int err = (*_exports.AddEmbedding)(env, IN_TAG_STATIC, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
    TESTERR(err, ERR_SUCCESS);

  #ifdef IN_PLATFORM_WIN32
    HMODULE handle = GetModuleHandleA("kernel32.dll");
    wchar_t sysdll[MAX_PATH];
    GetModuleFileNameW(handle, sysdll, MAX_PATH);
    auto syspath = path(sysdll).u8string();
    // Add kernel32
    err = (*_exports.AddEmbedding)(env, IN_TAG_DYNAMIC, (void*)syspath.c_str(), 0, 0);
    TESTERR(err, ERR_SUCCESS);
  #endif

    path file = "../scripts/debugging.wasm";
    if(err >= 0)
      (*_exports.AddModule)(env, file.u8string().c_str(), 0, file.stem().u8string().c_str(), &err);

    if(err >= 0)
      err = (*_exports.FinalizeEnvironment)(env);

    // JIT compile our script, exposing this process to it to it can access the static library
    if(err >= 0)
      err = (*_exports.CompileJIT)(env, false);
    TESTERR(err, ERR_SUCCESS);

    constexpr int n = 8;
    auto test       = (int (*)(int))(*_exports.LoadFunctionJIT)(env, "debugging", "debug");
    auto init       = (void (*)())(*_exports.LoadFunctionJIT)(env, 0, IN_INIT_FUNCTION);
    auto exit       = (void (*)())(*_exports.LoadFunctionJIT)(env, 0, IN_EXIT_FUNCTION);
    TEST(init != nullptr);
    TEST(test != nullptr);
    TEST(exit != nullptr);

    if(init && exit && test)
    {
      init();
      TEST((*test)(n) == 84);
      // Then clean up before destroying the environment
      exit();
    }

    (*_exports.DestroyEnvironment)(env);
  }
#endif
}
