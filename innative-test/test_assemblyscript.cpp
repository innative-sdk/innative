// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

#ifdef IN_DEBUG
  #define TEST_EMBEDDING "innative-assemblyscript-d" IN_STATIC_EXTENSION
#else
  #define TEST_EMBEDDING "innative-assemblyscript" IN_STATIC_EXTENSION
#endif

#ifdef IN_PLATFORM_POSIX
  #define LONGJMP(x, i) siglongjmp(x, i)
  #define SETJMP(x)     sigsetjmp(x, 1)
#else
  #define LONGJMP(x, i) longjmp(x, i)
  #define SETJMP(x)     setjmp(x)

  #include "../innative/win32.h"
#endif

jmp_buf jump_location;

void TestCrashHandler(int sig) { LONGJMP(jump_location, 1); }

void TestHarness::test_assemblyscript()
{
  auto fn = [this](const char* embed, size_t sz) {
    constexpr int i                  = 4;
    constexpr int j                  = 2;
    path dll_path                    = _folder / "astest" IN_LIBRARY_EXTENSION;
    constexpr const char wasm_path[] = "../scripts/assemblyscript.wasm";

    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags |= ENV_LIBRARY;
    env->system = "env";
#ifdef IN_DEBUG
    env->optimize = ENV_OPTIMIZE_O0;
    env->flags |= ENV_DEBUG;
#endif

    int err = (*_exports.AddWhitelist)(env, "env", "trace");
    err     = (*_exports.AddWhitelist)(env, "env", "abort");
    TEST(!err);

    err = (*_exports.AddEmbedding)(env, 0, embed, sz, 0);
    TEST(!err);
    err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
    TEST(!err);

    (*_exports.AddModule)(env, wasm_path, 0, "astest", &err);
    TEST(!err);
    err = (*_exports.FinalizeEnvironment)(env);
    TEST(!err);

    err = (*_exports.Compile)(env, dll_path.u8string().c_str());
    TEST(!err);

    (*_exports.DestroyEnvironment)(env);

    bool caught = false;
    void* assembly;

    signal(SIGILL, TestCrashHandler);

    if(SETJMP(jump_location) != 0)
      caught = true;
    else
    {
#ifdef IN_COMPILER_MSC
      // On windows, signals can sometimes get promoted to SEH exceptions across DLL bounderies.
      __try
      {
#endif
        assembly = (*_exports.LoadAssembly)(dll_path.u8string().c_str());
#ifdef IN_COMPILER_MSC
      }
      __except(1) // Only catch an illegal instruction
      {
        caught = true;
      }
#endif
    }

    TEST(caught);
    signal(SIGILL, SIG_DFL);

    if(assembly)
      (*_exports.FreeAssembly)(assembly);

    remove(dll_path);
  };

  fn(TEST_EMBEDDING, 0);

  size_t embedsz   = 0;
  auto embedfile = utility::LoadFile(TEST_EMBEDDING, embedsz);

  fn((const char*)embedfile.get(), embedsz);
}
