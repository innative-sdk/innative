// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

#define TEST_EMBEDDING "innative-assemblyscript"

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

bool isolate_call(IN_Entrypoint start, IN_Entrypoint cleanup)
{
  bool caught = false;
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
      (*start)();
#ifdef IN_COMPILER_MSC
    }
    __except(GetExceptionCode() == EXCEPTION_ILLEGAL_INSTRUCTION) // Only catch an illegal instruction
    {
      caught = true;
    }
#endif
  }
  if(cleanup)
    (*cleanup)();

  signal(SIGILL, SIG_DFL);
  return caught;
}

void TestHarness::test_assemblyscript()
{
  auto fn = [this](const char* embed, size_t sz) {
    constexpr int i                  = 4;
    constexpr int j                  = 2;
    path dll_path                    = _folder / "astest" IN_LIBRARY_EXTENSION;
    constexpr const char wasm_path[] = "../scripts/assemblyscript.wasm";

    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags |= ENV_LIBRARY | ENV_NO_INIT;
    env->system = "env";
#ifdef IN_DEBUG
    env->optimize = ENV_OPTIMIZE_O0;
    env->flags |= ENV_DEBUG;
#endif

    int err = (*_exports.AddWhitelist)(env, "env", "trace");
    err     = (*_exports.AddWhitelist)(env, "env", "abort");
    TESTERR(err, ERR_SUCCESS);

    err = (*_exports.AddEmbedding)(env, 0, embed, sz, 0);
    TESTERR(err, ERR_SUCCESS);
    err = (*_exports.AddEmbedding)(env, 0, (void*)(*_exports.GetDefaultEmbedding)(TestHarness::Debug), 0, 0);
    TESTERR(err, ERR_SUCCESS);

    (*_exports.AddModule)(env, wasm_path, 0, "astest", &err);
    TESTERR(err, ERR_SUCCESS);
    if(err < 0)
      return;

    err = (*_exports.FinalizeEnvironment)(env);
    TESTERR(err, ERR_SUCCESS);

    err = (*_exports.Compile)(env, dll_path.u8string().c_str());
    TESTERR(err, ERR_SUCCESS);

    (*_exports.DestroyEnvironment)(env);

    void* assembly        = LoadAssembly(dll_path);
    IN_Entrypoint start   = (*_exports.LoadFunction)(assembly, 0, IN_INIT_FUNCTION);
    IN_Entrypoint cleanup = (*_exports.LoadFunction)(assembly, 0, IN_EXIT_FUNCTION);
    TEST(start);
    if(start)
    {
      auto caught = isolate_call(start, cleanup);
      TEST(caught);
    }

    if(assembly)
      (*_exports.FreeAssembly)(assembly);

    remove(dll_path);
  };

  fn(TEST_EMBEDDING IN_STATIC_EXTENSION, 0);

  std::string embedpath;
  embedpath.resize((*_exports.GetEmbeddingPath)(CURRENT_ABI, CURRENT_ARCH, false, TEST_EMBEDDING, nullptr, 0));
  embedpath.resize(
    (*_exports.GetEmbeddingPath)(CURRENT_ABI, CURRENT_ARCH, false, TEST_EMBEDDING, embedpath.data(), embedpath.capacity()));

  size_t embedsz = 0;
  auto embedfile = utility::LoadFile(embedpath, embedsz);

  fn((const char*)embedfile.get(), embedsz);
}
