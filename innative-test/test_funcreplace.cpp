// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/util.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

int replacement(int a, int b) { return a + b; }

void TestHarness::test_funcreplace()
{
  constexpr int i                  = 4;
  constexpr int j                  = 2;
  path dll_path                    = _folder / "funcreplace" IN_LIBRARY_EXTENSION;
  constexpr const char wasm_path[] = "../scripts/funcreplace.wasm";

  Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
  env->flags |= ENV_LIBRARY | ENV_ENABLE_WAT;
  env->system = "env";
#ifdef IN_DEBUG
  env->optimize = ENV_OPTIMIZE_O0;
  env->flags |= ENV_DEBUG;
#endif

  int err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0, 0);
  TEST(!err);

  (*_exports.AddModule)(env, wasm_path, 0, "funcreplace", &err);
  TEST(!err);
  err = (*_exports.FinalizeEnvironment)(env);
  TEST(!err);

  err = (*_exports.Compile)(env, dll_path.u8string().c_str());
  TEST(!err);

  (*_exports.DestroyEnvironment)(env);

  void* assembly = (*_exports.LoadAssembly)(dll_path.u8string().c_str());
  if(assembly)
  {
    int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "funcreplace", "test");
    TEST(test != nullptr);

    (*_exports.ReplaceTableFuncPtr)(assembly, 0, 0, "replace_me", (IN_Entrypoint)&replacement);

    if(test)
      TEST((*test)(i, j) == 6);

    (*_exports.FreeAssembly)(assembly);
  }

  remove(dll_path);
}
