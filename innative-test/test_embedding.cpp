// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#define TEST_EMBEDDING "innative-test-embedding"

using namespace innative;

void TestHarness::test_embedding()
{
  int i                            = 4;
  int j                            = 2;
  constexpr const char dll_path[]  = "broken_module.dll";
  constexpr const char wasm_path[] = "../scripts/broken_module.wasm";

  Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
  env->flags |= ENV_LIBRARY;
  env->system   = "env";

  int err = (*_exports.AddWhitelist)(env, "env", "my_factorial");
  TEST(!err);
#ifdef IN_DEBUG
  err = (*_exports.AddEmbedding)(env, 0, TEST_EMBEDDING "-d" IN_STATIC_EXTENSION, 0);
#else
  err = (*_exports.AddEmbedding)(env, 0, TEST_EMBEDDING IN_STATIC_EXTENSION, 0);
#endif
  TEST(!err);
  err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);
  TEST(!err);

  (*_exports.AddModule)(env, wasm_path, 0, "module", &err);
  TEST(!err);
  err = (*_exports.FinalizeEnvironment)(env);
  TEST(!err);

  err = (*_exports.Compile)(env, dll_path);
  TEST(!err);

  (*_exports.DestroyEnvironment)(env);

  void* assembly        = (*_exports.LoadAssembly)(dll_path);
  int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "module", "test");

  TEST((*test)(i, j) == 26);
  (*_exports.FreeAssembly)(assembly);
}