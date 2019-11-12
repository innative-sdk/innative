// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/util.h"

using namespace innative;

#ifdef IN_DEBUG
#define TEST_EMBEDDING "innative-test-embedding-d" IN_STATIC_EXTENSION
#else
#define TEST_EMBEDDING "innative-test-embedding" IN_STATIC_EXTENSION
#endif

void TestHarness::test_embedding()
{
  auto fn = [this](const char* embed, size_t sz) {
    constexpr int i                  = 4;
    constexpr int j                  = 2;
    constexpr const char dll_path[]  = "embedded.dll";
    constexpr const char wasm_path[] = "../scripts/embedded.wat";

    Environment* env = (*_exports.CreateEnvironment)(1, 0, 0);
    env->flags |= ENV_LIBRARY | ENV_ENABLE_WAT;
    env->system = "env";

    int err = (*_exports.AddWhitelist)(env, "env", "my_factorial");
    TEST(!err);

    err = (*_exports.AddEmbedding)(env, 0, embed, sz);
    TEST(!err);
    err = (*_exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);
    TEST(!err);

    (*_exports.AddModule)(env, wasm_path, 0, "embedded", &err);
    TEST(!err);
    err = (*_exports.FinalizeEnvironment)(env);
    TEST(!err);

    err = (*_exports.Compile)(env, dll_path);
    TEST(!err);

    (*_exports.DestroyEnvironment)(env);

    void* assembly        = (*_exports.LoadAssembly)(dll_path);
    int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "embedded", "test");
    TEST(test != nullptr);

    if(test)
      TEST((*test)(i, j) == 26);

    (*_exports.FreeAssembly)(assembly);
  };

  fn(TEST_EMBEDDING, 0);

  long embedsz;
  auto embedfile = utility::LoadFile(TEST_EMBEDDING, embedsz);

  fn((const char*)embedfile.get(), embedsz);
}