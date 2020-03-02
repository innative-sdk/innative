// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"

using namespace innative;

#ifdef IN_DEBUG
  #define TEST_EMBEDDING "innative-test-embedding-d" IN_STATIC_EXTENSION
#else
  #define TEST_EMBEDDING "innative-test-embedding" IN_STATIC_EXTENSION
#endif
int TestHarness::do_embedding(void* assembly)
{
  constexpr int i = 4;
  constexpr int j = 2;

  int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "embedded", "test");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(i, j) == 26);

  return ERR_SUCCESS;
}

void TestHarness::test_embedding()
{
  const char* embed = TEST_EMBEDDING;
  size_t embedsz    = 0;
  const char* sys   = "env";
  auto lambda       = [&](Environment* env) -> int {
    int err = (*_exports.AddWhitelist)(env, "env", "my_factorial");
    TEST(!err);

    err = (*_exports.AddEmbedding)(env, 0, embed, embedsz, sys);
    TEST(!err);

    return err;
  };

  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) == ERR_SUCCESS);
  sys = "env2";
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) != ERR_SUCCESS);
  sys = "";
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "", lambda) != ERR_SUCCESS);

  sys = 0;
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "env", lambda) == ERR_SUCCESS);

  auto embedfile = utility::LoadFile(TEST_EMBEDDING, embedsz);
  embed          = (const char*)embedfile.get();
  TEST(CompileWASM("../scripts/embedded.wat", &TestHarness::do_embedding, "env", lambda) == ERR_SUCCESS);
}