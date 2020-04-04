// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

int TestHarness::do_debug(void* assembly)
{
  constexpr int n = 8;

  auto test = (int (*)(int))(*_exports.LoadFunction)(assembly, "debugging", "debug");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(n) == 83);

  return ERR_SUCCESS;
}

int TestHarness::do_debug_2(void* assembly)
{
  constexpr long long n = 8008;

  auto test = (long long (*)(long long))(*_exports.LoadFunction)(assembly, "constparse", "test");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(n) == (n - 9218868437227405313));

  return ERR_SUCCESS;
}

  void TestHarness::test_debug()
{
  int flags   = 0;
  auto lambda = [&](Environment* env) -> int {
    env->flags &= ~ENV_DEBUG;
    env->flags |= flags;
    return ERR_SUCCESS;
  };

  flags = ENV_DEBUG_PDB;
  TEST(CompileWASM("../scripts/debugging.wasm", &TestHarness::do_debug, "env", lambda) == ERR_SUCCESS);

  flags = ENV_DEBUG_DWARF;
  TEST(CompileWASM("../scripts/debugging.wasm", &TestHarness::do_debug, "env", lambda) == ERR_SUCCESS);

  TEST(CompileWASM("../scripts/constparse.wasm", &TestHarness::do_debug_2, "") == ERR_SUCCESS);
}
