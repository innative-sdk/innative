// Copyright (c)2021 Fundament Software
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
    TEST((*test)(n) == 84);

  return ERR_SUCCESS;
}

int TestHarness::do_debug_2(void* assembly)
{
  constexpr long long n = 8008;

  auto counter = (uint64_t*)(*_exports.LoadGlobal)(assembly, 0, IN_INSTRUCTION_COUNTER);
  TEST(*counter == 0);
  auto test = (long long (*)(long long))(*_exports.LoadFunction)(assembly, "constparse", "test");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(n) == (n - 9218868437227405313));

  TEST(*counter > 0);

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
  TESTERR(CompileWASM("../scripts/debugging.wasm", &TestHarness::do_debug, "env", lambda), ERR_SUCCESS);
  flags = ENV_DEBUG_DWARF;
  TESTERR(CompileWASM("../scripts/debugging.wasm", &TestHarness::do_debug, "env", lambda), ERR_SUCCESS);

  flags = ENV_COUNT_INSTRUCTIONS;
  TESTERR(CompileWASM("../scripts/constparse.wasm", &TestHarness::do_debug_2, "", lambda), ERR_SUCCESS);
}
