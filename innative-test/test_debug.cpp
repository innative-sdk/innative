// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/util.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

int TestHarness::do_debug(void* assembly)
{
  constexpr int n = 8;

  int (*test)(int) = (int (*)(int))(*_exports.LoadFunction)(assembly, "debugging", "debug");
  TEST(test != nullptr);

  if(test)
    TEST((*test)(n) == 83);

  return ERR_SUCCESS;
}

void TestHarness::test_debug() { CompileWASM("../scripts/debugging.wasm", &TestHarness::do_debug, "env"); }
