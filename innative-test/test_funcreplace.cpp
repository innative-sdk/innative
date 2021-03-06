// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <signal.h>
#include <setjmp.h>

using namespace innative;

int replacement(int a, int b) { return a + b; }
int TestHarness::do_funcreplace(void* assembly)
{
  constexpr int i = 4;
  constexpr int j = 2;

  int (*test)(int, int) = (int (*)(int, int))(*_exports.LoadFunction)(assembly, "funcreplace", "test");
  TEST(test != nullptr);

  int err = (*_exports.ReplaceTableFuncPtr)(assembly, 0, 0, "replace_me", (IN_Entrypoint)&replacement);

  if(test)
    TEST((*test)(i, j) == 6);

  return err;
}

void TestHarness::test_funcreplace() { TESTERR(CompileWASM("../scripts/funcreplace.wasm", &TestHarness::do_funcreplace, "env"), ERR_SUCCESS); }
