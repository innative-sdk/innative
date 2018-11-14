// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/stack.h"

using namespace innative;

void TestHarness::test_stack()
{
  Stack<int> s;
  TEST(!s.Capacity());
  TEST(!s.Limit());
  TEST(!s.Size());
  TEST(s.Size());
}