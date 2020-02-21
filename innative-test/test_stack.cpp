// Copyright (c)2020 Black Sphere Studios
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

  s.Reserve(1);
  TEST(s.Capacity() == 1);
  TEST(!s.Limit());
  TEST(!s.Size());

  s.Push(3);
  TEST(s.Capacity() == 1);
  TEST(!s.Limit());
  TEST(s.Size() == 1);
  TEST(s.Peek() == 3);

  s.SetLimit(1);
  TEST(s.Limit() == 1);
  TEST(s.Size() == 0);

  s.Push(5);
  TEST(s.Limit() == 1);
  TEST(s.Size() == 1);
  TEST(s.Peek() == 5);
  TEST(s.Pop() == 5);
  TEST(s.Size() == 0);

  s.SetLimit(0);
  TEST(s.Size() == 1);
  TEST(s.Limit() == 0);
  TEST(s.Peek() == 3);
  TEST(s.Pop() == 3);

  s.Push(3);
  s.Push(5);
  s.Push(7);

  TEST(s.Size() == 3);
  TEST(s.Pop() == 7);
  TEST(s.Pop() == 5);
  TEST(s.Pop() == 3);
}