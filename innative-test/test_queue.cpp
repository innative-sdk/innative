// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/queue.h"

using namespace innative;

void TestHarness::test_queue()
{
  Queue<int> q;
  TEST(!q.Capacity());
  TEST(!q.Size());
  TEST(!q.GetPosition());
  q.SetPosition(0);
  TEST(!q.GetPosition());

  q.Reserve(1);
  TEST(q.Capacity() == 1);
  TEST(!q.Size());
  TEST(!q.GetPosition());

  q.Push(5);
  TEST(q[0] == 5);
  TEST(q.Back() == 5);
  TEST(q.Front() == 5);
  TEST(q.Peek() == 5);
  TEST(q.Size() == 1);
  TEST(q.Capacity() == 1);
  TEST(q.GetPosition() == 0);

  TEST(q.Pop() == 5);
  TEST(q.Size() == 0);
  TEST(q.Back() == 5);
  TEST(q.Front() == 5);
  TEST(q.GetPosition() == 1);
  q.SetPosition(1);
  TEST(q.Size() == 0);
  q.SetPosition(0);
  TEST(q.Back() == 5);
  TEST(q.Front() == 5);
  TEST(q.Peek() == 5);
  TEST(q.Size() == 1);
  TEST(q.Capacity() == 1);
  TEST(q.GetPosition() == 0);

  q.Push(7);
  TEST(q[0] == 5);
  TEST(q[1] == 7);
  TEST(q.Back() == 7);
  TEST(q.Front() == 5);
  TEST(q.Peek() == 5);
  TEST(q.Size() == 2);
  TEST(q.Capacity() > 1);
  TEST(q.GetPosition() == 0);

  TEST(q.Pop() == 5);
  TEST(q.GetPosition() == 1);
  TEST(q[0] == 7);
  TEST(q.Size() == 1);
  TEST(q.Pop() == 7);
  TEST(q.GetPosition() == 2);
  TEST(q.Size() == 0);
  q.SetPosition(1);
  TEST(q.Size() == 1);
  TEST(q.Pop() == 7);
  TEST(q.GetPosition() == 2);
  TEST(q.Size() == 0);

  q.Push(13);
  TEST(q[0] == 13);
  TEST(q.Back() == 13);
  TEST(q.Front() == 5);
  TEST(q.Size() == 1);
  TEST(q.Peek() == 13);
  TEST(q.GetPosition() == 2);
  TEST(q.Pop() == 13);
  q.SetPosition(0);

  TEST(q[0] == 5);
  TEST(q[1] == 7);
  TEST(q[2] == 13);
  TEST(q.Size() == 3);
  TEST(q.Pop() == 5);
  TEST(q[0] == 7);
  TEST(q[1] == 13);
  TEST(q.Size() == 2);
  TEST(q.Pop() == 7);
  TEST(q.Size() == 1);
  TEST(q[0] == 13);
  TEST(q.Pop() == 13);
  TEST(q.Size() == 0);
}