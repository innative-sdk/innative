// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/stream.h"

using namespace innative;

void TestHarness::test_stream()
{
  uint8_t buf[]     = { 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 9 };
  utility::Stream s = { buf, sizeof(buf), 0 };

  TEST(!s.End());
  TEST(s.Get() == 1);
  TEST(s.Get() == 2);
  uint8_t a = 0;
  TEST(s.Read(a));
  TEST(a == 3);
  IN_ERROR err;
  TEST(s.ReadByte(err) == 4);
  TEST(s.ReadVarInt7(err) == 5);
  TEST(s.ReadVarUInt7(err) == 6);
  uint8_t target[8] = { 0 };
  TEST(s.ReadBytes(target, 2) == 2);
  TEST(target[0] == 7);
  TEST(target[1] == 8);
  TEST(target[2] == 0);
  TEST(s.ReadVarUInt1(err) == false);
  TEST(!s.End());

  s.pos = 0;
  TEST(s.ReadVarUInt1(err) == true);
  s.pos = 0;
  TEST(s.ReadBytes(target, 8) == 8);
  for(int i = 0; i < 8; ++i)
    TEST(target[i] == buf[i]);
  TEST(s.ReadVarUInt1(err) == false);
  TEST(s.ReadBytes(target, 8) == 7);
  for(int i = 0; i < 7; ++i)
    TEST(target[i] == buf[i + 9]);
  TEST(target[7] == 8);
  TEST(s.End());
  s.pos = 15;
  int b = 99;
  TEST(!s.Read(b));
  TEST(s.End());
  TEST(b == 99);
  s.pos = 15;
  TEST(!s.End());
  TEST(s.ReadVarUInt32(err) == 9);
}