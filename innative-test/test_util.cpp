// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <functional>

using namespace innative;
using namespace utility;

void TestHarness::test_util()
{
  for(int i = 0; i < sizeof(IN_BASE64) - 1; ++i)
    TEST(BASE64[IN_BASE64[i]] == i);

  {
    StringSpan a = { nullptr, 0 };
    StringSpan b = { "asdf", 4 };
    TEST(!(a == b));
    TEST(a == a);
    TEST(b == b);
  }

  {
    ValidationError* list = nullptr;
    internal::ReverseErrorList(list);
    ValidationError c = { 3, 0, 0, 0 };
    list              = &c;
    internal::ReverseErrorList(list);
    TEST(list->code == 3);
    TEST(!list->next);
    ValidationError b = { 2, 0, 0, &c };
    list              = &b;
    TEST(list->code == 2);
    TEST(list->next->code == 3);
    TEST(!list->next->next);
    internal::ReverseErrorList(list);
    TEST(list->code == 3);
    TEST(list->next->code == 2);
    TEST(!list->next->next);
    ValidationError a = { 1, 0, 0, &c };
    list              = &a;
    TEST(list->code == 1);
    TEST(list->next->code == 3);
    TEST(list->next->next->code == 2);
    TEST(!list->next->next->next);
    internal::ReverseErrorList(list);
    TEST(list->code == 2);
    TEST(list->next->code == 3);
    TEST(list->next->next->code == 1);
    TEST(!list->next->next->next);
  }

  {
    uint8_t b[4] = { 1, 2, 3, 4 };
    internal::FlipEndian(b, 0);
    TEST(b[0] == 1);
    TEST(b[1] == 2);
    TEST(b[2] == 3);
    TEST(b[3] == 4);
    internal::FlipEndian<uint8_t>(b);
    TEST(b[0] == 1);
    TEST(b[1] == 2);
    TEST(b[2] == 3);
    TEST(b[3] == 4);
    internal::FlipEndian(b, 2);
    TEST(b[0] == 2);
    TEST(b[1] == 1);
    TEST(b[2] == 3);
    TEST(b[3] == 4);
    internal::FlipEndian(b, 3);
    TEST(b[0] == 3);
    TEST(b[1] == 1);
    TEST(b[2] == 2);
    TEST(b[3] == 4);
    internal::FlipEndian(b, 4);
    TEST(b[0] == 4);
    TEST(b[1] == 2);
    TEST(b[2] == 1);
    TEST(b[3] == 3);
  }

  {
    int b = 0;
    {
      DeferLambda<std::function<void()>> x([&]() { b = 1; });
    }
    TEST(b == 1);
    b = 1;
    {
      DeferLambda<std::function<void()>> x([&]() { b *= 10; });
      DeferLambda<std::function<void()>> y([&]() { b /= 2; });
      DeferLambda<std::function<void()>> z([&]() { b += 1; });
    }
    TEST(b == 10);
  }

  {
    tmemcpy<int>(0, 0, 0, 0);
    uint8_t a[4] = { 0, 0, 0, 0 };
    uint8_t b[4] = { 1, 2, 3, 4 };
    tmemcpy<uint16_t>((uint16_t*)a, 1, (uint16_t*)b, 1);
    TEST(a[0] == 1);
    TEST(a[1] == 2);
    tmemcpy<uint16_t>((uint16_t*)a, 2, (uint16_t*)b, 2);
    TEST(a[0] == 1);
    TEST(a[1] == 2);
    TEST(a[2] == 3);
    TEST(a[3] == 4);
  }

  {
    Module m = { 0 };
    TEST(!ModuleHasSection(m, 0));
    TEST(!ModuleHasSection(m, 31));
    m.knownsections = ~0;
    TEST(ModuleHasSection(m, 0));
    TEST(ModuleHasSection(m, 31));
    m.knownsections = 2;
    TEST(!ModuleHasSection(m, 0));
    TEST(ModuleHasSection(m, 1));
  }

  uintcpuinfo info = { 0 };
  GetCPUInfo(info, 0);
  TEST(info[4] != 0);
}