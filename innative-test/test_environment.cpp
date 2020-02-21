// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"

extern "C" {
extern void _innative_internal_env_memcpy(char* dest, const char* src, uint64_t sz);
extern void* _innative_internal_env_grow_memory(void* p, uint64_t i, uint64_t max, uint64_t* size);
extern void _innative_internal_env_print(uint64_t a);
}

void TestHarness::test_environment()
{
  // We've linked ourselves with the innative_env library file, so we can directly test these functions
  _innative_internal_env_memcpy(0, 0, 0);

  {
    char src[1]  = { 14 };
    char dest[1] = { 0 };
    _innative_internal_env_memcpy(dest, src, 0);
    TEST(dest[0] == 0);
  }

  {
    char src[1]  = { 14 };
    char dest[1] = { 0 };
    _innative_internal_env_memcpy(dest, src, 1);
    TEST(dest[0] == 14);
  }

  {
    char src[8]  = { 14, 5, 5, 5, 5, 5, 5, 5 };
    char dest[8] = { 0 };
    _innative_internal_env_memcpy(dest, src, 1);
    TEST(dest[0] == 14);
    for(int i = 1; i < 8; ++i)
      TEST(!dest[i]);
  }

  char src[64];
  char dest[64];

  for(int n = 0; n < 64; ++n)
  {
    for(int i = 0; i < 64; ++i)
    {
      src[i]  = i;
      dest[i] = 0;
    }

    _innative_internal_env_memcpy(dest, src, n);

    for(int i = 0; i < n; ++i)
      TEST(dest[i] == src[i]);

    for(int i = n; i < 64; ++i)
      TEST(!dest[i]);
  }

  uint64_t sz = 0;
  uint64_t* p = (uint64_t*)_innative_internal_env_grow_memory(0, 0, 0, &sz);
  TEST(p != 0);
  p = (uint64_t*)_innative_internal_env_grow_memory(0, 0, 0, 0);
  TEST(!_innative_internal_env_grow_memory(0, 9, 1, &sz));
  TEST(!_innative_internal_env_grow_memory(p, 9, 1, &sz));
  p = (uint64_t*)_innative_internal_env_grow_memory(p, 1, 1, &sz);
  TEST(p != 0);
  TEST(sz == 1);
  TEST(reinterpret_cast<char*>(p)[0] == 0);
  TEST(!_innative_internal_env_grow_memory(p, 1, 1, &sz));
  p = (uint64_t*)_innative_internal_env_grow_memory(p, 1, 2, &sz);
  TEST(p != 0);
  TEST(sz == 2);
  TEST(reinterpret_cast<char*>(p)[0] == 0);
  TEST(reinterpret_cast<char*>(p)[1] == 0);
  p = (uint64_t*)_innative_internal_env_grow_memory(p, 1000, 0, &sz);
  TEST(p != 0);
  TEST(sz == 1002);
  for(int i = 0; i < 1000; ++i)
    TEST(!reinterpret_cast<char*>(p)[i]);

  p = (uint64_t*)_innative_internal_env_grow_memory(p, 100000, 0, &sz);
  TEST(p != 0);
  TEST(sz == 101002);
  TEST(!_innative_internal_env_grow_memory(p, 100000, 200000, &sz));
}