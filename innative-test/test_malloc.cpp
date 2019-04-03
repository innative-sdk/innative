// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/util.h"
#include <functional>

using namespace innative;
using namespace utility;

static const int WASM_PAGE = 65536;
static void* wasm_buf = malloc(WASM_PAGE * 32);
static size_t wasm_end = 0;

extern "C" {
  size_t __builtin_wasm_memory_size(size_t memory)
  {
    return (size_t)wasm_buf + wasm_end;
  }
  size_t __builtin_wasm_memory_grow(size_t memory, size_t delta)
  {
    size_t old = __builtin_wasm_memory_size(memory);
    wasm_end += delta;
    return old;
  }

  extern void* wasm_malloc(size_t num);
  extern void wasm_free(void* ptr);
  extern void* wasm_realloc(void* src, size_t num);
  extern void* wasm_calloc(size_t num, size_t size);
}

void TestHarness::test_malloc()
{
  void* begin_test = (char*)wasm_buf + WASM_PAGE;

  void* p = wasm_malloc(1);
  TEST(p == begin_test);
  TEST(wasm_realloc(p, 2) == p);
  TEST(wasm_realloc(p, 3) == p);
  TEST(wasm_realloc(p, 16) == p);
  void* x = wasm_malloc(32);
  TEST(p != x);
  wasm_free(p);
  wasm_free(x);
  p = wasm_malloc(1);
  TEST(p == begin_test);
  void* p2 = wasm_realloc(p, 62);
  TEST(p2 != p);
  TEST(wasm_realloc(p2, 64) == p2);
  wasm_free(p2);

  void* powers[15];
  for(int i = 0; i < 15; ++i)
  {
    powers[i] = wasm_malloc(1 << (i + 4));

    for(int j = 0; j < i; ++j)
      TEST(powers[j] != powers[i]);
  }

  for(int i = 0; i < 15; ++i)
    wasm_free(powers[i]);

  p = wasm_malloc(1);
  TEST(p == begin_test);
  wasm_free(p);
}