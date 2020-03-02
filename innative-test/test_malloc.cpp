// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"
#include "../innative/utility.h"
#include <functional>
#include <map>

using namespace innative;

static const int WASM_PAGE = (1 << 16);
static const int MAX_PAGES = 32;
static void* wasm_buf      = malloc(WASM_PAGE * (MAX_PAGES + 1));
static size_t wasm_end     = 0;

extern "C" {
size_t __builtin_wasm_memory_size(size_t memory) { return ((size_t)wasm_buf + wasm_end) / WASM_PAGE; }

size_t __builtin_wasm_memory_grow(size_t memory, size_t delta)
{
  delta *= WASM_PAGE;
  size_t old = __builtin_wasm_memory_size(memory);
  memset((char*)wasm_buf + wasm_end, 0, delta);
  wasm_end += delta;
  return old;
}

extern void* wasm_malloc(size_t num);
extern void wasm_free(void* ptr);
extern void* wasm_realloc(void* src, size_t num);
extern void* wasm_calloc(size_t num, size_t size);
extern char _verify_ptr(void* ptr);
extern char _verify_heaps();
}

void TestHarness::test_malloc()
{
  assert(wasm_buf != nullptr);
  // Page-align our fake memory allocation so the webassembly math works correctly
  memset(wasm_buf, 0xCC, WASM_PAGE * (MAX_PAGES + 1));
  (char*&)wasm_buf += WASM_PAGE - ((size_t)wasm_buf % WASM_PAGE);
  assert(!((size_t)wasm_buf % WASM_PAGE));
  memset(wasm_buf, 0xAA, WASM_PAGE * MAX_PAGES);
  void* begin_test = (char*)wasm_buf + WASM_PAGE;

  void* ptr = wasm_malloc(1);
  TEST(ptr == begin_test);
  TEST(wasm_realloc(ptr, 2) == ptr);
  TEST(wasm_realloc(ptr, 3) == ptr);
  TEST(wasm_realloc(ptr, 16) == ptr);
  void* x = wasm_malloc(32);
  TEST(ptr != x);
  wasm_free(ptr);
  wasm_free(x);
  ptr = wasm_malloc(1);
  TEST(ptr == begin_test);
  void* p2 = wasm_realloc(ptr, 62);
  TEST(p2 != ptr);
  TEST(wasm_realloc(p2, 64) == p2);
  wasm_free(p2);

  const uint32_t MAX_POWER = 15;
  void* powers[MAX_POWER];
  for(uint32_t i = 0; i < MAX_POWER; ++i)
  {
    powers[i] = wasm_malloc((size_t)1 << (i + 4));

    for(uint32_t j = 0; j < i; ++j)
      TEST(powers[j] != powers[i]);
  }

  for(uint32_t i = 0; i < MAX_POWER; ++i)
    wasm_free(powers[i]);

  ptr = wasm_malloc(1);
  TEST(ptr == begin_test);
  wasm_free(ptr);

  // Fuzzing
  const int MAXSIZE    = 5000;
  const int ITERATIONS = 200000;
  int64_t total        = 0;
  std::map<void*, int> tracker;

  for(uint32_t i = 0; i < ITERATIONS; ++i)
  {
    if((total < (WASM_PAGE * MAX_PAGES / 8)) && (tracker.size() < (ITERATIONS - i)) && (tracker.empty() || (rand() % 2)))
    {
      size_t len = rand() % MAXSIZE;
      void* p    = wasm_malloc(len);
      TEST(tracker.count(p) == 0);

      tracker.emplace(p, len);
      auto index = tracker.upper_bound(p);
      if(index != tracker.end())
      {
        TEST((*index).first >= ((char*)p + len));
      }

      bool valid = true;
      for(size_t j = 0; j < len; ++j)
        valid = valid && ((char*)p)[j] == 0;
      TEST(valid);

      total += len;
      memset(p, 0xF0, len);
    }
    else
    {
      auto index = tracker.begin();
      std::advance(index, rand() % tracker.size());
      uint8_t* p = reinterpret_cast<uint8_t*>((*index).first);
      size_t len = (*index).second;

      bool valid = true;
      for(size_t j = 0; j < len; ++j)
        valid = valid && p[j] == 0xF0;
      TEST(valid);

      memset(p, 0, len);
      total -= len;
      tracker.erase(index);
      wasm_free(p);
    }
  }

  TEST(!tracker.size());

  ptr = wasm_malloc(99999);
  TEST(ptr == begin_test);
  wasm_free(ptr);
  const int FILL_MAX = WASM_PAGE / 2;
  void* pfill[FILL_MAX];

  for(uint32_t i = 0; i < FILL_MAX - 1; ++i)
    pfill[i] = wasm_malloc(16);
  pfill[FILL_MAX - 1] = wasm_malloc(16);

  for(uint32_t i = FILL_MAX; i > 0; i--)
  {
    uint32_t index = rand() % i;
    wasm_free(pfill[index]);
    pfill[index] = pfill[i - 1];
  }

  ptr = wasm_malloc(1);
  TEST(ptr == begin_test);
  wasm_free(ptr);
}