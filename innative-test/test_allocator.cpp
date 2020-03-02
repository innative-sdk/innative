// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "../innative/utility.h"
#include "test.h"
#include <thread>
#include <algorithm>

using namespace innative;

void TestHarness::test_allocator()
{
  const size_t TRIALS   = 5;
  const size_t MAXALLOC = 150;
  const size_t MAXSIZE  = 500000;
  int NUM               = std::thread::hardware_concurrency();
  std::atomic_bool start;

  for(size_t k = 0; k < TRIALS; ++k)
  {
    std::atomic_size_t count;
    count.exchange(0);
    std::unique_ptr<std::thread[]> threads(new std::thread[NUM]);
    std::unique_ptr<std::vector<std::pair<void*, size_t>>[]> maps(new std::vector<std::pair<void*, size_t>>[NUM]);
    IN_WASM_ALLOCATOR alloc;

    auto fn = [&](int id) {
      while(!start.load())
        ;
      size_t total = 0;
      while(total < MAXSIZE)
      {
        size_t sz = 1 + (size_t)((float(rand()) / RAND_MAX) * MAXALLOC);
        total += sz;
        maps[id].push_back({ alloc.allocate(sz), sz });
      }

      count.fetch_add(maps[id].size());
    };

    start.store(false);
    for(int i = 0; i < NUM; ++i)
      threads[i] = std::thread(fn, i);
    start.store(true);

    for(int i = 0; i < NUM; ++i)
      threads[i].join();

    // Merge the list of allocations we made
    std::vector<std::pair<void*, size_t>> merge;
    merge.reserve(count.load());
    for(int i = 0; i < NUM; ++i)
      for(auto& e : maps[i])
        merge.push_back(e);

    std::sort(merge.begin(), merge.end(),
              [](const std::pair<void*, size_t>& a, const std::pair<void*, size_t>& b) { return a.first < b.first; });
    bool pass = true;

    for(size_t i = 1; i < merge.size(); ++i)
      pass = pass && ((((char*)merge[i - 1].first) + merge[i - 1].second) <= merge[i].first);

    TEST(pass);
  }
}