// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "test.h"

#include "../innative-env/wait_list.h"
#include "../innative-env/atomics.h"

#include <atomic>
#include <thread>
#include <chrono>

using namespace std::chrono_literals;

// Use std::barrier instead if we get C++20 support
struct spin_barrier
{
  spin_barrier(size_t c) : count(c), num(0) {}

  void reset(size_t c = 0)
  {
    num = 0;
    if(c)
      count = c;
  }

  void wait(size_t step)
  {
    num.fetch_add(1);
    while(num < count * step)
      ;
  }

  size_t count;
  std::atomic<size_t> num;
};

void TestHarness::test_atomic_waitnotify()
{
  // Start by testing the waitlist itself
  auto wait_map = in_wait_map{ 0 };

  auto addr1 = (void*)0x1337;
  auto addr2 = (void*)0xbad1e;

  // Two addresses should get very different lists, and shouldn't
  // care about the address value
  auto list1 = _innative_internal_env_wait_map_get(&wait_map, addr1, 1);
  auto list2 = _innative_internal_env_wait_map_get(&wait_map, addr2, 1);

  TEST(list1 != nullptr);
  TEST(list2 != nullptr);
  TEST(list1 != list2);
  TEST(wait_map.len == 2);
  TEST(wait_map.free_lists == nullptr);

  if(list1 == nullptr || list2 == nullptr)
    return; // The rest of these tests won't work if those are null

  // These lists were never used so they should get put on the free list
  _innative_internal_env_wait_map_return(&wait_map, addr1, list1);
  _innative_internal_env_wait_map_return(&wait_map, addr2, list2);
  TEST(wait_map.len == 0);
  TEST(list1->next_free_list == nullptr);
  TEST(list2->next_free_list == list1);
  TEST(wait_map.free_lists == list2);

  // Since they have no users they shouldn't exist if we ask for them
  auto empty1 = _innative_internal_env_wait_map_get(&wait_map, addr1, 0);
  auto empty2 = _innative_internal_env_wait_map_get(&wait_map, addr2, 0);
  TEST(empty1 == nullptr);
  TEST(empty2 == nullptr);

  // Now test the wait list itself
  list1 = _innative_internal_env_wait_map_get(&wait_map, addr1, 1);
  TEST(list1 != nullptr);
  if(list1 == nullptr)
    return;

  // Test adding waiters
  auto entry1 = _innative_internal_env_wait_list_push(list1);
  TEST(entry1 != nullptr);
  if(entry1 == nullptr)
    return;
  TEST(entry1->signaled == 0);
  TEST(list1->len == 1);
  TEST(list1->cap >= 1);
  TEST(list1->entries != nullptr);
  if(list1->entries == nullptr)
    return;
  TEST(list1->entries[0] == entry1);

  _innative_internal_env_wait_list_remove(list1, entry1);
  TEST(list1->len == 0);
  TEST(list1->cap >= 1);
  TEST(list1->entries != nullptr);
  TEST(list1->free_list == entry1);
  TEST(entry1->next_free_node == nullptr);

  _innative_internal_env_wait_list_shrink(list1);
  TEST(list1->len == 0);
  TEST(list1->cap == 0);
  TEST(list1->entries == nullptr);
  TEST(list1->free_list == nullptr);

  // Test notification
  spin_barrier barrier{ 2 };
  std::thread notif_thread([&]() {
    _innative_internal_env_wait_list_enter(list1);
    auto entry = _innative_internal_env_wait_list_push(list1);
    TEST(list1->len == 1);

    // Test timeout when no notification comes
    auto val = _innative_internal_env_wait_entry_wait(list1, entry, 1'000'000 ); // 1 ms
    TEST(val == 2);
    TEST(entry->signaled == 0);

    // Allow the other thread to progress
    barrier.wait(1);

    // Wait for the signal
    val = _innative_internal_env_wait_entry_wait(list1, entry, -1); // no timeout
    TEST(val == 0);
    TEST(entry->signaled == 1);
    // The entry should be neither on the list or free
    TEST(list1->len == 0);
    TEST(list1->free_list == nullptr);

    // This should put the node on the free list
    _innative_internal_env_wait_list_remove(list1, entry);
    TEST(list1->free_list == entry);

    _innative_internal_env_wait_list_exit(list1);
  });

  barrier.wait(1);
  _innative_internal_env_wait_list_enter(list1);
  _innative_internal_env_wait_list_notify(list1, 1);
  _innative_internal_env_wait_list_exit(list1);
  notif_thread.join();

  // Normally we'd lock this but now we know there's only 1 thread left
  _innative_internal_env_wait_map_return(&wait_map, addr1, list1);
  TEST(wait_map.len == 0);
  TEST(wait_map.free_lists == list1);

  _innative_internal_env_wait_map_cleanup(&wait_map);
  TEST(wait_map.cap == 0);
  TEST(wait_map.entries == nullptr);
  TEST(wait_map.free_lists == nullptr);
  
  // Test using the wait/notify to implement a crude lock
  // Use enough threads and lock enough times that it should really get tested
  std::atomic<int32_t> lock     = 0;
  size_t result                 = 0;
  size_t num_threads            = 100;
  size_t increment              = 100'000;
  std::atomic<size_t> remaining = num_threads;

  auto do_lock = [&]() {
    for(;;)
    {
      int32_t expected = 0;
      if(lock.compare_exchange_weak(expected, 1))
        break;

      _innative_internal_env_atomic_wait32(&lock, 1, -1);
    }
  };

  auto do_unlock = [&]() {
    lock.store(0);
    _innative_internal_env_atomic_notify(&lock, 1);
  };

  for(size_t i = 0; i < num_threads; ++i)
  {
    std::thread([&]() {
      for(size_t j = 0; j < increment; ++j)
      {
        do_lock();
        result += 1;
        do_unlock();
      }
      remaining.fetch_sub(1);
    }).detach();
  }

  while(remaining > 0)
    std::this_thread::sleep_for(50ms);

  TEST(result == num_threads * increment);
}
