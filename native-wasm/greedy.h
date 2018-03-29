// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __GREEDY_H__NW__
#define __GREEDY_H__NW__

#include "native-wasm/native-wasm.h"
#include <atomic>
#include <assert.h>

// Lockless multi-producer multi-consumer greedy allocator, intended to be cleaned up at program termination
struct GreedyAlloc
{
  GreedyAlloc(const GreedyAlloc& copy) = delete;
  GreedyAlloc& operator=(const GreedyAlloc& copy) = delete;
  struct Node
  {
    Node* next;
    size_t size;
  };

public:
  inline explicit GreedyAlloc(size_t init = 64) : _root(0), _curpos(0)
  {
    _flag.clear(std::memory_order_relaxed);
    _allocChunk(init);
  }
  inline ~GreedyAlloc()
  {
    Clear();
  }
  inline void* Alloc(size_t sz) noexcept
  {
    size_t r;
    Node* root;

    for(;;)
    {
      r = _curpos.fetch_add(sz, std::memory_order_acq_rel);
      size_t rend = r + sz;
      root = _root.load(std::memory_order_acquire);

      if(rend >= root->size)
      {
        if(!_flag.test_and_set())
        {
          if(rend >= _root.load(std::memory_order_acquire)->size) // We do another check in here to ensure another thread didn't already resize the root for us.
          {
            _allocChunk(rend * 2);
            _curpos.store(0, std::memory_order_release);
          }
        }
      }
      else
        break;
    }

    return reinterpret_cast<uint8_t*>(root + 1) + r;
  }
  void Clear()
  {
    Node* root = _root.load(std::memory_order_relaxed);

    while(root)
    {
      Node* hold = root->next;
      free(root);
      root = hold;
    }

    _root.store(0, std::memory_order_relaxed);
    _curpos.store(0, std::memory_order_relaxed);
  }

protected:
  inline void _allocChunk(size_t nsize) noexcept
  {
    Node* retval = reinterpret_cast<Node*>(malloc(sizeof(Node) + nsize));
    assert(retval != 0);
    retval->next = _root.load(std::memory_order_acquire);
    retval->size = nsize;
    _root.store(retval, std::memory_order_release);
  }

  NW_ALIGN(4) std::atomic_flag _flag;
  std::atomic<size_t> _curpos;
  std::atomic<Node*> _root;
};

#endif