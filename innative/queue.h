// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__QUEUE_H
#define IN__QUEUE_H

#include <malloc.h>
#include <assert.h>
#include <vector>

namespace innative {
  // Implements a simple one-time queue that can be reset
  template<typename T> class Queue
  {
  public:
    Queue() : _pos(0) {}
    ~Queue() {}
    inline void Reserve(size_t capacity) { _array.reserve(capacity); }
    inline void Push(const T& item) { _array.push_back(item); }
    inline T& Front() { return _array.front(); }
    inline T& Back() { return _array.back(); }
    inline T Pop()
    {
      assert(_array.size() > _pos);
      return _array[_pos++];
    } // This only works with trivial types
    inline T& Peek()
    {
      assert(_array.size() > _pos);
      return _array[_pos];
    }
    inline const T& Peek() const
    {
      assert(_array.size() > _pos);
      return _array[_pos];
    }
    inline size_t Capacity() const { return _array.capacity(); }
    inline size_t Size() const { return _array.size() - _pos; }
    inline void SetPosition(size_t pos) { _pos = pos; }
    inline size_t GetPosition() const { return _pos; }
    inline void Clear()
    {
      _pos = 0;
      _array.clear();
    }

    const T& operator[](size_t i) const
    {
      assert(_pos + i < _array.size());
      return _array[_pos + i];
    }
    T& operator[](size_t i)
    {
      assert(_pos + i < _array.size());
      return _array[_pos + i];
    }

  protected:
    std::vector<T> _array;
    size_t _pos;
  };
}

#endif