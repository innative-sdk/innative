// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __STREAM_H__IR__
#define __STREAM_H__IR__

#include "native-wasm/native-wasm.h"
#include <inttypes.h>
#include <memory.h>

namespace innative {
  // We just read the entire payload into memory, so this re-implements trivial stream operations without C++ stream overhead
  struct Stream
  {
    uint8_t* data;
    size_t size;
    size_t pos;

    // Attempts to read num bytes from the stream, returns actual number of bytes read
    inline size_t ReadBytes(uint8_t* target, size_t num) noexcept
    {
      if(!num)
        return 0;

      size_t diff = size - pos; // pos is always less than size so this never underflows
      if(diff < num)
        num = diff;

      memcpy(target, data + pos, num);
      pos += num;
      return num;
    }

    // Reads a type from the stream, returns false if EOF reached before entire type could be read.
    template<typename T>
    IR_FORCEINLINE bool Read(T& t) noexcept { return ReadBytes(reinterpret_cast<uint8_t*>(&t), sizeof(T)) == sizeof(T); }

    // Reads one character from the stream, returns -1 if EOF has been reached
    IR_FORCEINLINE int Get() noexcept
    {
      if(pos < size)
        return data[pos++];
      return -1;
    }

    // Returns true if end of the stream has been reached
    IR_FORCEINLINE bool End() noexcept { return pos >= size; }
  };
}

#endif