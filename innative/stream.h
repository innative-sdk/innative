// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__STREAM_H
#define IN__STREAM_H

#include "innative/schema.h"
#include <inttypes.h>
#include <memory.h>

namespace innative {
  namespace utility {
    // We read the entire payload into memory, so this re-implements trivial stream operations without C++ stream
    // overhead
    struct Stream
    {
      const uint8_t* data;
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
      template<typename T> IN_FORCEINLINE bool Read(T& t) noexcept
      {
        if(sizeof(T) > (size - pos))
        {
          pos = size; // We advance the read pointer to the end, but don't read anything
          return false;
        }

        memcpy(reinterpret_cast<void*>(&t), data + pos, sizeof(T));
        pos += sizeof(T);
        return true;
      }

      // Reads one character from the stream, returns -1 if EOF has been reached
      IN_FORCEINLINE int Get() noexcept
      {
        if(pos < size)
          return data[pos++];
        return -1;
      }

      // Returns true if end of the stream has been reached
      IN_FORCEINLINE bool End() noexcept { return pos >= size; }
      IN_FORCEINLINE uint32 ReadUInt32(IN_ERROR& err)
      {
        uint32 r = 0;
        if(!Read(r))
          err = ERR_PARSE_UNEXPECTED_EOF;

#ifdef IN_ENDIAN_BIG
        internal::FlipEndian(&r);
#endif
        return r;
      }

      IN_COMPILER_DLLEXPORT uint64_t DecodeLEB128(IN_ERROR& err, unsigned int maxbits, bool sign);
      IN_FORCEINLINE varuint1 ReadVarUInt1(IN_ERROR& err) { return DecodeLEB128(err, 1, false) != 0; }
      IN_FORCEINLINE varuint7 ReadVarUInt7(IN_ERROR& err) { return static_cast<varuint7>(DecodeLEB128(err, 7, false)); }
      IN_FORCEINLINE varuint32 ReadVarUInt32(IN_ERROR& err) { return static_cast<varuint32>(DecodeLEB128(err, 32, false)); }
      IN_FORCEINLINE varuint64 ReadVarUInt64(IN_ERROR& err) { return static_cast<varuint64>(DecodeLEB128(err, 64, false)); }
      IN_FORCEINLINE varsint7 ReadVarInt7(IN_ERROR& err) { return static_cast<varsint7>(DecodeLEB128(err, 7, true)); }
      IN_FORCEINLINE varsint32 ReadVarInt32(IN_ERROR& err) { return static_cast<varsint32>(DecodeLEB128(err, 32, true)); }
      IN_FORCEINLINE varsint64 ReadVarInt64(IN_ERROR& err) { return static_cast<varsint64>(DecodeLEB128(err, 64, true)); }
      template<class T> inline T ReadPrimitive(IN_ERROR& err)
      {
        T r = 0;
        if(!Read<T>(r))
          err = ERR_PARSE_UNEXPECTED_EOF;
        return r;
      }
      IN_FORCEINLINE float64 ReadFloat64(IN_ERROR& err) { return ReadPrimitive<float64>(err); }
      IN_FORCEINLINE float32 ReadFloat32(IN_ERROR& err) { return ReadPrimitive<float32>(err); }
      IN_FORCEINLINE uint8_t ReadByte(IN_ERROR& err) { return ReadPrimitive<uint8_t>(err); }
    };
  }
}

#endif