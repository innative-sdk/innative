// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __STREAM_H__IR__
#define __STREAM_H__IR__

#include "innative/schema.h"
#include <inttypes.h>
#include <memory.h>

namespace innative {
  namespace utility {
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
      IR_FORCEINLINE uint32 ReadUInt32(IR_ERROR& err)
      {
        uint32 r = 0;
        if(!Read(r))
          err = ERR_PARSE_UNEXPECTED_EOF;

#ifdef IR_ENDIAN_BIG
        internal::FlipEndian(&r);
#endif
        return r;
      }

      uint64_t DecodeLEB128(IR_ERROR& err, unsigned int maxbits, bool sign);
      IR_FORCEINLINE varuint1 ReadVarUInt1(IR_ERROR& err) { return DecodeLEB128(err, 1, false) != 0; }
      IR_FORCEINLINE varuint7 ReadVarUInt7(IR_ERROR& err) { return static_cast<varuint7>(DecodeLEB128(err, 7, false)); }
      IR_FORCEINLINE varuint32 ReadVarUInt32(IR_ERROR& err) { return static_cast<varuint32>(DecodeLEB128(err, 32, false)); }
      IR_FORCEINLINE varuint64 ReadVarUInt64(IR_ERROR& err) { return static_cast<varuint64>(DecodeLEB128(err, 64, false)); }
      IR_FORCEINLINE varsint7 ReadVarInt7(IR_ERROR& err) { return static_cast<varsint7>(DecodeLEB128(err, 7, true)); }
      IR_FORCEINLINE varsint32 ReadVarInt32(IR_ERROR& err) { return static_cast<varsint32>(DecodeLEB128(err, 32, true)); }
      IR_FORCEINLINE varsint64 ReadVarInt64(IR_ERROR& err) { return static_cast<varsint64>(DecodeLEB128(err, 64, true)); }
      template<class T>
      inline T ReadPrimitive(IR_ERROR& err)
      {
        T r = 0;
        if(!Read<T>(r))
          err = ERR_PARSE_UNEXPECTED_EOF;
        return r;
      }
      IR_FORCEINLINE float64 ReadFloat64(IR_ERROR& err) { return ReadPrimitive<float64>(err); }
      IR_FORCEINLINE float32 ReadFloat32(IR_ERROR& err) { return ReadPrimitive<float32>(err); }
      IR_FORCEINLINE uint8_t ReadByte(IR_ERROR& err) { return ReadPrimitive<uint8_t>(err); }
    };
  }
}

#endif