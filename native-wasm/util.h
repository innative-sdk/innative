// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __UTIL_H__NW__
#define __UTIL_H__NW__

#include "native-wasm/schema.h"
#include <string>
#include <math.h>
#include <exception>
#include <assert.h>
#include <utility>

#define NW_ENTRYPOINT "__native_wasm_main_func"
#define NW_GETCPUINFO "__native_wasm_getcpuinfo"
#define NW_EXTENSION ".nw-cache"
typedef int uintcpuinfo[5];

inline void FlipEndian(uint8_t* target, uint8_t n) noexcept
{
  uint8_t t;
  uint8_t end = (n >> 1);
  --n;
  for(uint8_t i = 0; i < end; ++i)
  {
    t = target[n - i];
    target[n - i] = target[i];
    target[i] = t;
  }
}

template<typename T>
NW_FORCEINLINE void FlipEndian(T* target) noexcept
{
  FlipEndian(reinterpret_cast<uint8_t*>(target), sizeof(T));
}

// Implements a very simple stack structure that ignores constructors/destructors
template<typename T>
class Stack
{
public:
  Stack() : _array(0), _capacity(0), _size(0) {}
  ~Stack()
  {
    if(_array)
      free(_array);
  }
  inline void Reserve(size_t capacity)
  {
    if(_capacity < capacity)
    {
      _array = reinterpret_cast<T*>(realloc(_array, capacity));
      _capacity = capacity;
    }
  }
  inline void Push(const T& item)
  { 
    if(_size >= _capacity) 
      Reserve(_capacity * 2); 
    _array[_size++] = item;
  }
  inline T Pop() { assert(_size > 0); return _array[--_size]; } // This only works with trivial types
  inline T& Peek() { _array[_size - 1]; }
  inline const T& Peek() const { _array[_size - 1]; }
  inline size_t Capacity() const { return _capacity; }
  inline size_t Size() const { return _size; }

protected:
  T* _array;
  size_t _capacity;
  size_t _size;
};

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
  NW_FORCEINLINE bool Read(T& t) noexcept { return ReadBytes(reinterpret_cast<uint8_t*>(&t), sizeof(T)) == sizeof(T); }
  
  // Reads one character from the stream, returns -1 if EOF has been reached
  NW_FORCEINLINE int Get() noexcept
  { 
    if(pos < size) 
      return data[pos++]; 
    return -1; 
  }

  // Returns true if end of the stream has been reached
  NW_FORCEINLINE bool End() noexcept { return pos >= size; }
};

NW_FORCEINLINE uint32 ReadUInt32(Stream& s, ERROR_CODE& err)
{ 
  uint32 r = 0;
  if(!s.Read(r))
    err = ERR_PARSE_UNEXPECTED_EOF;

#ifdef NW_ENDIAN_BIG
  FlipEndian(&r);
#endif
  return r;
}

uint64_t DecodeLEB128(Stream& s, ERROR_CODE& err, unsigned int maxbits, bool sign);
NW_FORCEINLINE varuint1 ReadVarUInt1(Stream& s, ERROR_CODE& err) { return DecodeLEB128(s, err, 1, false) != 0; }
NW_FORCEINLINE varuint7 ReadVarUInt7(Stream& s, ERROR_CODE& err) { return static_cast<varuint7>(DecodeLEB128(s, err, 7, false)); }
NW_FORCEINLINE varuint32 ReadVarUInt32(Stream& s, ERROR_CODE& err) { return static_cast<varuint32>(DecodeLEB128(s, err, 32, false)); }
NW_FORCEINLINE varuint64 ReadVarUInt64(Stream& s, ERROR_CODE& err) { return static_cast<varuint64>(DecodeLEB128(s, err, 64, false)); }
NW_FORCEINLINE varsint7 ReadVarInt7(Stream& s, ERROR_CODE& err) { return static_cast<varsint7>(DecodeLEB128(s, err, 7, true)); }
NW_FORCEINLINE varsint32 ReadVarInt32(Stream& s, ERROR_CODE& err) { return static_cast<varsint32>(DecodeLEB128(s, err, 32, true)); }
NW_FORCEINLINE varsint64 ReadVarInt64(Stream& s, ERROR_CODE& err) { return static_cast<varsint64>(DecodeLEB128(s, err, 64, true)); }
NW_FORCEINLINE float32 ReadFloat32(Stream& s, ERROR_CODE& err)
{
  float32 r = 0;
  if(!s.Read(r))
    err = ERR_PARSE_UNEXPECTED_EOF;
  return r;
}
NW_FORCEINLINE float64 ReadFloat64(Stream& s, ERROR_CODE& err)
{
  float64 r = 0;
  if(!s.Read(r))
    err = ERR_PARSE_UNEXPECTED_EOF;
  return r;
}

NW_FORCEINLINE bool ModuleHasSection(Module& m, varuint7 opcode) { return (m.knownsections&(1 << opcode)) != 0; }

FunctionSig* ModuleFunction(Module& m, varuint32 index);
TableDesc* ModuleTable(Module& m, varuint32 index);
MemoryDesc* ModuleMemory(Module& m, varuint32 index);
GlobalDesc* ModuleGlobal(Module& m, varuint32 index);
std::pair<Module*, Export*> ResolveExport(Environment& env, Import& imp);
std::string GetProgramPath();
void GetCPUInfo(uintcpuinfo& info, int flags);
void* LoadDLL(const char* path);
void* LoadDLLFunction(void* dll, const char* name);
void FreeDLL(void* dll);

#endif