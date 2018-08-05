// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __UTIL_H__IR__
#define __UTIL_H__IR__

#include "innative/schema.h"
#include "stream.h"
#include "path.h"
#include <string>
#include <math.h>
#include <exception>
#include <assert.h>
#include <utility>

#define IR_INIT_FUNCTION "_innative_internal_start"
#define kh_exist2(h, x) ((iter < kh_end(h)) && kh_exist(h, iter))

namespace innative {
  extern const char* IR_ENTRYPOINT;
  extern const char* IR_GETCPUINFO;
  extern const char* IR_EXTENSION;
  extern const char* IR_ENV_EXTENSION;
  extern const char* IR_GLUE_STRING;

  extern const char OPNAMES[][20];
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
  IR_FORCEINLINE void FlipEndian(T* target) noexcept
  {
    FlipEndian(reinterpret_cast<uint8_t*>(target), sizeof(T));
  }

  IR_FORCEINLINE uint32 ReadUInt32(Stream& s, IR_ERROR& err)
  {
    uint32 r = 0;
    if(!s.Read(r))
      err = ERR_PARSE_UNEXPECTED_EOF;

#ifdef IR_ENDIAN_BIG
    FlipEndian(&r);
#endif
    return r;
  }

  struct StringRef
  {
    const char* s;
    size_t len;

    bool operator ==(const StringRef& r) const
    {
      if(len != r.len)
        return false;
      return !memcmp(s, r.s, len);
    }
  };

  static inline khint_t __ac_X31_hash_stringrefins(StringRef ref)
  {
    const char* s = ref.s;
    const char* end = ref.s + ref.len;
    khint_t h = ((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s;
    if(h)
      for(++s; s < end; ++s)
        h = (h << 5) - h + (((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s);
    return h;
  }

  template<class T>
  inline errno_t tmemcpy(T* dest, size_t destsize, const T* src, size_t srcsize)
  {
#ifdef IR_COMPILER_MSC
    return memcpy_s(dest, destsize * sizeof(T), src, srcsize * sizeof(T));
#else
    return memcpy(dest, src, srcsize * sizeof(T));
#endif
  }

  template<class T>
  inline T* trealloc(T* p, size_t sz)
  {
    return reinterpret_cast<T*>(realloc(p, sz * sizeof(T)));
  }

  uint64_t DecodeLEB128(Stream& s, IR_ERROR& err, unsigned int maxbits, bool sign);
  IR_FORCEINLINE varuint1 ReadVarUInt1(Stream& s, IR_ERROR& err) { return DecodeLEB128(s, err, 1, false) != 0; }
  IR_FORCEINLINE varuint7 ReadVarUInt7(Stream& s, IR_ERROR& err) { return static_cast<varuint7>(DecodeLEB128(s, err, 7, false)); }
  IR_FORCEINLINE varuint32 ReadVarUInt32(Stream& s, IR_ERROR& err) { return static_cast<varuint32>(DecodeLEB128(s, err, 32, false)); }
  IR_FORCEINLINE varuint64 ReadVarUInt64(Stream& s, IR_ERROR& err) { return static_cast<varuint64>(DecodeLEB128(s, err, 64, false)); }
  IR_FORCEINLINE varsint7 ReadVarInt7(Stream& s, IR_ERROR& err) { return static_cast<varsint7>(DecodeLEB128(s, err, 7, true)); }
  IR_FORCEINLINE varsint32 ReadVarInt32(Stream& s, IR_ERROR& err) { return static_cast<varsint32>(DecodeLEB128(s, err, 32, true)); }
  IR_FORCEINLINE varsint64 ReadVarInt64(Stream& s, IR_ERROR& err) { return static_cast<varsint64>(DecodeLEB128(s, err, 64, true)); }
  template<class T>
  inline T ReadPrimitive(Stream& s, IR_ERROR& err)
  {
    T r = 0;
    if(!s.Read<T>(r))
      err = ERR_PARSE_UNEXPECTED_EOF;
    return r;
  }
  IR_FORCEINLINE float64 ReadFloat64(Stream& s, IR_ERROR& err) { return ReadPrimitive<float64>(s, err); }
  IR_FORCEINLINE float32 ReadFloat32(Stream& s, IR_ERROR& err) { return ReadPrimitive<float32>(s, err); }
  IR_FORCEINLINE uint8_t ReadByte(Stream& s, IR_ERROR& err) { return ReadPrimitive<uint8_t>(s, err); }
  uint8_t GetInstruction(StringRef s);
  void* GreedyAlloc(size_t n);

  template<class T>
  T* tmalloc(size_t n)
  {
    return !n ? 0 : reinterpret_cast<T*>(GreedyAlloc(n * sizeof(T)));
  }

  IR_FORCEINLINE bool ModuleHasSection(Module& m, varuint7 opcode) { return (m.knownsections&(1 << opcode)) != 0; }
  inline std::string MergeStrings(const char* a, const char* b) { return std::string(!a ? "" : a) + b; }

  varuint32 ModuleFunctionType(Module& m, varuint32 index);
  FunctionSig* ModuleFunction(Module& m, varuint32 index);
  TableDesc* ModuleTable(Module& m, varuint32 index);
  MemoryDesc* ModuleMemory(Module& m, varuint32 index);
  GlobalDesc* ModuleGlobal(Module& m, varuint32 index);
  std::pair<Module*, Export*> ResolveExport(Environment& env, Import& imp);
  Path GetProgramPath();
  Path GetWorkingDir();
  bool SetWorkingDir(const char* path);
  std::string StrFormat(const char* fmt, ...);
  void GetCPUInfo(uintcpuinfo& info, int flags);
  void* LoadDLL(const char* path);
  void* LoadDLLFunction(void* dll, const char* name);
  void FreeDLL(void* dll);

  template<class F>
  class DeferLambda
  {
  public:
    inline DeferLambda(const F& f) : _f(f) {}
    inline ~DeferLambda() { _f(); }

  protected:
    F _f;
  };

  // Merges two strings with the standard IR_GLUE_STRING
  inline std::string MergeName(const char* prefix, const char* name, int index = -1)
  {
    if(index >= 0)
    {
      char buf[20] = { 0 };
      ITOA(index, buf, 20, 10);
      return !prefix ? std::string(name) + buf : (std::string(prefix) + IR_GLUE_STRING + name + buf);
    }
    return !prefix ? name : (std::string(prefix) + IR_GLUE_STRING + name);
  }

  // Generates the correct mangled C function name
  inline std::string CanonImportName(const void* module_name, const void* export_name)
  {
    if(!module_name || strchr((const char*)module_name, '!') != nullptr) // blank imports or imports with a calling convention are always raw function names
      return (const char*)export_name;
    return MergeName((const char*)module_name, (const char*)export_name); // Otherwise do a standard merge
  }

  IR_FORCEINLINE std::string CanonImportName(Import& imp)
  {
    return CanonImportName(imp.module_name.bytes, imp.export_name.bytes);
  }

  // Generates a whitelist string for a module and export name, which includes calling convention information
  inline size_t CanonWhitelist(const void* module_name, const void* export_name, char* out)
  {
    if(module_name != nullptr && !STRICMP((const char*)module_name, "!C"))
      module_name = nullptr;
    if(!module_name)
      module_name = "";

    size_t module_len = strlen((const char*)module_name) + 1;
    size_t export_len = strlen((const char*)export_name) + 1;
    if(out)
    {
      tmemcpy<char>(out, module_len + export_len, (const char*)module_name, module_len);
      tmemcpy<char>(out + module_len, export_len, (const char*)export_name, export_len);
    }
    return module_len + export_len;
  }
  IR_FORCEINLINE std::string CanonWhitelist(const void* module_name, const void* export_name)
  {
    std::string s;
    s.resize(CanonWhitelist(module_name, export_name, nullptr));
    CanonWhitelist(module_name, export_name, const_cast<char*>(s.data()));
    return s;
  }
}

#endif