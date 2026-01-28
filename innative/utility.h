// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__UTIL_H
#define IN__UTIL_H

#include "innative/schema.h"
#include "constants.h"
#include <string>
#include <math.h>
#include <exception>
#include <assert.h>
#include <utility>
#include <memory>
#include <atomic>
#include <vector>
#include "../innative/filesys.h"

struct IN_WASM_ALLOCATOR
{
  IN_WASM_ALLOCATOR() : mem(0), sz(0), cur(0), commit(0) {}
  IN_COMPILER_DLLEXPORT ~IN_WASM_ALLOCATOR();

  std::atomic<void*> mem;
  std::atomic_size_t sz;
  std::atomic_size_t cur;
  std::atomic_size_t commit;
  std::vector<std::pair<void*, size_t>> list;

  IN_COMPILER_DLLEXPORT void* allocate(size_t n);
};

  // Allocator that uses an external policy to perform allocations
template<typename T> class WasmAllocPolicy
{
public:
  using value_type = T;
  template<class U> struct rebind
  {
    typedef WasmAllocPolicy<U> other;
  };

  explicit WasmAllocPolicy(IN_WASM_ALLOCATOR& alloc) noexcept : _alloc(&alloc) {}
  template<class U> WasmAllocPolicy(WasmAllocPolicy<U> const& other) noexcept : _alloc(other._alloc) {}
  template<class U> WasmAllocPolicy(WasmAllocPolicy<U>&& other) noexcept : _alloc(std::move(other).move_alloc()) {}

  value_type* allocate(std::size_t n) { return reinterpret_cast<value_type*>(_alloc->allocate(n * sizeof(value_type))); }
  inline value_type* reallocate(size_t n, T* p, size_t oldsize) { return _alloc->allocate(n * sizeof(value_type))); }
  void deallocate(value_type* p, std::size_t sz) noexcept {}

  inline IN_WASM_ALLOCATOR* move_alloc() && noexcept
  {
    auto p = _alloc;
    _alloc = nullptr;
    return p;
  }

  using propagate_on_container_copy_assignment = std::true_type;
  using propagate_on_container_move_assignment = std::true_type;
  using propagate_on_container_swap            = std::true_type;
  using is_always_equal                        = std::false_type;

  WasmAllocPolicy select_on_container_copy_construction() const noexcept { return *this; }

protected:
  IN_WASM_ALLOCATOR* _alloc;
};

extern "C" int64_t GetRSPValue();

namespace innative {
  namespace utility {
#ifdef IN_PLATFORM_WIN32
    typedef int uintcpuinfo[5];
#else
    typedef unsigned int uintcpuinfo[5];
#endif

    struct StringSpan
    {
      const char* s;
      size_t len;

      static StringSpan From(const ByteArray& b) { return StringSpan{ b.str(), b.size() }; }
      static StringSpan From(const char* s) { return StringSpan{ s, !s ? 0 : strlen(s) }; }

      bool operator==(const StringSpan& r) const
      {
        if(len != r.len)
          return false;
        if(!len)
          return true;
        return !memcmp(s, r.s, len);
      }
    };
  }

  namespace internal {
    // For simplicity reasons, we assemble the error list backwards. This reverses it so it appears in the correct order.
    inline void ReverseErrorList(ValidationError*& errors) noexcept
    {
      auto cur              = errors;
      ValidationError* prev = nullptr;
      while(cur != nullptr)
      {
        auto next = cur->next;
        cur->next = prev;
        prev      = cur;
        cur       = next;
      }
      errors = prev;
    }

    inline void FlipEndian(uint8_t* target, uint8_t n) noexcept
    {
      uint8_t t;
      uint8_t end = (n >> 1);
      --n;
      for(uint8_t i = 0; i < end; ++i)
      {
        t             = target[n - i];
        target[n - i] = target[i];
        target[i]     = t;
      }
    }

    template<typename T> IN_FORCEINLINE void FlipEndian(T* target) noexcept
    {
      FlipEndian(reinterpret_cast<uint8_t*>(target), sizeof(T));
    }

    inline khint_t __ac_X31_hash_stringrefins(utility::StringSpan ref)
    {
      const char* s   = ref.s;
      const char* end = ref.s + ref.len;
      khint_t h       = ((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s;
      if(h)
        for(++s; s < end; ++s)
          h = (h << 5) - h + (((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s);
      return h;
    }

    inline khint_t __ac_X31_hash_bytearray(const ByteArray& id)
    {
      const char* s   = reinterpret_cast<const char*>(id.get());
      const char* end = s + id.size();
      khint_t h       = 0;
      if(s < end)
      {
        h = *s;
        for(++s; s < end; ++s)
          h = (h << 5) - h + (khint_t)*s;
      }
      return h;
    }

    // Hashes a pair of strings seperated by a null terminator
    kh_inline khint_t __ac_X31_hash_string_pair(const char* s)
    {
      khint_t h = (khint_t)*s;
      if(h)
        for(++s; *s; ++s)
          h = (h << 5) - h + (khint_t)*s;
      for(++s; *s; ++s)
        h = (h << 5) - h + (khint_t)*s;
      return h;
    }
  }

  namespace utility {
    template<class F> class DeferLambda
    {
    public:
      inline DeferLambda(const F& f) : _f(f) {}
      inline ~DeferLambda() { _f(); }

    protected:
      F _f;
    };

    template<class T> inline void tmemcpy(T* dest, size_t destsize, const T* src, size_t srcsize)
    {
#ifdef IN_COMPILER_MSC
      memcpy_s(dest, destsize * sizeof(T), src, srcsize * sizeof(T));
#else
      memcpy(dest, src, srcsize * sizeof(T));
#endif
    }
    const char* AllocString(const char* s, size_t n);

    template<class T> inline T* trealloc(T* p, size_t sz) { return reinterpret_cast<T*>(realloc(p, sz * sizeof(T))); }

    // Checks if an integer is a power of two
    inline bool IsPowerOfTwo(varuint32 x) noexcept { return (x & (x - 1)) == 0; }

    // Given an exact power of two, quickly gets the log2 value
    inline uint32_t Power2Log2(uint32_t v) noexcept
    {
      assert(IsPowerOfTwo(v));
#ifdef IN_COMPILER_MSC
      unsigned long r;
      _BitScanReverse(&r, v);
#elif defined(IN_COMPILER_GCC)
      uint32_t r = (sizeof(uint32_t) << 3) - 1 - __builtin_clz(v);
#else
      const uint32_t b[] = { 0xAAAAAAAA, 0xCCCCCCCC, 0xF0F0F0F0, 0xFF00FF00, 0xFFFF0000 };
      uint32_t r         = (v & b[0]) != 0;
      r |= ((v & b[4]) != 0) << 4;
      r |= ((v & b[3]) != 0) << 3;
      r |= ((v & b[2]) != 0) << 2;
      r |= ((v & b[1]) != 0) << 1;
#endif
      return r;
    }

    inline varuint32 NextPow2(varuint32 v) noexcept
    {
      v -= 1;
      v |= (v >> 1);
      v |= (v >> 2);
      v |= (v >> 4);
      v |= (v >> 8);
      v |= (v >> 16);

      return v + 1;
    }

    IN_FORCEINLINE bool ModuleHasSection(const Module& m, varuint7 opcode)
    {
      return (m.knownsections & (1 << opcode)) != 0;
    }

    inline const char* AllocString(IN_WASM_ALLOCATOR& alloc, const char* s, size_t n)
    {
      char* t = reinterpret_cast<char*>(alloc.allocate(n + 1));
      if(!t)
        return nullptr;

      tmemcpy<char>(t, n + 1, s, n + 1);
      return t;
    }
    IN_FORCEINLINE const char* AllocString(IN_WASM_ALLOCATOR& alloc, const char* s)
    {
      return !s ? nullptr : AllocString(alloc, s, strlen(s));
    }
    IN_FORCEINLINE const char* AllocString(IN_WASM_ALLOCATOR& alloc, const std::string& s)
    {
      return AllocString(alloc, s.data(), s.size());
    }

    using OpcodeInt = uint16_t;
    static_assert(sizeof(OpcodeInt) >= MAX_OPCODE_BYTES, "MAX_OPCODE_BYTES must fit inside an OpcodeInt");

    OpcodeInt GetInstruction(StringSpan s);
    varuint32 ModuleFunctionType(const Module& m, varuint32 index);
    FunctionType* ModuleFunction(const Module& m, varuint32 index);
    TableDesc* ModuleTable(const Module& m, varuint32 index);
    MemoryDesc* ModuleMemory(const Module& m, varuint32 index);
    GlobalDesc* ModuleGlobal(const Module& m, varuint32 index);
    Import* ResolveImport(const Module& m, const Export& imp);
    path GetProgramPath(const char* arg0);
    path GetWorkingDir();
    IN_COMPILER_DLLEXPORT bool SetWorkingDir(const path& path);
    path GetAbsolutePath(const path& path);
    inline path GetPath(const char8_t* utf8) { return !utf8 ? u8"" : utf8; }
    IN_COMPILER_DLLEXPORT void GetCPUInfo(uintcpuinfo& info, int flags);
    void* LoadDLL(const path& path);
    void* LoadDLLFunction(void* dll, const char* name);
    void FreeDLL(void* dll);
    IN_COMPILER_DLLEXPORT char* LoadDLLError();
    IN_COMPILER_DLLEXPORT void LoadDLLErrorFree(char* p);
    IN_COMPILER_DLLEXPORT int GetArchBits(uint8_t arch);
    IN_COMPILER_DLLEXPORT bool IsLittleEndian(uint8_t abi, uint8_t arch);

  #ifdef IN_PLATFORM_WIN32
    int Install(const char* arg0, bool full);
    int Uninstall(); 
  #endif

    // Creates a C-compatible mangled name with an optional index
    inline std::string CanonicalName(StringSpan prefix, StringSpan name, int index = -1)
    {
      static const char HEX[17] = "0123456789ABCDEF";

      std::string str;
      std::string strname(name.s, name.len);
      std::string strprefix(prefix.s, prefix.len);
      if(index >= 0)
      {
        char buf[20] = { 0 };
        snprintf(buf, 20, "%d", index);
        str = (!prefix.s ? strname + buf : (strprefix + IN_GLUE_STRING + strname + buf));
      }
      else
        str = (!prefix.s ? strname : (strprefix + IN_GLUE_STRING + strname));

      std::string canonical;
      canonical.reserve(str.capacity());

      for(unsigned char c : str)
      {
        switch(c)
        {
        case ' ': canonical += "--"; break;
        case ':': canonical += "#3A"; break;
        case '=': canonical += "#3D"; break;
        case '/': canonical += "#2F"; break;
        case '"': canonical += "#22"; break;
        case ',': canonical += "#2C"; break;
        case '@': canonical += "#40"; break;
        default:
          if(c < 32 || c > 126)
          {
            char buf[4] = { '#' };
            buf[1]      = HEX[c / 16];
            buf[2]      = HEX[c % 16];
            canonical += buf;
          }
          else
            canonical += (char)c;
          break;
        }
      }

      return canonical;
    }

    inline bool IsSystemImport(const Identifier& module_name, const char* system)
    {
      const char* module_end = strchr(module_name.str(), '!');
      return !strncmp(system, module_name.str(), !module_end ? module_name.size() : (module_end - module_name.str()));
    }

    inline std::unique_ptr<uint8_t[]> LoadFile(const path& file, size_t& sz)
    {
      FILE* f = nullptr;
      FOPEN(f, file.c_str(), "rb");
      if(!f)
        return nullptr;
      fseek(f, 0, SEEK_END);
      long pos = ftell(f);
      if(pos < 0)
      {
        fclose(f);
        return nullptr;
      }
      sz = static_cast<size_t>(pos);
      fseek(f, 0, SEEK_SET);
      std::unique_ptr<uint8_t[]> data(new uint8_t[sz]);
      sz = fread(data.get(), 1, sz, f);
      fclose(f);
      return data;
    }

    inline bool DumpFile(const path& file, const void* data, size_t sz)
    {
      FILE* f = nullptr;
      FOPEN(f, file.c_str(), "wb");
      if(!f)
        return false;
      if(fwrite(data, 1, sz, f) != sz)
        return false;
      return !fclose(f);
    }

    IN_FORCEINLINE varuint32 GetBlockSigResults(varsint64 sig, const Module& m)
    {
      switch(sig)
      {
      case TE_i32:
      case TE_i64:
      case TE_f32:
      case TE_f64: return 1;
      case TE_void: return 0;
      }

      if(sig >= 0 && sig < m.type.n_functypes)
        return m.type.functypes[sig].n_returns;

      return 0;
    }

    IN_FORCEINLINE varuint32 GetBlockSigParams(varsint64 sig, const Module& m)
    {
      if(sig >= 0 && sig < m.type.n_functypes)
        return m.type.functypes[sig].n_params;

      return 0;
    }

    IN_FORCEINLINE varsint7 GetBlockSigResult(varsint64 sig, varuint32 i, const Module& m)
    {
      switch(sig)
      {
      case TE_i32: return TE_i32;
      case TE_i64: return TE_i64;
      case TE_f32: return TE_f32;
      case TE_f64: return TE_f64;
      case TE_void: return TE_void;
      }

      if(sig >= 0 && sig < m.type.n_functypes && i < m.type.functypes[sig].n_returns)
        return m.type.functypes[sig].returns[i];

      return TE_NONE;
    }

    IN_FORCEINLINE varsint7 GetBlockSigParam(varsint64 sig, varuint32 i, const Module& m)
    {
      if(sig >= 0 && sig < m.type.n_functypes && i < m.type.functypes[sig].n_params)
        return m.type.functypes[sig].params[i];

      return TE_NONE;
    }
  }
}

#endif
