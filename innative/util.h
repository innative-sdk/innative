// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __UTIL_H__IR__
#define __UTIL_H__IR__

#include "innative/schema.h"
#include "innative/path.h"
#include "constants.h"
#include <string>
#include <math.h>
#include <exception>
#include <assert.h>
#include <utility>
#include <memory>
#include <atomic>
#include <vector>

struct __WASM_ALLOCATOR
{
  __WASM_ALLOCATOR() : mem(0), sz(0), cur(0), commit(0) {}
  ~__WASM_ALLOCATOR();

  std::atomic<void*> mem;
  std::atomic_size_t sz;
  std::atomic_size_t cur;
  std::atomic_size_t commit;
  std::vector<std::pair<void*, size_t>> list;

  void* allocate(size_t n);
};

extern "C" int64_t GetRSPValue();

namespace innative {
  namespace utility {
#ifdef IR_PLATFORM_WIN32
    typedef int uintcpuinfo[5];
#else
    typedef unsigned int uintcpuinfo[5];
#endif

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
  }

  namespace internal {
    // For simplicity reasons, we assemble the error list backwards. This reverses it so it appears in the correct order.
    inline void ReverseErrorList(ValidationError*& errors) noexcept
    {
      auto cur = errors;
      ValidationError* prev = nullptr;
      while(cur != nullptr)
      {
        auto next = cur->next;
        cur->next = prev;
        prev = cur;
        cur = next;
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

    inline khint_t __ac_X31_hash_stringrefins(utility::StringRef ref)
    {
      const char* s = ref.s;
      const char* end = ref.s + ref.len;
      khint_t h = ((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s;
      if(h)
        for(++s; s < end; ++s)
          h = (h << 5) - h + (((*s) > 64 && (*s) < 91) ? (*s) + 32 : *s);
      return h;
    }

    inline khint_t __ac_X31_hash_bytearray(const ByteArray& id)
    {
      const char* s = (const char*)id.get();
      const char* end = s + id.size();
      khint_t h = 0;
      if(s < end)
      {
        h = *s;
        for(++s; s < end; ++s)
          h = (h << 5) - h + (khint_t)*s;
      }
      return h;
    }

    // Hashes a pair of strings seperated by a null terminator
    kh_inline khint_t __ac_X31_hash_string_pair(const char *s)
    {
      khint_t h = (khint_t)*s;
      if(h) for(++s; *s; ++s) h = (h << 5) - h + (khint_t)*s;
      for(++s; *s; ++s) h = (h << 5) - h + (khint_t)*s;
      return h;
    }
  }

  namespace utility {
    template<class F>
    class DeferLambda
    {
    public:
      inline DeferLambda(const F& f) : _f(f) {}
      inline ~DeferLambda() { _f(); }

    protected:
      F _f;
    };

    template<class T>
    inline void tmemcpy(T* dest, size_t destsize, const T* src, size_t srcsize)
    {
#ifdef IR_COMPILER_MSC
      memcpy_s(dest, destsize * sizeof(T), src, srcsize * sizeof(T));
#else
      memcpy(dest, src, srcsize * sizeof(T));
#endif
    }

    template<class T>
    inline T* trealloc(T* p, size_t sz)
    {
      return reinterpret_cast<T*>(realloc(p, sz * sizeof(T)));
    }

    template<class T>
    inline T* tmalloc(const Environment& env, size_t n)
    {
      return reinterpret_cast<T*>(env.alloc->allocate(n * sizeof(T)));
    }

    IR_FORCEINLINE bool ModuleHasSection(const Module& m, varuint7 opcode) { return (m.knownsections&(1 << opcode)) != 0; }

    uint8_t GetInstruction(StringRef s);
    varuint32 ModuleFunctionType(const Module& m, varuint32 index);
    FunctionType* ModuleFunction(const Module& m, varuint32 index);
    TableDesc* ModuleTable(const Module& m, varuint32 index);
    MemoryDesc* ModuleMemory(const Module& m, varuint32 index);
    GlobalDesc* ModuleGlobal(const Module& m, varuint32 index);
    std::pair<Module*, Export*> ResolveExport(const Environment& env, const Import& imp);
    std::pair<Module*, Export*> ResolveTrueExport(const Environment& env, const Import& imp);
    Import* ResolveImport(const Module& m, const Export& imp);
    Path GetProgramPath(const char* arg0);
    Path GetWorkingDir();
    bool SetWorkingDir(const char* path);
    Path GetAbsolutePath(const char* path);
    std::string StrFormat(const char* fmt, ...);
    void GetCPUInfo(uintcpuinfo& info, int flags);
    void* LoadDLL(const char* path);
    void* LoadDLLFunction(void* dll, const char* name);
    void FreeDLL(void* dll);
    int Install(const char* arg0, bool full);
    int Uninstall();
    bool RestoreStackGuard(void* lpPage);

    // Creates a C-compatible mangled name with an optional index
    inline std::string CanonicalName(const char* prefix, const char* name, int index = -1)
    {
      static const char HEX[17] = "0123456789ABCDEF";

      std::string str;
      if(index >= 0)
      {
        char buf[20] = { 0 };
        snprintf(buf, 20, "%d", index);
        str = (!prefix ? std::string(name) + buf : (std::string(prefix) + IR_GLUE_STRING + name + buf));
      }
      else
        str = (!prefix ? name : (std::string(prefix) + IR_GLUE_STRING + name));

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
            buf[1] = HEX[c / 16];
            buf[2] = HEX[c % 16];
            canonical += buf;
          }
          else
            canonical += (char)c;
          break;
        }
      }

      return canonical;
    }

    // Generates the correct mangled C function name
    inline std::string CanonImportName(const Import& imp)
    {
      if(!imp.module_name.size() || memchr(imp.module_name.get(), '!', imp.module_name.size()) != nullptr) // blank imports or imports with a calling convention are always raw function names
        return CanonicalName(0, imp.export_name.str());
      return CanonicalName(imp.module_name.str(), imp.export_name.str());
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

    inline std::unique_ptr<uint8_t[]> LoadFile(const char* file, long& sz)
    {
      FILE* f = nullptr;
      FOPEN(f, file, "rb");
      if(!f)
        return nullptr;
      fseek(f, 0, SEEK_END);
      sz = ftell(f);
      fseek(f, 0, SEEK_SET);
      std::unique_ptr<uint8_t[]> data(new uint8_t[sz]);
      sz = (long)fread(data.get(), 1, sz, f);
      fclose(f);
      return data;
    }
  }
}

#endif