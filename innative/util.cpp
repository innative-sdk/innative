// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "util.h"
#include <assert.h>
#include <stdexcept>
#include <stdarg.h>
#include <algorithm>

#ifdef IR_PLATFORM_WIN32
#pragma pack(push)
#pragma pack(8)
#define WINVER 0x0501 //_WIN32_WINNT_WINXP   
#define _WIN32_WINNT 0x0501
#define NTDDI_VERSION 0x05010300 //NTDDI_WINXPSP3 
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX // Some compilers enable this by default
#define NOMINMAX
#endif
#define NODRAWTEXT
#define NOBITMAP
#define NOMCX
#define NOSERVICE
#define NOHELP
#define NOGDI
#include <windows.h>
#pragma pack(pop)
#include <intrin.h>
#elif defined(IR_PLATFORM_POSIX)
#include <unistd.h>
#include <cpuid.h>
#include <limits.h>
#include <dlfcn.h>
#include <sys/mman.h>
#else
#error unknown platform
#endif

using std::string;

void* __WASM_ALLOCATOR::allocate(size_t n)
{
  size_t index = cur.fetch_add(n, std::memory_order_acq_rel);
  size_t end = index + n;
  size_t max;

  while(end > (max = sz.load(std::memory_order_acquire)))
  {
    if(index <= max && end > max) // Exactly one allocation can be in this state at a time, the others will spin while waiting for the reallocation
    {
      while(commit.load(std::memory_order_acquire) != index); // Spin until all reads are done

      size_t len = std::max<size_t>(end, 4096) * 2;
      void* prev = malloc(len); // = mem.load(std::memory_order_acquire);
      list.push_back({ prev, len }); // Add real pointer and size to our destructor list
      mem.exchange((char*)prev - index, std::memory_order_release); // backtrack to trick the current index into pointing to the right address
      sz.exchange(len + index, std::memory_order_release); // Actual "end" is our previous allocation endpoint (not the memory endpoint) plus current size
    }
  }

  void* m = mem.load(std::memory_order_acquire);
  commit.fetch_add(n, std::memory_order_release);
  return (char*)m + index;
}

__WASM_ALLOCATOR::~__WASM_ALLOCATOR()
{
  mem.exchange(nullptr, std::memory_order_release);
  sz.exchange(0, std::memory_order_release);
  cur.exchange(0, std::memory_order_release);

  while(!list.empty())
  {
    free(list.back().first);
    list.pop_back();
  }
}

namespace innative {
  namespace utility {
    KHASH_INIT(opnames, StringRef, uint8_t, 1, internal::__ac_X31_hash_stringrefins, kh_int_hash_equal);

    kh_opnames_t* GenOpNames()
    {
      kh_opnames_t* h = kh_init_opnames();
      int r;

      for(int i = 0; i < OPNAMECOUNT; ++i)
      {
        if(strcmp(OPNAMES[i], "RESERVED") != 0)
        {
          khiter_t iter = kh_put_opnames(h, StringRef{ OPNAMES[i], strlen(OPNAMES[i]) }, &r);
          kh_val(h, iter) = (uint8_t)i;
        }
      }

      std::pair<const char*, const char*> legacy[] = {
        { "grow_memory", "memory.grow" },
        { "mem.grow", "memory.grow" },
        { "current_memory", "memory.size" },
        { "mem.size", "memory.size" },
      };

      for(auto& i : legacy)
      {
        khiter_t iter = kh_put_opnames(h, StringRef{ i.first, strlen(i.first) }, &r);
        kh_val(h, iter) = kh_val(h, kh_get_opnames(h, StringRef{ i.second, strlen(i.second) }));
      }

      return h;
    }

    uint8_t GetInstruction(StringRef ref)
    {
      static const kh_opnames_t* h = GenOpNames();

      khiter_t iter = kh_get_opnames(h, ref);
      return kh_exist2(h, iter) ? kh_val(h, iter) : (uint8_t)0xFF;
    }

    varuint32 ModuleFunctionType(const Module& m, varuint32 index)
    {
      if(index < m.importsection.functions)
        return m.importsection.imports[index].func_desc.type_index;
      index -= m.importsection.functions;
      if(index < m.function.n_funcdecl)
        return m.function.funcdecl[index];
      return (varuint32)~0;
    }

    FunctionType* ModuleFunction(const Module& m, varuint32 index)
    {
      if(index < m.importsection.functions)
        return &m.type.functions[m.importsection.imports[index].func_desc.type_index];
      index -= m.importsection.functions;
      if(index < m.function.n_funcdecl)
        return &m.type.functions[m.function.funcdecl[index]];
      return nullptr;
    }
    TableDesc* ModuleTable(const Module& m, varuint32 index)
    {
      size_t i = index + m.importsection.functions; // Shift index to table section
      if(i < m.importsection.tables)
        return &m.importsection.imports[i].table_desc;
      i -= m.importsection.tables;
      if(i < m.table.n_tables)
        return &m.table.tables[i];
      return nullptr;
    }
    MemoryDesc* ModuleMemory(const Module& m, varuint32 index)
    {
      size_t i = index + m.importsection.tables; // Shift index to memory section
      if(i < m.importsection.memories)
        return &m.importsection.imports[i].mem_desc;
      i -= m.importsection.memories;
      if(i < m.memory.n_memories)
        return &m.memory.memories[i];
      return nullptr;
    }
    GlobalDesc* ModuleGlobal(const Module& m, varuint32 index)
    {
      size_t i = index + m.importsection.memories; // Shift index to globals section
      if(i < m.importsection.globals)
        return &m.importsection.imports[i].global_desc;
      i -= m.importsection.globals;
      if(i < m.global.n_globals)
        return &m.global.globals[i].desc;
      return nullptr;
    }
    //Export* ModuleExport(const Module& m, varuint32 index, WASM_KIND kind)
    //{
    //  for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
    //    if(m.exportsection.exports[i].kind == kind && m.exportsection.exports[i].index == index)
    //      return m.exportsection.exports + i;
    //  return 0;
    //}
    Import* ResolveImport(const Module& m, const Export& exp)
    {
      switch(exp.kind)
      {
      case WASM_KIND_FUNCTION:
        if(exp.index < m.importsection.functions)
          return &m.importsection.imports[exp.index];
        break;
      case WASM_KIND_TABLE:
        if(exp.index < (m.importsection.tables - m.importsection.functions))
          return &m.importsection.imports[exp.index + m.importsection.functions];
        break;
      case WASM_KIND_MEMORY:
        if(exp.index < (m.importsection.memories - m.importsection.tables))
          return &m.importsection.imports[exp.index + m.importsection.tables];
        break;
      case WASM_KIND_GLOBAL:
        if(exp.index < (m.importsection.globals - m.importsection.memories))
          return &m.importsection.imports[exp.index + m.importsection.memories];
        break;
      }

      return nullptr;
    }

    std::pair<Module*, Export*> ResolveExport(const Environment& env, const Import& imp)
    {
      khint_t iter = kh_get_modules(env.modulemap, imp.module_name);
      if(iter == kh_end(env.modulemap))
        return { nullptr,nullptr };

      size_t i = kh_value(env.modulemap, iter);
      if(i >= env.n_modules)
        return { nullptr,nullptr };

      iter = kh_get_exports(env.modules[i].exports, imp.export_name);
      if(iter == kh_end(env.modules[i].exports))
        return { env.modules + i, 0 };

      varuint32 j = kh_value(env.modules[i].exports, iter);
      if(j >= env.modules[i].exportsection.n_exports)
        return { env.modules + i, 0 };

      return { env.modules + i, env.modules[i].exportsection.exports + j };
    }

    std::pair<Module*, Export*> ResolveTrueExport(const Environment& env, const Import& init)
    {
      const Import* imp = &init;
      auto pair = ResolveExport(env, *imp);

      while(imp = ResolveImport(*pair.first, *pair.second))
        pair = ResolveExport(env, *imp);

      return pair;
    }

    Path GetProgramPath(const char* arg0)
    {
      string buf;
#ifdef IR_PLATFORM_WIN32
      buf.resize(MAX_PATH);
      buf.resize(GetModuleFileNameA(NULL, const_cast<char*>(buf.data()), (DWORD)buf.capacity()));
#else
      return Path(std::move(GetAbsolutePath(arg0)));
#endif
      return Path(std::move(buf));
    }

    Path GetWorkingDir()
    {
      string buf;
#ifdef IR_PLATFORM_WIN32
      buf.resize(GetCurrentDirectoryA(0, 0));
      buf.resize(GetCurrentDirectoryA((DWORD)buf.capacity(), const_cast<char*>(buf.data())));
#elif defined(IR_PLATFORM_POSIX)
      buf.resize(PATH_MAX);
      getcwd(const_cast<char*>(buf.data()), buf.capacity());
      buf.resize(strlen(buf.data()));
#else
#error unknown platform
#endif
      return Path(std::move(buf));
    }

    Path GetAbsolutePath(const char* path)
    {
      string buf;
#ifdef IR_PLATFORM_WIN32
      buf.resize(GetFullPathNameA(path, 0, 0, 0));
      buf.resize(GetFullPathNameA(path, (DWORD)buf.capacity(), const_cast<char*>(buf.data()), 0));
#elif defined(IR_PLATFORM_POSIX)
      buf.resize(PATH_MAX);
      const char* resolve = realpath(path, const_cast<char*>(buf.data()));
      if(!resolve)
        return Path(path);
      buf.resize(strlen(resolve));
#else
#error unknown platform
#endif
      return Path(std::move(buf));
    }
    bool SetWorkingDir(const char* path)
    {
#ifdef IR_PLATFORM_WIN32
      return SetCurrentDirectoryA(path) != 0;
#elif defined(IR_PLATFORM_POSIX)
      return chdir(path) != 0;
#endif
    }

    void GetCPUInfo(uintcpuinfo& info, int flags)
    {
#ifdef IR_PLATFORM_WIN32
      SYSTEM_INFO sysinfo;
      GetSystemInfo(&sysinfo);
      info[4] = sysinfo.wProcessorArchitecture | (flags << 16);
      __cpuid(info, 1);
#elif defined(IR_PLATFORM_POSIX)
      info[4] = (flags << 16);
      __get_cpuid(1, info + 0, info + 1, info + 2, info + 3);
#endif
    }

    string StrFormat(const char* fmt, ...)
    {
      va_list args;
      string s;
      va_start(args, fmt);
      s.resize(vsnprintf(0, 0, fmt, args) + 1);
      va_end(args);
      va_start(args, fmt);
      s.resize(vsnprintf((char*)s.data(), s.capacity(), fmt, args));
      va_end(args);

      return s;
    }

#ifdef IR_PLATFORM_WIN32
    void* LoadDLL(const char* path) { return LoadLibraryA(path); }
    void* LoadDLLFunction(void* dll, const char* name) { return GetProcAddress((HMODULE)dll, name); }
    void FreeDLL(void* dll) { FreeLibrary((HMODULE)dll); }
#elif defined(IR_PLATFORM_POSIX)
    void* LoadDLL(const char* path) { return dlopen(path, RTLD_NOW); } // We MUST load and initialize WASM dlls immediately for init function testing
    void* LoadDLLFunction(void* dll, const char* name) { return dlsym(dll, name); }
    void FreeDLL(void* dll) { dlclose(dll); }
#endif

    int install()
    {
#ifdef IR_PLATFORM_WIN32
      // Install detailed information to registry

      // Set %INNATIVE_PATH% to our path only if we are the newest version

      // Register ourselves as handling *.wast *.wat and *.wasm files
#elif defined(IR_PLATFORM_POSIX)
      // Install symlinks to /usr/bin

      // Calculate new master symlink
#endif
      return -1;
    }

    int uninstall()
    {
#ifdef IR_PLATFORM_WIN32
      // Remove this version from the registry

      // Recalculate %INNATIVE_PATH%

      // Remove our file handling registration
#elif defined(IR_PLATFORM_POSIX)
      // Remove symlink from /usr/bin

      // Calculate new master symlink
#endif
      return -1;
    }
  }
}