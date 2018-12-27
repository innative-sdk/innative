// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "util.h"
#include "innative/export.h"
#include <assert.h>
#include <stdexcept>
#include <stdarg.h>
#include <algorithm>

#ifdef IR_PLATFORM_WIN32
#include "../innative/win32.h"
#include <intrin.h>
#elif defined(IR_PLATFORM_POSIX)
#include <unistd.h>
#include <cpuid.h>
#include <limits.h>
#include <dlfcn.h>
#include <sys/mman.h>
#include <dirent.h>
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

#define MAKEWSTRING2(x) L#x
#define MAKEWSTRING(x) MAKEWSTRING2(x)
#define IR_VERSION_PATH MAKEWSTRING(INNATIVE_VERSION_MAJOR) L"\\" MAKEWSTRING(INNATIVE_VERSION_MINOR) L"\\" MAKEWSTRING(INNATIVE_VERSION_REVISION)
#define IR_WIN32_APPS L"SOFTWARE\\Classes\\Applications\\"
#define IR_WIN32_CLASSPATH IR_WIN32_APPS L"innative-cmd.exe"
#define IR_WIN32_CLSID L"25A4AE98-4F30-4735-B29B-37B6EDC5A9E9"

    bool Win32SetKey(HKEY hive, const wchar_t* key, const wchar_t* value, const wchar_t* data)
    {
      DWORD	dwDisposition = 0;
      HKEY hTempKey = (HKEY)0;
      bool ret = false;
      if(ERROR_SUCCESS == ::RegCreateKeyExW(hive, key, 0, 0, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, 0, &hTempKey, &dwDisposition))
      {
        if(!data || ::RegSetValueExW(hTempKey, value, 0, REG_SZ, (LPBYTE)data, ((DWORD)wcslen(data) + 1) * sizeof(wchar_t)) == ERROR_SUCCESS)
          ret = true;
      }

      if(hTempKey)
        ::RegCloseKey(hTempKey);

      return ret;
    };

    bool Win32DeleteKeyValue(HKEY hive, const wchar_t* key, const wchar_t* value)
    {
      HKEY hTempKey = (HKEY)0;
      bool ret = false;

      if(ERROR_SUCCESS == ::RegOpenKeyExW(hive, key, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, &hTempKey))
      {
        if(::RegDeleteValueW(hTempKey, value) == ERROR_SUCCESS)
          ret = true;
      }

      if(hTempKey)
        ::RegCloseKey(hTempKey);

      return ret;
    }

    template<typename F>
    bool Win32EnumKey(HKEY hive, const wchar_t* key, F && f)
    {
      HKEY hKey;
      if(RegOpenKeyExW(hive, key, 0, KEY_READ, &hKey) != ERROR_SUCCESS)
        return false;

      FILETIME ftWrite;
      DWORD dwSize = MAX_PATH;
      wchar_t szName[MAX_PATH];
      DWORD index = 0;
      LSTATUS lResult = RegEnumKeyExW(hKey, index, szName, &dwSize, nullptr, nullptr, nullptr, &ftWrite);

      while(lResult == ERROR_SUCCESS)
      {
        f(szName);
        dwSize = MAX_PATH;
        lResult = RegEnumKeyExW(hKey, ++index, szName, &dwSize, nullptr, nullptr, nullptr, &ftWrite);
      }

      RegCloseKey(hKey);
      return true;
    }

    bool Win32Install(uint64_t version, bool full)
    {
      std::wstring path(IR_WIN32_REGPATH);
      path += L"\\" + std::to_wstring((version >> 48) & 0xFFFF);
      path += L"\\" + std::to_wstring((version >> 32) & 0xFFFF);
      path += L"\\" + std::to_wstring((version >> 16) & 0xFFFF);

      HKEY hKey;
      if(RegOpenKeyExW(HKEY_CURRENT_USER, path.c_str(), 0, KEY_READ | KEY_QUERY_VALUE, &hKey) != ERROR_SUCCESS)
        return false;

      DWORD len = 0;
      RegQueryValueExW(hKey, 0, 0, 0, 0, &len);
      std::unique_ptr<wchar_t[]> exe(new wchar_t[(len + 1) / 2]);
      if(RegQueryValueExW(hKey, 0, 0, 0, (LPBYTE)exe.get(), &len) != ERROR_SUCCESS)
        return false;

      RegQueryValueExW(hKey, L"runtime", 0, 0, 0, &len);
      std::unique_ptr<wchar_t[]> runtime(new wchar_t[(len + 1) / 2]);
      if(RegQueryValueExW(hKey, L"runtime", 0, 0, (LPBYTE)runtime.get(), &len) != ERROR_SUCCESS)
        return false;

      if(!Win32SetKey(HKEY_CURRENT_USER, L"Environment", L"INNATIVE_PATH", runtime.get()))
        return false;
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_REGPATH, nullptr, exe.get()))
        return false;
      if(!full)
        return true;

      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH, L"FriendlyAppName", L"inNative Runtime"))
        return false;
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH, L"IsHostApp", L""))
        return false;
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH, L"UseExecutableForTaskbarGroupIcon", L""))
        return false;

      auto cmdline = L'"' + std::wstring(exe.get()) + L"\" \"%1\"";
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\shell\\open\\command", nullptr, cmdline.c_str()))
        return false;

      
      // Set file type handlers
      if(!Win32SetKey(HKEY_CURRENT_USER, L"Software\\Classes\\CLSID\\{" IR_WIN32_CLSID L"}", nullptr, L"inNative Runtime"))
        return false;

      auto regtype = [](const wchar_t* cmdline, const wchar_t* progid, const wchar_t* ext, const wchar_t* name) -> bool {
        auto id = std::wstring(progid) + ext + L"." + MAKEWSTRING(INNATIVE_VERSION_MAJOR);
         
        if(!Win32SetKey(HKEY_CURRENT_USER, (IR_WIN32_APPS + id).c_str(), nullptr, L"WebAssembly Script"))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, (IR_WIN32_APPS + id + L"\\shell\\open\\command").c_str(), nullptr, cmdline))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\SupportedTypes", ext, id.c_str()))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, (std::wstring(L"Software\\Classes\\") + ext + L"\\OpenWithProgids").c_str(), id.c_str(), L""))
          return false;
        return true;
      };

      if(!regtype(cmdline.c_str(), L"innative-cmd", L".wast", L"WebAssembly Script"))
        return false;
      if(!regtype(cmdline.c_str(), L"innative-cmd", L".wat", L"WebAssembly Text Module"))
        return false;
      if(!regtype(cmdline.c_str(), L"innative-cmd", L".wasm", L"WebAssembly Module"))
        return false;

      return true;
    }

#elif defined(IR_PLATFORM_POSIX)
    void* LoadDLL(const char* path) { return dlopen(path, RTLD_NOW); } // We MUST load and initialize WASM dlls immediately for init function testing
    void* LoadDLLFunction(void* dll, const char* name) { return dlsym(dll, name); }
    void FreeDLL(void* dll) { dlclose(dll); }

#define POSIX_LIB_BASE "/usr/lib/libinnative.so"
#define POSIX_LIB_PATH POSIX_LIB_BASE "." MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)

    int FindLatestVersion(const std::string& prefix, const std::vector<std::string>& files)
    {
      int v = -1;

      for(auto f : files)
      {
        if(f.size() > prefix.size() && !strnicmp(f.c_str(), prefix.c_str(), prefix.size()))
        {
          auto s = f.substr(prefix.size() + 1);

          if(s.find_first_of('.') == std::string::npos)
          {
            char* end;
            v = std::max<int>(v, strtol(s.c_str(), &end, 10));
          }
        }
      }

      return v;
    }

    bool GenSymlink(const std::vector<std::string>& files, int major, int minor)
    {
      std::string link(POSIX_LIB_BASE);
      std::string src(POSIX_LIB_BASE);
      if(major >= 0)
      {
        link += '.';
        link += std::to_string(major);
      }
      else
        major = FindLatestVersion(src, files);

      src += '.';
      src += std::to_string(major);

      if(minor >= 0)
      {
        link += '.';
        link += std::to_string(minor);
      }
      else
        minor = FindLatestVersion(src, files);

      src += '.';
      src += std::to_string(minor);
      int revision = FindLatestVersion(src, files);
      
      if(revision < 0 || minor < 0 || major < 0)
        return unlink(link.c_str()) != 0; // No version exists here, so just delete it

      unlink(link.c_str());
      auto src = POSIX_LIB_BASE "." + std::to_string(major) + "." + std::to_string(minor) + "." + std::to_string(revision);
      return symlink(src.c_str(), link.c_str()) != 0;
    }

    // Finds the latest version for each level: revision, minor, major. Then sets the /usr/bin symlink to the executable corresponding to the latest version
    bool UpdateSymlinks()
    {
      std::vector<std::string> files;

      DIR* pdir;
      if((pdir = opendir("/usr/lib/")) == nullptr)
        return false;

      struct dirent *d;
      while((d = readdir(pdir)) != NULL)
      {
        if(strlen(d->d_name) >= sizeof("libinnative.so") && !strnicmp(d->d_name, "libinnative.so", sizeof("libinnative.so")))
          files.emplace_back(d->d_name);
      }

      closedir(pdir);
      
      if(!GenSymlink(files, INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR))
        return false;

      if(!GenSymlink(files, INNATIVE_VERSION_MAJOR, -1))
        return false;

      return GenSymlink(files, -1, -1);
    }
#endif

    uint64_t GetLatestVersion()
    {
      uint16_t major = 0;
      uint16_t minor = 0;
      uint16_t revision = 0;
      uint16_t build = 0;

#ifdef IR_PLATFORM_WIN32
      if(!Win32EnumKey(HKEY_CURRENT_USER,
        IR_WIN32_REGPATH,
        [&major](const wchar_t* s) { wchar_t *end; major = std::max((uint16_t)wcstol(s, &end, 10), major); }))
        return 0;

      if(!Win32EnumKey(HKEY_CURRENT_USER,
        (IR_WIN32_REGPATH L"\\" + std::to_wstring(major)).c_str(),
        [&minor](const wchar_t* s) { wchar_t *end; minor = std::max((uint16_t)wcstol(s, &end, 10), minor); }))
        return 0;

      if(!Win32EnumKey(HKEY_CURRENT_USER,
        (IR_WIN32_REGPATH L"\\" + std::to_wstring(major) + L"\\" + std::to_wstring(minor)).c_str(),
        [&revision](const wchar_t* s) { wchar_t *end; revision = std::max((uint16_t)wcstol(s, &end, 10), revision); }))
        return 0;

      HKEY hKey;
      if(RegOpenKeyExW(HKEY_CURRENT_USER, (IR_WIN32_REGPATH L"\\" + std::to_wstring(major) + L"\\" + std::to_wstring(minor) + L"\\" + std::to_wstring(revision)).c_str(), 0, KEY_READ, &hKey) != ERROR_SUCCESS)
        return 0;
      RegCloseKey(hKey);
#elif defined(IR_PLATFORM_POSIX)

#endif
      return INNATIVE_VERSION(major, minor, revision, build);
    }

    int Install(const char* arg0, bool full)
    {
#ifdef IR_PLATFORM_WIN32
      // Open our base registry entry and install this specific version info (overwrite any existing info)
      std::wstring path;
      path.resize(MAX_PATH);
      path.resize(GetModuleFileNameW(NULL, const_cast<wchar_t*>(path.data()), (DWORD)path.capacity()));
      std::wstring buf;
      buf.resize(GetFullPathNameW(path.c_str(), 0, 0, 0));
      buf.resize(GetFullPathNameW(path.c_str(), (DWORD)buf.capacity(), const_cast<wchar_t*>(buf.data()), 0));
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_REGPATH L"\\" IR_VERSION_PATH, nullptr, buf.c_str()))
        return -1;
      std::wstring runtime = buf.substr(0, buf.find_last_of(L'\\')) + L"\\innative.dll";
      if(!Win32SetKey(HKEY_CURRENT_USER, IR_WIN32_REGPATH L"\\" IR_VERSION_PATH, L"runtime", runtime.c_str()))
        return -2;

      constexpr uint64_t cur = INNATIVE_VERSION(INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR, INNATIVE_VERSION_REVISION, 0);
      if(GetLatestVersion() <= cur) // Only if we are the latest version do we perform a full install
        if(!Win32Install(cur, full))
          return -3;
      
#elif defined(IR_PLATFORM_POSIX)
      Path path = GetProgramPath(arg0).BaseDir().Append("/libinnative.so");

      // Install symlinks to /usr/lib
      if(symlink(path.c_str(), POSIX_LIB_PATH) != 0)
        return -4;

      // Calculate new master symlinks for lib and exe
      if(!UpdateSymlinks())
        return -5;
#endif
      return 0;
    }

    int Uninstall()
    {
#ifdef IR_PLATFORM_WIN32
      // Remove this version from the registry by deleting all version levels. Only the levels that have no more subkeys will actually be deleted.
      uint64_t oldversion = GetLatestVersion();
      bool r = RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_REGPATH L"\\" IR_VERSION_PATH) == ERROR_SUCCESS;
      RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_REGPATH L"\\" MAKEWSTRING(INNATIVE_VERSION_MAJOR) L"\\" MAKEWSTRING(INNATIVE_VERSION_MINOR));
      RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_REGPATH L"\\" MAKEWSTRING(INNATIVE_VERSION_MAJOR));

      HKEY hKey = 0;
      bool full = RegOpenKeyExW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH, 0, KEY_READ, &hKey) == ERROR_SUCCESS;
      if(hKey)
        RegCloseKey(hKey);

      uint64_t newversion = GetLatestVersion();
      if(!newversion) // That was the last version, so wipe all registry keys
      {
        r &= Win32DeleteKeyValue(HKEY_CURRENT_USER, L"Environment", L"INNATIVE_PATH") == ERROR_SUCCESS;
        r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_REGPATH) == ERROR_SUCCESS; // This should work because we should have no more version subkeys
        if(full)
        {
          r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\shell\\open\\command") == ERROR_SUCCESS;
          r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\shell\\open") == ERROR_SUCCESS;
          r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\shell") == ERROR_SUCCESS;
          r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH L"\\SupportedTypes") == ERROR_SUCCESS;
          r &= RegDeleteKeyW(HKEY_CURRENT_USER, IR_WIN32_CLASSPATH) == ERROR_SUCCESS;

          auto deregtype = [](bool& r, const wchar_t* progid, const wchar_t* ext) {
            auto id = std::wstring(progid) + ext + L"." + MAKEWSTRING(INNATIVE_VERSION_MAJOR);

            r &= RegDeleteKeyW(HKEY_CURRENT_USER, (IR_WIN32_APPS + id + L"\\shell\\open\\command").c_str()) == ERROR_SUCCESS;
            r &= RegDeleteKeyW(HKEY_CURRENT_USER, (IR_WIN32_APPS + id + L"\\shell\\open").c_str()) == ERROR_SUCCESS;
            r &= RegDeleteKeyW(HKEY_CURRENT_USER, (IR_WIN32_APPS + id + L"\\shell").c_str()) == ERROR_SUCCESS;
            r &= RegDeleteKeyW(HKEY_CURRENT_USER, (IR_WIN32_APPS + id).c_str()) == ERROR_SUCCESS;
            r &= Win32DeleteKeyValue(HKEY_CURRENT_USER, (std::wstring(L"Software\\Classes\\") + ext + L"\\OpenWithProgids").c_str(), id.c_str());
          };

          deregtype(r, L"innative-cmd", L".wast");
          deregtype(r, L"innative-cmd", L".wat");
          deregtype(r, L"innative-cmd", L".wasm");
        }
    }
      else if(newversion != oldversion) // Otherwise, overwrite current registry entries with new version
        if(!Win32Install(newversion, full))
          return -9;

      return r ? 0 : -1;
#elif defined(IR_PLATFORM_POSIX)
      // Remove symlink from /usr/lib
      if(unlink(POSIX_LIB_PATH) != 0)
        return -1;

      // Calculate new symlinks
      if(!UpdateSymlinks())
        return -2;
      return 0;
#endif
  }
}
    }