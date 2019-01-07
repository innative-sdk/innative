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

namespace innative {
  namespace utility {
#ifdef IR_PLATFORM_WIN32
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

      auto cmdline = L'"' + std::wstring(exe.get()) + L"\" \"%1\" -r -fo3";
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
#define POSIX_LIB_BASE "/usr/lib/libinnative.so"
#define POSIX_LIB_PATH POSIX_LIB_BASE "." MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)

    int FindLatestVersion(const std::string& prefix, const std::vector<std::string>& files)
    {
      int v = -1;

      for(auto f : files)
      {
        if(f.size() > prefix.size() && !strncasecmp(f.c_str(), prefix.c_str(), prefix.size()))
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
      auto origin = POSIX_LIB_BASE "." + std::to_string(major) + "." + std::to_string(minor) + "." + std::to_string(revision);
      return symlink(origin.c_str(), link.c_str()) != 0;
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
        if(strlen(d->d_name) >= sizeof("libinnative.so") && !strncasecmp(d->d_name, "libinnative.so", sizeof("libinnative.so")))
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
      Path path = GetProgramPath(arg0).BaseDir() += "/libinnative.so";

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