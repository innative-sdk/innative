// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "utility.h"
#include "innative/export.h"
#include <assert.h>
#include <stdexcept>
#include <stdarg.h>
#include <algorithm>

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
  #include <intrin.h>
#elif defined(IN_PLATFORM_POSIX)
  #include <unistd.h>
  #include <cpuid.h>
  #include <limits.h>
  #include <dlfcn.h>
  #include <sys/mman.h>
  #include <dirent.h>
  #include <fstream>
  #include <sys/stat.h>
#else
  #error unknown platform
#endif

namespace innative {
  namespace utility {
#ifdef IN_PLATFORM_WIN32
  #define MAKEWSTRING2(x) L#x
  #define MAKEWSTRING(x)  MAKEWSTRING2(x)
  #define IN_VERSION_PATH               \
    MAKEWSTRING(INNATIVE_VERSION_MAJOR) \
    L"\\" MAKEWSTRING(INNATIVE_VERSION_MINOR) L"\\" MAKEWSTRING(INNATIVE_VERSION_REVISION)
  #define IN_WIN32_APPS      L"SOFTWARE\\Classes\\Applications\\"
  #define IN_WIN32_CLASSPATH IN_WIN32_APPS L"innative-cmd.exe"
  #define IN_WIN32_CLSID     L"25A4AE98-4F30-4735-B29B-37B6EDC5A9E9"

    bool Win32SetKey(HKEY hive, const wchar_t* key, const wchar_t* value, const wchar_t* data)
    {
      DWORD dwDisposition = 0;
      HKEY hTempKey       = (HKEY)0;
      bool ret            = false;
      if(ERROR_SUCCESS ==
         ::RegCreateKeyExW(hive, key, 0, 0, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, 0, &hTempKey, &dwDisposition))
      {
        if(!data || ::RegSetValueExW(hTempKey, value, 0, REG_SZ, (LPBYTE)data,
                                     ((DWORD)wcslen(data) + 1) * sizeof(wchar_t)) == ERROR_SUCCESS)
          ret = true;
      }

      if(hTempKey)
        ::RegCloseKey(hTempKey);

      return ret;
    };

    bool Win32DeleteKeyValue(HKEY hive, const wchar_t* key, const wchar_t* value)
    {
      HKEY hTempKey = (HKEY)0;
      bool ret      = false;

      if(ERROR_SUCCESS == ::RegOpenKeyExW(hive, key, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, &hTempKey))
      {
        LSTATUS status = ::RegDeleteValueW(hTempKey, value);
        if(status == ERROR_SUCCESS || status == ERROR_FILE_NOT_FOUND)
          ret = true;
      }

      if(hTempKey)
        ::RegCloseKey(hTempKey);

      return ret;
    }

    template<typename F> bool Win32EnumKey(HKEY hive, const wchar_t* key, F&& f)
    {
      HKEY hKey;
      if(RegOpenKeyExW(hive, key, 0, KEY_READ, &hKey) != ERROR_SUCCESS)
        return false;

      FILETIME ftWrite;
      DWORD dwSize = MAX_PATH;
      wchar_t szName[MAX_PATH];
      DWORD index     = 0;
      LSTATUS lResult = RegEnumKeyExW(hKey, index, szName, &dwSize, nullptr, nullptr, nullptr, &ftWrite);

      while(lResult == ERROR_SUCCESS)
      {
        f(szName);
        dwSize  = MAX_PATH;
        lResult = RegEnumKeyExW(hKey, ++index, szName, &dwSize, nullptr, nullptr, nullptr, &ftWrite);
      }

      RegCloseKey(hKey);
      return true;
    }

    bool Win32Install(uint64_t version, bool full)
    {
      std::wstring target(IN_WIN32_REGPATH);
      target += L"\\" + std::to_wstring((version >> 48) & 0xFFFF);
      target += L"\\" + std::to_wstring((version >> 32) & 0xFFFF);
      target += L"\\" + std::to_wstring((version >> 16) & 0xFFFF);

      HKEY hKey;
      if(RegOpenKeyExW(HKEY_CURRENT_USER, target.c_str(), 0, KEY_READ | KEY_QUERY_VALUE, &hKey) != ERROR_SUCCESS)
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
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_REGPATH, nullptr, exe.get()))
        return false;
      if(!full)
        return true;

      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH, L"FriendlyAppName", L"inNative Runtime"))
        return false;
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH, L"IsHostApp", L""))
        return false;
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH, L"UseExecutableForTaskbarGroupIcon", L""))
        return false;

      auto cmdline = L'"' + std::wstring(exe.get()) + L"\" \"%1\" -r -f o3";
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH L"\\shell\\open\\command", nullptr, cmdline.c_str()))
        return false;

      // Set file type handlers
      if(!Win32SetKey(HKEY_CURRENT_USER, L"Software\\Classes\\CLSID\\{" IN_WIN32_CLSID L"}", nullptr, L"inNative Runtime"))
        return false;

      auto regtype = [](const wchar_t* cmdline, const wchar_t* progid, const wchar_t* ext, const wchar_t* name) -> bool {
        auto id = std::wstring(progid) + ext + L"." + MAKEWSTRING(INNATIVE_VERSION_MAJOR);

        if(!Win32SetKey(HKEY_CURRENT_USER, (IN_WIN32_APPS + id).c_str(), nullptr, L"WebAssembly Script"))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, (IN_WIN32_APPS + id + L"\\shell\\open\\command").c_str(), nullptr, cmdline))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH L"\\SupportedTypes", ext, id.c_str()))
          return false;
        if(!Win32SetKey(HKEY_CURRENT_USER, (std::wstring(L"Software\\Classes\\") + ext + L"\\OpenWithProgids").c_str(),
                        id.c_str(), L""))
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

    bool DeleteRegKey(const wchar_t* s)
    {
      LSTATUS status = RegDeleteKeyW(HKEY_CURRENT_USER, s);
      return status == ERROR_SUCCESS || status == ERROR_FILE_NOT_FOUND;
    };

#elif defined(IN_PLATFORM_POSIX)
  #define POSIX_VERSION_STR \
    "." MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION)
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

    bool GenSymlink(const char* file, const char* folder, int major, int minor)
    {
      std::vector<std::string> files;

      {
        size_t len = strlen(file);
        DIR* pdir;
        if((pdir = opendir(folder)) == nullptr)
          return -18;

        struct dirent* d;
        while((d = readdir(pdir)) != NULL)
        {
          if(strlen(d->d_name) >= len && !strncasecmp(d->d_name, file, len))
            files.emplace_back(d->d_name);
        }

        closedir(pdir);
      }

      std::string link(folder);
      link += file;
      std::string src(file);

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
        return !unlink(link.c_str()); // No version exists here, so just delete it

      unlink(link.c_str());
      auto origin = std::string(folder) + file + "." + std::to_string(major) + "." + std::to_string(minor) + "." +
                    std::to_string(revision);
      return !symlink(origin.c_str(), link.c_str());
    }

    // Finds the latest version for each level: revision, minor, major. Then sets the /usr/bin symlink to the executable
    // corresponding to the latest version
    int UpdateSymlinks(const char* file, const char* folder)
    {
      if(!GenSymlink(file, folder, INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR))
        return -19;

      if(!GenSymlink(file, folder, INNATIVE_VERSION_MAJOR, -1))
        return -20;

      return GenSymlink(file, folder, -1, -1) ? 0 : -21;
    }

    int InstallFile(const char* arg0, const char* file, const char* folder)
    {
      path src     = GetProgramPath(arg0).parent_path() / file;
      auto target  = (path(folder) / src) += POSIX_VERSION_STR;
      bool success = copy_file(src, target, copy_options::overwrite_existing);

      // Calculate new master symlinks
      return success ? UpdateSymlinks(file, folder) : -16;
    }
#endif

    uint64_t GetLatestVersion()
    {
      uint16_t major    = 0;
      uint16_t minor    = 0;
      uint16_t revision = 0;
      uint16_t build    = 0;

#ifdef IN_PLATFORM_WIN32
      if(!Win32EnumKey(HKEY_CURRENT_USER, IN_WIN32_REGPATH, [&major](const wchar_t* s) {
           wchar_t* end;
           major = std::max((uint16_t)wcstol(s, &end, 10), major);
         }))
        return 0;

      if(!Win32EnumKey(HKEY_CURRENT_USER, (IN_WIN32_REGPATH L"\\" + std::to_wstring(major)).c_str(),
                       [&minor](const wchar_t* s) {
                         wchar_t* end;
                         minor = std::max((uint16_t)wcstol(s, &end, 10), minor);
                       }))
        return 0;

      if(!Win32EnumKey(HKEY_CURRENT_USER,
                       (IN_WIN32_REGPATH L"\\" + std::to_wstring(major) + L"\\" + std::to_wstring(minor)).c_str(),
                       [&revision](const wchar_t* s) {
                         wchar_t* end;
                         revision = std::max((uint16_t)wcstol(s, &end, 10), revision);
                       }))
        return 0;

      HKEY hKey;
      if(RegOpenKeyExW(HKEY_CURRENT_USER,
                       (IN_WIN32_REGPATH L"\\" + std::to_wstring(major) + L"\\" + std::to_wstring(minor) + L"\\" +
                        std::to_wstring(revision))
                         .c_str(),
                       0, KEY_READ, &hKey) != ERROR_SUCCESS)
        return 0;
      RegCloseKey(hKey);
#elif defined(IN_PLATFORM_POSIX)

#endif
      return INNATIVE_VERSION(major, minor, revision, build);
    }

    int Install(const char* arg0, bool full)
    {
#ifdef IN_PLATFORM_WIN32
      // Open our base registry entry and install this specific version info (overwrite any existing info)
      std::wstring path;
      path.resize(MAX_PATH);
      path.resize(GetModuleFileNameW(NULL, const_cast<wchar_t*>(path.data()), (DWORD)path.capacity()));
      std::wstring buf;
      buf.resize(GetFullPathNameW(path.c_str(), 0, 0, 0));
      buf.resize(GetFullPathNameW(path.c_str(), (DWORD)buf.capacity(), const_cast<wchar_t*>(buf.data()), 0));
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_REGPATH L"\\" IN_VERSION_PATH, nullptr, buf.c_str()))
        return -1;
      std::wstring runtime = buf.substr(0, buf.find_last_of(L'\\')) + L"\\innative.dll";
      if(!Win32SetKey(HKEY_CURRENT_USER, IN_WIN32_REGPATH L"\\" IN_VERSION_PATH, L"runtime", runtime.c_str()))
        return -2;

      constexpr uint64_t cur =
        INNATIVE_VERSION(INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR, INNATIVE_VERSION_REVISION, 0);
      if(GetLatestVersion() <= cur) // Only if we are the latest version do we perform a full install
        if(!Win32Install(cur, full))
          return -3;

#elif defined(IN_PLATFORM_POSIX)
      int err = 0;
      if((err = InstallFile(arg0, "libinnative.so", "/usr/lib/")) < 0)
        return err - 256;
      if((err = InstallFile(arg0, "innative-env.a", "/usr/lib/")) < 0)
        return err - 512;
      if((err = InstallFile(arg0, "innative-env-d.a", "/usr/lib/")) < 0)
        return err - 768;
      if((err = InstallFile(arg0, "innative-cmd", "/usr/bin/")) < 0)
        return err - 1024;
      chmod("/usr/bin/innative-cmd" POSIX_VERSION_STR,
            0777); // Make the actual file executable (we don't need to fix the symlinks)
#endif
      return 0;
    }

    int Uninstall()
    {
#ifdef IN_PLATFORM_WIN32
      // Remove this version from the registry by deleting all version levels. Only the levels that have no more subkeys
      // will actually be deleted.
      uint64_t oldversion = GetLatestVersion();
      bool r              = DeleteRegKey(IN_WIN32_REGPATH L"\\" IN_VERSION_PATH);
      DeleteRegKey(IN_WIN32_REGPATH L"\\" MAKEWSTRING(INNATIVE_VERSION_MAJOR) L"\\" MAKEWSTRING(INNATIVE_VERSION_MINOR));
      DeleteRegKey(IN_WIN32_REGPATH L"\\" MAKEWSTRING(INNATIVE_VERSION_MAJOR));

      HKEY hKey = 0;
      bool full = RegOpenKeyExW(HKEY_CURRENT_USER, IN_WIN32_CLASSPATH, 0, KEY_READ, &hKey) == ERROR_SUCCESS;
      if(hKey)
        RegCloseKey(hKey);

      uint64_t newversion = GetLatestVersion();
      if(!newversion) // That was the last version, so wipe all registry keys
      {
        r &= Win32DeleteKeyValue(HKEY_CURRENT_USER, L"Environment", L"INNATIVE_PATH");
        r &= DeleteRegKey(IN_WIN32_REGPATH); // This should work because we should have no more version subkeys
        if(full)
        {
          r &= DeleteRegKey(IN_WIN32_CLASSPATH L"\\shell\\open\\command");
          r &= DeleteRegKey(IN_WIN32_CLASSPATH L"\\shell\\open");
          r &= DeleteRegKey(IN_WIN32_CLASSPATH L"\\shell");
          r &= DeleteRegKey(IN_WIN32_CLASSPATH L"\\SupportedTypes");
          r &= DeleteRegKey(IN_WIN32_CLASSPATH);

          auto deregtype = [](bool& r, const wchar_t* progid, const wchar_t* ext) {
            auto id = std::wstring(progid) + ext + L"." + MAKEWSTRING(INNATIVE_VERSION_MAJOR);

            r &= DeleteRegKey((IN_WIN32_APPS + id + L"\\shell\\open\\command").c_str());
            r &= DeleteRegKey((IN_WIN32_APPS + id + L"\\shell\\open").c_str());
            r &= DeleteRegKey((IN_WIN32_APPS + id + L"\\shell").c_str());
            r &= DeleteRegKey((IN_WIN32_APPS + id).c_str());
            r &= Win32DeleteKeyValue(HKEY_CURRENT_USER,
                                     (std::wstring(L"Software\\Classes\\") + ext + L"\\OpenWithProgids").c_str(),
                                     id.c_str());
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
#elif defined(IN_PLATFORM_POSIX)
      if(unlink("/usr/lib/libinnative.so" POSIX_VERSION_STR) != 0)
        return -1;
      if(unlink("/usr/lib/innative-env.a" POSIX_VERSION_STR) != 0)
        return -2;
      if(unlink("/usr/lib/innative-env-d.a" POSIX_VERSION_STR) != 0)
        return -3;
      if(unlink("/usr/bin/innative-cmd" POSIX_VERSION_STR) != 0)
        return -4;

      // Calculate new symlinks
      int err = 0;
      if((err = UpdateSymlinks("libinnative.so", "/usr/lib/")) < 0)
        return err - 256;
      if((err = UpdateSymlinks("innative-env.a", "/usr/lib/")) < 0)
        return err - 512;
      if((err = UpdateSymlinks("innative-env-d.a", "/usr/lib/")) < 0)
        return err - 768;
      if((err = UpdateSymlinks("innative-cmd", "/usr/bin/")) < 0)
        return err - 1024;
      return 0;
#endif
    }
  }
}