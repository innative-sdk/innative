// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"

const wchar_t* GetRegString(wchar_t* buf, size_t sz, int major, int minor, int revision)
{
  buf[0] = 0;
  wcscat_s(buf, sz, IN_WIN32_REGPATH L"\\");
  _itow_s(major, buf + wcslen(buf), sz - wcslen(buf), 10);

  if(minor >= 0)
  {
    wcscat_s(buf, sz, L"\\");
    _itow_s(minor, buf + wcslen(buf), sz - wcslen(buf), 10);
  }

  if(revision >= 0)
  {
    wcscat_s(buf, sz, L"\\");
    _itow_s(revision, buf + wcslen(buf), sz - wcslen(buf), 10);
  }

  return buf;
}

wchar_t* GetRuntimeVersion(wchar_t* buf, size_t sz, uint16_t major, uint16_t minor, uint16_t revision)
{
  HKEY hKey;
  if(RegOpenKeyExW(HKEY_CURRENT_USER, GetRegString(buf, sz, major, minor, revision), 0, KEY_READ, &hKey) != ERROR_SUCCESS)
    return 0;

  DWORD len;
  RegQueryValueExW(hKey, L"runtime", 0, 0, 0, &len);
  LPBYTE runtime = malloc(sizeof(wchar_t) * ((len + 1) / 2));
  if(!runtime)
    return 0;

  if(RegQueryValueExW(hKey, L"runtime", 0, 0, runtime, &len) != ERROR_SUCCESS)
  {
    free(runtime);
    runtime = 0;
  }

  RegCloseKey(hKey);
  return (wchar_t*)runtime;
}

bool EnumKeyValue(HKEY hive, const wchar_t* key, uint16_t* version)
{
  HKEY hKey;
  if(RegOpenKeyExW(hive, key, 0, KEY_READ, &hKey) != ERROR_SUCCESS)
    return false;

  FILETIME ftWrite;
  DWORD dwSize = MAX_PATH;
  wchar_t szName[MAX_PATH];
  DWORD index     = 0;
  LSTATUS lResult = RegEnumKeyExW(hKey, index, szName, &dwSize, 0, 0, 0, &ftWrite);

  while(lResult == ERROR_SUCCESS)
  {
    wchar_t* end;
    uint16_t v = (uint16_t)wcstoul(szName, &end, 10);
    if(!*end &&
       v > *version) // This only counts if end points at the actual end of the string, otherwise it wasn't a pure number
      *version = v;

    dwSize  = MAX_PATH;
    lResult = RegEnumKeyExW(hKey, ++index, szName, &dwSize, 0, 0, 0, &ftWrite);
  }

  RegCloseKey(hKey);
  return true;
}
#elif defined(IN_PLATFORM_POSIX)
  #define MAKESTRING2(x) #x
  #define MAKESTRING(x)  MAKESTRING2(x)

  #include <dlfcn.h>
#endif

// This is a stub loader for the runtime. It looks for an existing installation
// of the runtime on the OS that is equal to or newer than the compiled version.
IN_COMPILER_DLLEXPORT extern void innative_runtime(INExports* exports)
{
#ifdef IN_PLATFORM_WIN32
  // On windows, we use the registry to store versions, with a key set to the DLL path of the runtime.
  // We prefer using an exact match to our compiled version if it is available. Otherwise, we get the next closest version.

  wchar_t buf[(sizeof(IN_WIN32_REGPATH) / 2) + 6 * 4]; // uint16_t is a maximum of 5 digits, plus the backspace character

  wchar_t* runtime =
    GetRuntimeVersion(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR, INNATIVE_VERSION_REVISION);
  if(!runtime)
  {
    uint16_t revision = 0;
    if(EnumKeyValue(HKEY_CURRENT_USER,
                    GetRegString(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR, -1), &revision))
      runtime = GetRuntimeVersion(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, INNATIVE_VERSION_MINOR, revision);
  }

  if(!runtime)
  {
    uint16_t revision = 0;
    uint16_t minor    = 0;
    if(EnumKeyValue(HKEY_CURRENT_USER, GetRegString(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, -1, -1), &minor))
    {
      if(EnumKeyValue(HKEY_CURRENT_USER, GetRegString(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, minor, -1), &revision))
        runtime = GetRuntimeVersion(buf, sizeof(buf) / 2, INNATIVE_VERSION_MAJOR, minor, revision);
    }
  }

  if(!runtime)
  {
    uint16_t revision = 0;
    uint16_t minor    = 0;
    uint16_t major    = 0;

    if(EnumKeyValue(HKEY_CURRENT_USER, IN_WIN32_REGPATH, &major))
    {
      if(EnumKeyValue(HKEY_CURRENT_USER, GetRegString(buf, sizeof(buf) / 2, major, -1, -1), &minor))
      {
        if(EnumKeyValue(HKEY_CURRENT_USER, GetRegString(buf, sizeof(buf) / 2, major, minor, -1), &revision))
          runtime = GetRuntimeVersion(buf, sizeof(buf) / 2, major, minor, revision);
      }
    }
  }

  if(runtime)
  {
    HMODULE dll = LoadLibraryW(runtime);
    if(dll != NULL)
    {
      void (*hook)(INExports*) = (void (*)(INExports*))GetProcAddress((HMODULE)dll, "innative_runtime");
      if(hook)
        (*hook)(exports);
      else
        FreeLibrary(
          dll); // Only free the library if the function FAILED, we'll need it later if the runtime was loaded correctly
    }

    free(runtime);
  }

#elif defined(IN_PLATFORM_POSIX)
  // Try each symlink level sequentially
  void* lib = dlopen("libinnative.so." MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(
                       INNATIVE_VERSION_MINOR) "." MAKESTRING(INNATIVE_VERSION_REVISION),
                     RTLD_NOW);
  if(!lib)
    lib = dlopen("libinnative.so." MAKESTRING(INNATIVE_VERSION_MAJOR) "." MAKESTRING(INNATIVE_VERSION_MINOR), RTLD_NOW);
  if(!lib)
    lib = dlopen("libinnative.so." MAKESTRING(INNATIVE_VERSION_MAJOR), RTLD_NOW);
  if(!lib)
    lib = dlopen("libinnative.so", RTLD_NOW);

  if(lib != NULL)
  {
    void (*hook)(INExports*) = (void (*)(INExports*))dlsym(lib, "innative_runtime");
    if(hook)
      (*hook)(exports);
    else
      dlclose(lib);
  }
#else
  #error Unknown platform!
#endif
}