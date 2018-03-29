// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include <stdio.h>

#ifdef NW_PLATFORM_WIN32
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

struct WinPass
{
  NWExports* exports;
  Environment* env;
  int* err;
};

BOOL CALLBACK CountResource(__in_opt HMODULE hModule, __in LPCWSTR lpType, __in LPWSTR lpName, __in LONG_PTR lParam)
{
  *((unsigned int*)lParam) += 1;
  return TRUE;
}

BOOL CALLBACK EnumModule(__in_opt HMODULE hModule, __in LPCWSTR lpType, __in LPWSTR lpName, __in LONG_PTR lParam)
{
  struct WinPass* pass = (struct WinPass*)lParam;
  HRSRC res = FindResourceW(hModule, lpName, lpType);
  if(res != NULL)
  {
    HGLOBAL buf = LoadResource(hModule, res);
    if(buf)
    {
      void* data = LockResource(buf);
      if(data)
      {
        (*pass->exports->AddModule)(pass->env, data, SizeofResource(hModule, res), lpName, pass->err);
        return TRUE;
      }
    }
  }
  *pass->err = ERR_FATAL_NULL_POINTER;
  return FALSE;
}

BOOL CALLBACK EnumEnvironment(__in_opt HMODULE hModule, __in LPCWSTR lpType, __in LPWSTR lpName, __in LONG_PTR lParam)
{
  struct WinPass* pass = (struct WinPass*)lParam;
  HRSRC res = FindResourceW(hModule, lpName, lpType);
  if(res != NULL)
  {
    HGLOBAL buf = LoadResource(hModule, res);
    if(buf)
    {
      uint8_t* data = LockResource(buf);
      if(data)
      {
        (*pass->exports->AddEmbedding)(pass->env, data[0], data + 1, SizeofResource(hModule, res) - 1);
        return TRUE;
      }
    }
  }
  *pass->err = ERR_FATAL_NULL_POINTER;
  return FALSE;
}

#elif defined(NW_PLATFORM_POSIX)
#error TODO
#else
#error unknown platform!
#endif

// This is a generic cross-platform WASM runtime loader. It uses the runtime stub
// that was compiled with it and loads whatever payloads it was configured with.
int main(int argc, char** argv)
{
  // First we load the runtime using whatever runtime function was linked.
  // This will either be an embedded runtime, or a stub function that dynamically loads a DLL.
  NWExports exports;
  native_wasm_runtime(&exports);
  unsigned int flags = 0; //ENV_MULTITHREADED;
  unsigned int maxthreads = 0;
  void* cache = (*exports.LoadCache)(flags);

  // Before doing anything, check if we have a cached version available
  if(!cache)
  {
    // Count WASM module payloads.
#ifdef NW_PLATFORM_WIN32
    unsigned int modules = 0;
    if(EnumResourceNamesW(NULL, L"WASM_MODULE", &CountResource, (LONG_PTR)&modules) == FALSE)
    {
      fprintf(stderr, "Error counting resources: %u\n", GetLastError());
      return GetLastError();
    }

    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    maxthreads = sysinfo.dwNumberOfProcessors;
#elif defined(NW_PLATFORM_POSIX)
    maxthreads = sysconf(_SC_NPROCESSORS_ONLN);
#endif

    // Then create the runtime environment with the module count.
    Environment* env = (*exports.CreateEnvironment)(flags, modules, maxthreads);
    if(!env)
    {
      fprintf(stderr, "Unknown error creating environment.\n");
      return -1;
    }

    // Then add each module payload to the environment, checking for any fatal errors.
#ifdef NW_PLATFORM_WIN32
    int err = 0;
    struct WinPass pass = { &exports, env, &err };

    if(EnumResourceNamesW(NULL, L"WASM_MODULE", &EnumModule, (LONG_PTR)&pass) == FALSE)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating modules: %u\n", GetLastError());
        return err;
      }
    }
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif

    if(err < 0)
    {
      fprintf(stderr, "Error loading modules: %i\n", err);
      return err;
    }

    // Ensure all modules are loaded, in case we have multithreading enabled
    (*exports.WaitForLoad)(env);

    // Then add each embedding environment payload to the environment.
    // These payloads have a tag, but have no set format. What the tag means depends on the runtime we've loaded.
#ifdef NW_PLATFORM_WIN32
    if(EnumResourceNamesW(NULL, L"WASM_ENVIRONMENT", &EnumEnvironment, (LONG_PTR)&pass) == FALSE || err < 0)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating embedding environments: %u - %i\n", GetLastError(), err);
        return err;
      }
    }

#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif

    // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env);
    if(err < 0)
    {
      fprintf(stderr, "Compile error: %i\n", err);

      for(ValidationError* err = env->errors; err != 0; err = err->next)
        fprintf(stderr, "Error %i: %s\n", err->code, err->error);

      int i = 0;
      scanf_s("%i", &i);
      return err;
    }

    // Destroy environment now that compilation is complete
    (*exports.DestroyEnvironment)(env);
    cache = (*exports.LoadCache)(flags);
  }

  if(!cache)
    return ERR_FATAL_NULL_POINTER;

  // Run the resulting code and return any errors
  return (*exports.Run)(cache);
}

#ifdef NW_PLATFORM_WIN32
struct HINSTANCE__;

// WinMain function, simply a catcher that calls the main function
int __stdcall WinMain(struct HINSTANCE__* hInstance, struct HINSTANCE__* hPrevInstance, char* lpCmdLine, int nShowCmd)
{
  main(0, (char**)hInstance);
}
#endif