// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include <stdio.h>

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"

struct WinPass
{
  INExports* exports;
  Environment* env;
  int* err;
};

BOOL CALLBACK CountResource(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam)
{
  *((unsigned int*)lParam) += 1;
  return TRUE;
}

BOOL CALLBACK EnumHandler(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam,
                          void (*handler)(struct WinPass*, uint8_t*, DWORD, const char*))
{
  struct WinPass* pass = (struct WinPass*)lParam;
  HRSRC res            = FindResourceA(hModule, lpName, lpType);
  if(res != NULL)
  {
    HGLOBAL buf = LoadResource(hModule, res);
    if(buf)
    {
      uint8_t* data = LockResource(buf);
      if(data)
        (*handler)(pass, data, SizeofResource(hModule, res), lpName);
      return TRUE;
    }
  }
  *pass->err = ERR_FATAL_NULL_POINTER;
  return FALSE;
}

void EnumEnvironmentHandler(struct WinPass* pass, uint8_t* data, DWORD sz, const char* name)
{
  *pass->err = (*pass->exports->AddEmbedding)(pass->env, atoi(name), data, sz, 0);
}

void EnumModuleHandler(struct WinPass* pass, uint8_t* data, DWORD sz, const char* name)
{
  (*pass->exports->AddModule)(pass->env, data, sz, name, pass->err);
}
void EnumWhitelistHandler(struct WinPass* pass, uint8_t* data, DWORD sz, const char* name)
{
  *pass->err = 0;
  (*pass->exports->AddWhitelist)(pass->env, name, data);
}
void EnumFlagsHandler(struct WinPass* pass, uint8_t* data, DWORD sz, const char* name)
{
  if(sz != sizeof(uint64_t))
    *pass->err = ERR_FATAL_RESOURCE_ERROR;
  else
  {
    uint64_t flags = *(uint64_t*)data;

    if(!STRICMP(name, WIN32_RESOURCE_FLAGS_FLAGS))
      pass->env->flags = flags;
    else if(!STRICMP(name, WIN32_RESOURCE_FLAGS_OPTIMIZE))
      pass->env->optimize = flags;
    else if(!STRICMP(name, WIN32_RESOURCE_FLAGS_FEATURES))
      pass->env->features = flags;
    else
      *pass->err = ERR_FATAL_RESOURCE_ERROR;
  }
}

BOOL CALLBACK EnumEnvironment(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam)
{
  return EnumHandler(hModule, lpType, lpName, lParam, &EnumEnvironmentHandler);
}
BOOL CALLBACK EnumModule(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam)
{
  return EnumHandler(hModule, lpType, lpName, lParam, &EnumModuleHandler);
}
BOOL CALLBACK EnumWhitelist(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam)
{
  return EnumHandler(hModule, lpType, lpName, lParam, &EnumWhitelistHandler);
}
BOOL CALLBACK EnumFlags(__in_opt HMODULE hModule, __in LPCSTR lpType, __in LPSTR lpName, __in LONG_PTR lParam)
{
  return EnumHandler(hModule, lpType, lpName, lParam, &EnumFlagsHandler);
}

#elif defined(IN_PLATFORM_POSIX)
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
  INExports exports;
  innative_runtime(&exports);
  unsigned int maxthreads = 0;
  void* assembly          = (*exports.LoadAssembly)("out.cache");

  // Before doing anything, check if we have a cached version available
  if(!assembly)
  {
    // Count WASM module payloads.
#ifdef IN_PLATFORM_WIN32
    unsigned int modules = 0;
    if(EnumResourceNamesA(NULL, WIN32_RESOURCE_MODULE, &CountResource, (LONG_PTR)&modules) == FALSE)
    {
      fprintf(stderr, "Error counting resources: %u\n", GetLastError());
      return GetLastError();
    }

    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    maxthreads = sysinfo.dwNumberOfProcessors;
#elif defined(IN_PLATFORM_POSIX)
    maxthreads = sysconf(_SC_NPROCESSORS_ONLN);
#endif

    // Then create the runtime environment with the module count.
    Environment* env = (*exports.CreateEnvironment)(modules, maxthreads, (!argc ? 0 : argv[0]));
    if(!env)
    {
      fprintf(stderr, "Unknown error creating environment.\n");
      return -1;
    }

    // Set the flag values
#ifdef IN_PLATFORM_WIN32
    int err             = ERR_SUCCESS;
    struct WinPass pass = { &exports, env, &err };
    if(EnumResourceNamesA(NULL, WIN32_RESOURCE_FLAGS, &EnumFlags, (LONG_PTR)&pass) == FALSE || err < 0)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating flag values: %u - %i\n", GetLastError(), err);
        (*exports.DestroyEnvironment)(env);
        return err;
      }
    }
#elif defined(IN_PLATFORM_POSIX)
  #error TODO
#endif
    env->flags |= ENV_NO_INIT | ENV_LIBRARY;

    // Then add each module payload to the environment, checking for any fatal errors.
#ifdef IN_PLATFORM_WIN32
    if(EnumResourceNamesA(NULL, WIN32_RESOURCE_MODULE, &EnumModule, (LONG_PTR)&pass) == FALSE)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating modules: %u\n", GetLastError());
        (*exports.DestroyEnvironment)(env);
        return err;
      }
    }
#elif defined(IN_PLATFORM_POSIX)
  #error TODO
#endif

    if(err < 0)
    {
      fprintf(stderr, "Error loading modules: %i\n", err);
      (*exports.DestroyEnvironment)(env);
      return err;
    }

    // Then add each embedding environment payload to the environment.
    // These payloads have a tag, but have no set format. What the tag means depends on the runtime we've loaded.
#ifdef IN_PLATFORM_WIN32
    if(EnumResourceNamesA(NULL, WIN32_RESOURCE_EMBEDDING, &EnumEnvironment, (LONG_PTR)&pass) == FALSE || err < 0)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating embedding environments: %u - %i\n", GetLastError(), err);
        (*exports.DestroyEnvironment)(env);
        return err;
      }
    }
#elif defined(IN_PLATFORM_POSIX)
  #error TODO
#endif

    // Add the whitelist values, the resource name being the module and the data being the function
#ifdef IN_PLATFORM_WIN32
    if(EnumResourceNamesA(NULL, WIN32_RESOURCE_WHITELIST, &EnumWhitelist, (LONG_PTR)&pass) == FALSE || err < 0)
    {
      if(GetLastError() != ERROR_RESOURCE_TYPE_NOT_FOUND)
      {
        fprintf(stderr, "Error enumerating whitelist: %u - %i\n", GetLastError(), err);
        (*exports.DestroyEnvironment)(env);
        return err;
      }
    }
#elif defined(IN_PLATFORM_POSIX)
  #error TODO
#endif

    // Ensure all modules are loaded, in case we have multithreading enabled
    err = (*exports.FinalizeEnvironment)(env);

    // Attempt to compile. If an error happens, output it and any validation errors to stderr
    if(err >= 0)
      err = (*exports.Compile)(env, "out.cache");
    if(err < 0)
    {
      fprintf(stderr, "Compile error: %i\n", err);

      for(ValidationError* err = env->errors; err != 0; err = err->next)
        fprintf(stderr, "Error %i: %s\n", err->code, err->error);

      int i = 0;
      scanf_s("%i", &i);
      (*exports.DestroyEnvironment)(env);
      return err;
    }

    // Destroy environment now that compilation is complete
    (*exports.DestroyEnvironment)(env);
    assembly = (*exports.LoadAssembly)("out.cache");
  }

  if(!assembly)
    return ERR_FATAL_NULL_POINTER;

  // Load the entry point and execute it
  IN_Entrypoint start = (*exports.LoadFunction)(assembly, 0, IN_INIT_FUNCTION);
  IN_Entrypoint exit  = (*exports.LoadFunction)(assembly, 0, IN_EXIT_FUNCTION);
  if(!start)
  {
    (*exports.FreeAssembly)(assembly);
    return ERR_INVALID_START_FUNCTION;
  }

  (*start)();
  if(exit)
    (*exit)();

  (*exports.FreeAssembly)(assembly);
  return ERR_SUCCESS;
}

#ifdef IN_PLATFORM_WIN32
struct HINSTANCE__;

// WinMain function, simply a catcher that calls the main function
int __stdcall WinMain(struct HINSTANCE__* hInstance, struct HINSTANCE__* hPrevInstance, char* lpCmdLine, int nShowCmd)
{
  main(0, (char**)hInstance);
}
#endif