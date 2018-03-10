// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include "parse.h"
#include <atomic>
#include <thread>
#include <stdio.h>

struct __ENVIRONMENT* CreateEnvironment(unsigned int flags, unsigned int modules, unsigned int maxthreads)
{
  Environment* env = (Environment*)calloc(1, sizeof(Environment));
  if(env)
  {
    env->modules = (Module*)realloc(0, modules * sizeof(Module));
    if(!env->modules)
    {
      free(env);
      return 0;
    }

    env->capacity = modules;
    env->flags = flags;
  }
  return env;
}

void DestroyEnvironment(struct __ENVIRONMENT* env)
{
  if(!env)
    return;

  while(env->errors)
  {
    ValidationError* e = env->errors;
    env->errors = env->errors->next;
    free(e);
  }

  for(varuint32 i = 0; i < env->n_modules; ++i)
    kh_destroy_exports(env->modules[i].exports);

  kh_destroy_modules(env->modulemap);
  free(env->modules);
  free(env);
}

void LoadModule(struct __ENVIRONMENT* env, size_t index, void* data, uint64_t size, int* err)
{
  Stream s = { (uint8_t*)data, size, 0 };
  *err = ParseModule(s, env->modules[index]);
  ((std::atomic<size_t>&)env->n_modules).fetch_add(1, std::memory_order_release);
}

void AddModule(struct __ENVIRONMENT* env, void* data, uint64_t size, int* err)
{
  if(!env || !err)
  {
    *err = ERR_FATAL_NULL_POINTER;
    return;
  }

  if((env->flags & ENV_MULTITHREADED) != 0 && env->maxthreads > 0)
  {
    while(((std::atomic<size_t>&)env->size).load(std::memory_order_relaxed) - ((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed) >= env->maxthreads)
      std::this_thread::sleep_for(std::chrono::milliseconds(2)); // If we're using maxthreads, block until one finishes (we must block up here or we risk a deadlock)
  }

  size_t index = ((std::atomic<size_t>&)env->size).fetch_add(1, std::memory_order_acq_rel);
  if(index >= env->capacity)
  {
    while(((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed) != index)
      std::this_thread::sleep_for(std::chrono::milliseconds(5)); // If we've exceeded our capacity, block until all other threads have finished

    env->modules = (Module*)realloc(env->modules, index * 2 * sizeof(Module));
    if(!env->modules)
    {
      *err = ERR_FATAL_OUT_OF_MEMORY;
      return;
    }
    ((std::atomic<size_t>&)env->capacity).store(index * 2, std::memory_order_release);
  }

  if(env->flags & ENV_MULTITHREADED)
    std::thread(LoadModule, env, index, data, size, err).detach();
  else
    LoadModule(env, index, data, size, err);
}

void WaitForLoad(struct __ENVIRONMENT* env)
{
  while(((std::atomic<size_t>&)env->size).load(std::memory_order_relaxed) > ((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed))
    std::this_thread::sleep_for(std::chrono::milliseconds(5)); // Spin until all modules loaded
}

enum ERROR_CODE AddEmbedding(struct __ENVIRONMENT* env, int tag, void* data, uint64_t size)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;
  return ERR_SUCCESS;
}

enum ERROR_CODE Compile(struct __ENVIRONMENT* env)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;
  return ERR_SUCCESS;
}

enum ERROR_CODE Run(void* cache)
{
  if(!cache)
    return ERR_FATAL_NULL_POINTER;
  NW_Entrypoint func = (NW_Entrypoint)LoadDLLFunction(cache, NW_ENTRYPOINT);
  if(!func)
    return ERR_FATAL_NULL_POINTER;
  (*func)();
  return ERR_SUCCESS;
}

void* LoadCache(int flags)
{
  std::string path = GetProgramPath() + NW_EXTENSION;
  void* handle = LoadDLL(path.c_str());
  if(!handle)
    return 0;
  NW_GetCPUInfo func = (NW_GetCPUInfo)LoadDLLFunction(handle, NW_GETCPUINFO);
  if(!func)
    return 0;
  uintcpuinfo target;
  (*func)(target);
  uintcpuinfo source;
  GetCPUInfo(source, flags);
  return memcmp(target, source, sizeof(uintcpuinfo)) ? 0 : handle;
}

// Return pointers to all our internal functions
void native_wasm_runtime(NWExports* exports)
{
  exports->CreateEnvironment = &CreateEnvironment;
  exports->AddModule = &AddModule;
  exports->WaitForLoad = &WaitForLoad;
  exports->AddEmbedding = &AddEmbedding;
  exports->Compile = &Compile;
  exports->Run = &Run;
  exports->LoadCache = &LoadCache;
}
