// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __EXPORT_H__NW__
#define __EXPORT_H__NW__

#include "native-wasm/schema.h"

#ifdef  __cplusplus
extern "C" {
#endif

    typedef void(*NW_GetCPUInfo)(int* out);
    typedef void(*NW_Entrypoint)();

    // Contains the actual runtime functions
    typedef struct __NW_EXPORTS
    {
      struct __ENVIRONMENT* (*CreateEnvironment)(unsigned int flags, unsigned int modules, unsigned int maxthreads);
      void(*AddModule)(struct __ENVIRONMENT* env, void* data, uint64_t size, const char* name, int* err);
      void(*WaitForLoad)(struct __ENVIRONMENT* env);
      enum ERROR_CODE(*AddEmbedding)(struct __ENVIRONMENT* env, int tag, void* data, uint64_t size);
      enum ERROR_CODE(*Compile)(struct __ENVIRONMENT* env);
      enum ERROR_CODE(*Run)(void* cache);
      void*(*LoadCache)(int flags);
      void(*DestroyEnvironment)(struct __ENVIRONMENT* env);
    } NWExports;

    // Statically linked function that loads the runtime stub, which then loads the actual runtime functions.
    NW_COMPILER_DLLEXPORT extern void native_wasm_runtime(NWExports* exports);

#ifdef  __cplusplus
}
#endif

#endif