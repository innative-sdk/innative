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
      void(*AddModule)(struct __ENVIRONMENT* env, void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path
      void(*AddWhitelist)(struct __ENVIRONMENT* env, const char* module_name, const char* export_name, FunctionSig* sig);
      void(*WaitForLoad)(struct __ENVIRONMENT* env);
      enum ERROR_CODE(*AddEmbedding)(struct __ENVIRONMENT* env, int tag, void* data, uint64_t size); // If size is 0, data points to a null terminated UTF8 file path
      enum ERROR_CODE(*Compile)(struct __ENVIRONMENT* env, const char* file);
      enum ERROR_CODE(*Run)(void* cache);
      void*(*LoadCache)(int flags, const char* file);
      void(*DestroyEnvironment)(struct __ENVIRONMENT* env);
    } NWExports;

    // Statically linked function that loads the runtime stub, which then loads the actual runtime functions.
    NW_COMPILER_DLLEXPORT extern void native_wasm_runtime(NWExports* exports);

    // Command Line Tool exports
    struct _NW_CHUNK
    {
      void* data;
      uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
      const char* name; // If NULL, this is treated as an embedding
      int tag; // Only used for embedding types
    };

    NW_COMPILER_DLLEXPORT extern int native_wasm_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const char* const* whitelist, int n_whitelist);
    NW_COMPILER_DLLEXPORT extern int native_wasm_build_loader(struct _NW_CHUNK* chunks, const char* out, bool dynamic);

#ifdef  __cplusplus
}
#endif

#endif