// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __EXPORT_H__IR__
#define __EXPORT_H__IR__

#include "innative/schema.h"

#ifdef IR_PLATFORM_WIN32
#define IR_LIB_EXTENSION ".lib"
#define IR_LIB_FLAG ""
#else
#define IR_LIB_EXTENSION ".a"
#define IR_LIB_FLAG "-l"
#endif

#ifdef IR_DEBUG
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env_d" IR_LIB_EXTENSION
#else
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env" IR_LIB_EXTENSION
#endif

#ifdef  __cplusplus
extern "C" {
#endif
  typedef void(*IR_Entrypoint)();

  // Contains the actual runtime functions
  typedef struct __IR_EXPORTS
  {
    Environment* (*CreateEnvironment)(unsigned int flags, unsigned int modules, unsigned int maxthreads, const char* arg0);
    void(*AddModule)(Environment* env, const void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path
    void(*AddWhitelist)(Environment* env, const char* module_name, const char* export_name, const FunctionType* sig);
    void(*WaitForLoad)(Environment* env);
    enum IR_ERROR(*AddEmbedding)(Environment* env, int tag, const void* data, uint64_t size); // If size is 0, data points to a null terminated UTF8 file path
    enum IR_ERROR(*Compile)(Environment* env, const char* file);
    IR_Entrypoint(*LoadFunction)(void* cache, const char* module_name, const char* function, const FunctionType* sig); // if function is null, loads the entrypoint function
    void*(*LoadGlobal)(void* cache, const char* module_name, const char* export_name);
    void*(*LoadAssembly)(int flags, const char* file);
    void(*DestroyEnvironment)(Environment* env);
  } IRExports;

  // Statically linked function that loads the runtime stub, which then loads the actual runtime functions.
  IR_COMPILER_DLLEXPORT extern void innative_runtime(IRExports* exports);

  // Command Line Tool exports
  struct _IR_CHUNK
  {
    void* data;
    uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
    const char* name; // If NULL, this is treated as an embedding
    int tag; // Only used for embedding types
  };

  struct _IR_WHITELIST
  {
    const char* module_name;
    const char* export_name;
    FunctionType sig;
  };

  // Tooling functions that exist for command line utilities that always statically link to the runtime
  IR_COMPILER_DLLEXPORT extern int innative_compile_script(const uint8_t* data, size_t sz, Environment* env);
  IR_COMPILER_DLLEXPORT extern int innative_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const struct _IR_WHITELIST* whitelist, unsigned int n_whitelist, const char* arg0);
  IR_COMPILER_DLLEXPORT extern int innative_build_loader(struct _IR_CHUNK* chunks, const char* out, bool dynamic);
  IR_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin(const char* arg0);

#ifdef  __cplusplus
}
#endif

#endif