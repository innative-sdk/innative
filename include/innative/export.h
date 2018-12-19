// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __EXPORT_H__IR__
#define __EXPORT_H__IR__

#include "innative/schema.h"

#ifdef IR_PLATFORM_WIN32
#define IR_STATIC_EXTENSION ".lib"
#define IR_STATIC_FLAG ""
#define IR_LIBRARY_EXTENSION ".dll"
#define IR_EXE_EXTENSION ".exe"
#define IR_WIN32_REGPATH L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\innative-cmd.exe"
#else
#define IR_STATIC_EXTENSION ".a"
#define IR_STATIC_FLAG "-l"
#define IR_LIBRARY_EXTENSION ".so"
#define IR_EXE_EXTENSION ""
#endif

#ifdef IR_DEBUG
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env_d" IR_STATIC_EXTENSION
#else
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env" IR_STATIC_EXTENSION
#endif

#define IR_INIT_FUNCTION "_innative_internal_start"
#define IR_EXIT_FUNCTION "_innative_internal_exit"

#ifdef  __cplusplus
extern "C" {
#endif
  typedef void(*IR_Entrypoint)();

  enum IR_EMBEDDING_TAGS
  {
    IR_TAG_ANY = 0,
    IR_TAG_STATIC,
    IR_TAG_DYNAMIC
  };

  typedef union __IR_GLOBAL_TYPE
  {
    uint32_t i32;
    uint64_t i64;
    float f32;
    double f64;
    void* memory; // The additional indirection for memory is important here, becuase the global is a pointer to a pointer
  } IRGlobal;

  // Contains the actual runtime functions
  typedef struct __IR_EXPORTS
  {
    Environment* (*CreateEnvironment)(unsigned int modules, unsigned int maxthreads, const char* arg0);
    void(*AddModule)(Environment* env, const void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path
    void(*AddWhitelist)(Environment* env, const char* module_name, const char* export_name);
    void(*WaitForLoad)(Environment* env);
    enum IR_ERROR(*AddEmbedding)(Environment* env, int tag, const void* data, uint64_t size); // If size is 0, data points to a null terminated UTF8 file path
    enum IR_ERROR(*Compile)(Environment* env, const char* file);
    IR_Entrypoint(*LoadFunction)(void* cache, const char* module_name, const char* function); // if function is null, loads the entrypoint function
    IR_Entrypoint(*LoadTable)(void* cache, const char* module_name, const char* table);
    IRGlobal*(*LoadGlobal)(void* cache, const char* module_name, const char* export_name);
    void*(*LoadAssembly)(const char* file);
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
  };

  // Tooling functions that exist for command line utilities that always statically link to the runtime
  IR_COMPILER_DLLEXPORT extern int innative_compile_script(const uint8_t* data, size_t sz, Environment* env, bool always_compile);
  IR_COMPILER_DLLEXPORT extern int innative_compile_file(const char* file, const char* out, uint64_t flags, uint64_t optimize, uint64_t features, bool dynamic, const struct _IR_WHITELIST* whitelist, unsigned int n_whitelist, const char* arg0);
  IR_COMPILER_DLLEXPORT extern int innative_build_loader(struct _IR_CHUNK* chunks, const char* out, bool dynamic);
  IR_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin(const char* arg0);
  IR_COMPILER_DLLEXPORT extern int innative_install(const char* arg0, bool full); // full install requires elevation on windows
  IR_COMPILER_DLLEXPORT extern int innative_uninstall();

#ifdef  __cplusplus
}
#endif

#endif