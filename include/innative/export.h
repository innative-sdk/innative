// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __EXPORT_H__IN__
#define __EXPORT_H__IN__

#include "innative/schema.h"

#ifdef IN_PLATFORM_WIN32
#define IN_STATIC_EXTENSION ".lib"
#define IN_STATIC_FLAG ""
#define IN_LIBRARY_EXTENSION ".dll"
#define IN_EXE_EXTENSION ".exe"
#define IN_WIN32_REGPATH L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\innative-cmd.exe"
#else
#define IN_STATIC_EXTENSION ".a"
#define IN_STATIC_FLAG "-l"
#define IN_LIBRARY_EXTENSION ".so"
#define IN_EXE_EXTENSION ""
#endif

#ifdef IN_DEBUG
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env_d" IN_STATIC_EXTENSION
#else
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env" IN_STATIC_EXTENSION
#endif

#define IN_INIT_FUNCTION "_innative_internal_start"
#define IN_EXIT_FUNCTION "_innative_internal_exit"

#ifdef  __cplusplus
extern "C" {
#endif
  typedef void(*IN_Entrypoint)();

  enum IN_EMBEDDING_TAGS
  {
    IN_TAG_ANY = 0,
    IN_TAG_STATIC,
    IN_TAG_DYNAMIC
  };

  typedef union __IN_GLOBAL_TYPE
  {
    uint32_t i32;
    uint64_t i64;
    float f32;
    double f64;
    void* memory; // The additional indirection for memory is important here, becuase the global is a pointer to a pointer
  } IRGlobal;

  // Contains the actual runtime functions
  typedef struct __IN_EXPORTS
  {
    Environment* (*CreateEnvironment)(unsigned int modules, unsigned int maxthreads, const char* arg0);
    void(*AddModule)(Environment* env, const void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path
    void(*AddWhitelist)(Environment* env, const char* module_name, const char* export_name);
    enum IN_ERROR(*AddEmbedding)(Environment* env, int tag, const void* data, uint64_t size); // If size is 0, data points to a null terminated UTF8 file path
    enum IN_ERROR(*FinalizeEnvironment)(Environment* env); // Waits for all modules to finish loading and performs any cleanup necessary
    enum IN_ERROR(*Compile)(Environment* env, const char* file);
    IN_Entrypoint(*LoadFunction)(void* assembly, const char* module_name, const char* function); // if function is null, loads the entrypoint function
    IN_Entrypoint(*LoadTable)(void* assembly, const char* module_name, const char* table);
    IRGlobal*(*LoadGlobal)(void* assembly, const char* module_name, const char* export_name);
    void*(*LoadAssembly)(const char* file);
    void(*FreeAssembly)(void* assembly);
    void(*DestroyEnvironment)(Environment* env);
  } IRExports;

  // Statically linked function that loads the runtime stub, which then loads the actual runtime functions.
  IN_COMPILER_DLLEXPORT extern void innative_runtime(IRExports* exports);

  // Command Line Tool exports
  struct _IN_CHUNK
  {
    void* data;
    uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
    const char* name; // If NULL, this is treated as an embedding
    int tag; // Only used for embedding types
  };

  struct _IN_WHITELIST
  {
    const char* module_name;
    const char* export_name;
  };

  // Tooling functions that exist for command line utilities that always statically link to the runtime
  IN_COMPILER_DLLEXPORT extern int innative_compile_script(const uint8_t* data, size_t sz, Environment* env, bool always_compile);
  IN_COMPILER_DLLEXPORT extern int innative_compile_file(const char* file, const char* out, uint64_t flags, uint64_t optimize, uint64_t features, bool dynamic, const struct _IN_WHITELIST* whitelist, unsigned int n_whitelist, const char* arg0);
  IN_COMPILER_DLLEXPORT extern int innative_serialize_module(Environment* env, size_t m, const char* out);
  IN_COMPILER_DLLEXPORT extern int innative_build_loader(struct _IN_CHUNK* chunks, const char* out, bool dynamic);
  IN_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin(const char* arg0);
  IN_COMPILER_DLLEXPORT extern int innative_install(const char* arg0, bool full); // full install requires elevation on windows
  IN_COMPILER_DLLEXPORT extern int innative_uninstall();
  IN_COMPILER_DLLEXPORT extern const char* innative_type_encoding_string(int type_encoding);
  IN_COMPILER_DLLEXPORT extern const char* innative_error_string(int error_code);
  IN_COMPILER_DLLEXPORT extern int innative_compile_llvm(const char** files, size_t n, int flags, const char* out, FILE* log, const char* sdkpath, const char* arg0);

#ifdef  __cplusplus
}
#endif

#endif