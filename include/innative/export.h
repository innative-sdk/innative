// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __EXPORT_H__IR__
#define __EXPORT_H__IR__

#include "innative/schema.h"

#ifdef  __cplusplus
extern "C" {
#endif

    typedef void(*IR_Entrypoint)();

    // Contains the actual runtime functions
    typedef struct __IR_EXPORTS
    {
      Environment* (*CreateEnvironment)(unsigned int flags, unsigned int modules, unsigned int maxthreads);
      void(*AddModule)(Environment* env, void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path
      void(*AddWhitelist)(Environment* env, const char* module_name, const char* export_name, const FunctionType* sig);
      void(*WaitForLoad)(Environment* env);
      enum IR_ERROR(*AddEmbedding)(Environment* env, int tag, void* data, uint64_t size); // If size is 0, data points to a null terminated UTF8 file path
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

    IR_COMPILER_DLLEXPORT extern int innative_compile_script(const char* file, unsigned int flags, ValidationError** errors);
    IR_COMPILER_DLLEXPORT extern int innative_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const struct _IR_WHITELIST* whitelist, int n_whitelist);
    IR_COMPILER_DLLEXPORT extern int innative_build_loader(struct _IR_CHUNK* chunks, const char* out, bool dynamic);
    IR_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin();

#ifdef  __cplusplus
}
#endif

#endif