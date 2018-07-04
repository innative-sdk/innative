// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __TOOLS_H__NW__
#define __TOOLS_H__NW__

#include <stdint.h>

struct __ENVIRONMENT* CreateEnvironment(unsigned int flags, unsigned int modules, unsigned int maxthreads);
void DestroyEnvironment(struct __ENVIRONMENT* env);
void LoadModule(struct __ENVIRONMENT* env, size_t index, void* data, uint64_t size, const char* name, int* err);
void AddModule(struct __ENVIRONMENT* env, void* data, uint64_t size, const char* name, int* err);
void AddWhitelist(struct __ENVIRONMENT* env, const char* module_name, const char* export_name, const FunctionSig* sig);
void WaitForLoad(struct __ENVIRONMENT* env);
enum ERROR_CODE AddEmbedding(struct __ENVIRONMENT* env, int tag, void* data, uint64_t size);
enum ERROR_CODE Compile(struct __ENVIRONMENT* env, const char* file);
enum ERROR_CODE Run(void* cache);
void* LoadCache(int flags, const char* file);

#endif