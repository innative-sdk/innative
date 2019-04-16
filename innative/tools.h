// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __TOOLS_H__IN__
#define __TOOLS_H__IN__

#include "innative/export.h"
#include <stdint.h>
#include <ostream>

struct __WASM_ENVIRONMENT;

namespace innative {
  struct __WASM_ENVIRONMENT* CreateEnvironment(unsigned int modules, unsigned int maxthreads, const char* arg0);
  void ClearEnvironmentCache(struct __WASM_ENVIRONMENT* env, Module* m);
  void DestroyEnvironment(struct __WASM_ENVIRONMENT* env);
  void LoadModule(struct __WASM_ENVIRONMENT* env, size_t index, const void* data, uint64_t size, const char* name, const char* path, int* err);
  void AddModule(struct __WASM_ENVIRONMENT* env, const void* data, uint64_t size, const char* name, int* err);
  void AddWhitelist(struct __WASM_ENVIRONMENT* env, const char* module_name, const char* export_name);
  enum IN_ERROR AddEmbedding(struct __WASM_ENVIRONMENT* env, int tag, const void* data, uint64_t size);
  enum IN_ERROR FinalizeEnvironment(struct __WASM_ENVIRONMENT* env);
  enum IN_ERROR Compile(struct __WASM_ENVIRONMENT* env, const char* file);
  IN_Entrypoint LoadFunction(void* assembly, const char* module_name, const char* function);
  IN_Entrypoint LoadTable(void* assembly, const char* module_name, const char* table, varuint32 index);
  IRGlobal* LoadGlobal(void* assembly, const char* module_name, const char* export_name);
  void* LoadAssembly(const char* file);
  void FreeAssembly(void* assembly);
  void DumpModule(std::ostream& stream, Module& mod);
}

#endif  