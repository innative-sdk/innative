// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__TOOLS_H
#define IN__TOOLS_H

#include "innative/export.h"
#include <stdint.h>
#include <ostream>

struct IN_WASM_ENVIRONMENT;

namespace innative {
  struct IN_WASM_ENVIRONMENT* CreateEnvironment(unsigned int modules, unsigned int maxthreads, const char* arg0);
  void ClearEnvironmentCache(struct IN_WASM_ENVIRONMENT* env, Module* m);
  void DestroyEnvironment(struct IN_WASM_ENVIRONMENT* env);
  void LoadModule(struct IN_WASM_ENVIRONMENT* env, size_t index, const void* data, uint64_t size, const char* name,
                  const char* file, int* err);
  void AddModule(struct IN_WASM_ENVIRONMENT* env, const void* data, uint64_t size, const char* name, int* err);
  enum IN_ERROR AddWhitelist(struct IN_WASM_ENVIRONMENT* env, const char* module_name, const char* export_name);
  enum IN_ERROR AddEmbedding(struct IN_WASM_ENVIRONMENT* env, int tag, const void* data, uint64_t size);
  enum IN_ERROR FinalizeEnvironment(struct IN_WASM_ENVIRONMENT* env);
  enum IN_ERROR Validate(struct IN_WASM_ENVIRONMENT* env);
  enum IN_ERROR Compile(struct IN_WASM_ENVIRONMENT* env, const char* file);
  IN_Entrypoint LoadFunction(void* assembly, const char* module_name, const char* function);
  IN_Entrypoint LoadTable(void* assembly, const char* module_name, const char* table, varuint32 index);
  IRGlobal* LoadGlobal(void* assembly, const char* module_name, const char* export_name);
  void* LoadAssembly(const char* file);
  void FreeAssembly(void* assembly);
  void DumpModule(std::ostream& stream, Module& mod);
  const char* GetTypeEncodingString(int type_encoding);
  const char* GetErrorString(int error_code);
  int CompileScript(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output);
  int SerializeModule(Environment* env, size_t m, const char* out, size_t* len);
}

#endif