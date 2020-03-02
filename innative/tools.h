// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__TOOLS_H
#define IN__TOOLS_H

#include "innative/export.h"
#include <stdint.h>
#include <ostream>

namespace innative {
  Environment* CreateEnvironment(unsigned int modules, unsigned int maxthreads, const char* arg0);
  void ClearEnvironmentCache(Environment* env, Module* m);
  void DestroyEnvironment(Environment* env);
  void LoadModule(Environment* env, size_t index, const void* data, size_t size, const char* name, const char* file,
                  int* err);
  void AddModule(Environment* env, const void* data, size_t size, const char* name, int* err);
  int AddModuleObject(Environment* env, const Module* m);
  enum IN_ERROR AddWhitelist(Environment* env, const char* module_name, const char* export_name);
  enum IN_ERROR AddEmbedding(Environment* env, int tag, const void* data, size_t size, const char* name_override);
  enum IN_ERROR AddCustomExport(Environment* env, const char* symbol);
  enum IN_ERROR FinalizeEnvironment(Environment* env);
  enum IN_ERROR Validate(Environment* env);
  enum IN_ERROR Compile(Environment* env, const char* file);
  IN_Entrypoint LoadFunction(void* assembly, const char* module_name, const char* function);
  IN_Entrypoint LoadTable(void* assembly, const char* module_name, const char* table, varuint32 index);
  INGlobal* LoadGlobal(void* assembly, const char* module_name, const char* export_name);
  INModuleMetadata* GetModuleMetadata(void* assembly, uint32_t module_index);
  IN_Entrypoint LoadTableIndex(void* assembly, uint32_t module_index, uint32_t table_index, varuint32 function_index);
  INGlobal* LoadGlobalIndex(void* assembly, uint32_t module_index, uint32_t global_index);
  INGlobal* LoadMemoryIndex(void* assembly, uint32_t module_index, uint32_t memory_index);
  int ReplaceTableFuncPtr(void* assembly, uint32_t module_index, uint32_t table_index, const char* function,
                          IN_Entrypoint replace);
  void* LoadAssembly(const char* file);
  void FreeAssembly(void* assembly);
  const char* GetTypeEncodingString(int type_encoding);
  const char* GetErrorString(int error_code);
  int CompileScript(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output);
  int SerializeModule(Environment* env, size_t m, const char* out, size_t* len, bool emitdebug);
  int LoadSourceMap(Environment* env, unsigned int m, const char* path, size_t len);
  int InsertModuleSection(Environment* env, Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index);
  int DeleteModuleSection(Environment* env, Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index);
  int SetByteArray(Environment* env, ByteArray* bytearray, const void* data, varuint32 size);
  int SetIdentifier(Environment* env, Identifier* identifier, const char* str);
  int InsertModuleLocal(Environment* env, FunctionBody* body, varuint32 index, varsint7 local, varuint32 count,
                        DebugInfo* info);
  int RemoveModuleLocal(Environment* env, FunctionBody* body, varuint32 index);
  int InsertModuleInstruction(Environment* env, FunctionBody* body, varuint32 index, Instruction* ins);
  int RemoveModuleInstruction(Environment* env, FunctionBody* body, varuint32 index);
  int InsertModuleParam(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index, varsint7 param,
                        DebugInfo* name);
  int RemoveModuleParam(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index);
  int InsertModuleReturn(Environment* env, FunctionType* func, varuint32 index, varsint7 result);
  int RemoveModuleReturn(Environment* env, FunctionType* func, varuint32 index);
  size_t ReserveModule(Environment* env, int* err);
}

#endif