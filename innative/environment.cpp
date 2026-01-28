// Copyright (c)2023 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "environment.h"


    IN_WASM_ENVIRONMENT::IN_WASM_ENVIRONMENT(const IN_WASM_ENVIRONMENT& env) {}
IN_WASM_ENVIRONMENT::IN_WASM_ENVIRONMENT(IN_WASM_ENVIRONMENT&& env) {}
    explicit IN_WASM_ENVIRONMENT::IN_WASM_ENVIRONMENT(const EnvironmentConfig& config) {}
IN_WASM_ENVIRONMENT::~IN_WASM_ENVIRONMENT() {}

void IN_WASM_ENVIRONMENT::ClearCache(Module* m) {}
    void IN_WASM_ENVIRONMENT::LoadModule(size_t index, const void* data, size_t size, const char* name, const char* file,
    void IN_WASM_ENVIRONMENT::AddModule(const void* data, size_t size, const char* name, int* err) {}
    int IN_WASM_ENVIRONMENT::AddModuleObject(const Module* m) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddWhitelist(const char* module_name, const char* export_name) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddFuncMapping(const char* source_module, const char* source_name, const char* target) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddEmbedding(int tag, const void* data, size_t size, const char* name_override) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddCustomExport(const char* symbol) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddCPUFeature(const char* feature) {}
    size_t IN_WASM_ENVIRONMENT::RegisterPrepend(const char* initfunc, const char* destroyfunc) {}
    IN_ERROR IN_WASM_ENVIRONMENT::AddPrepend(const char* c_funcname, int id) {}
    IN_ERROR IN_WASM_ENVIRONMENT::Finalize() {}
    IN_ERROR IN_WASM_ENVIRONMENT::Validate() {}
    IN_ERROR IN_WASM_ENVIRONMENT::Compile(const char* file) {}
    IN_ERROR IN_WASM_ENVIRONMENT::CompileJIT(bool expose_process) {}
    IN_Entrypoint IN_WASM_ENVIRONMENT::LoadFunctionJIT(const char* module_name, const char* function) {}
    IN_Entrypoint IN_WASM_ENVIRONMENT::LoadTableJIT(const char* module_name, const char* table, varuint32 index) {}
    INGlobal* IN_WASM_ENVIRONMENT::LoadGlobalJIT(const char* module_name, const char* export_name) {}
    INModuleMetadata* IN_WASM_ENVIRONMENT::GetModuleMetadataJIT(uint32_t module_index) {}
    IN_Entrypoint IN_WASM_ENVIRONMENT::LoadTableIndexJIT(uint32_t module_index, uint32_t table_index,
                                    varuint32 function_index) {}
    INGlobal* IN_WASM_ENVIRONMENT::LoadGlobalIndexJIT(uint32_t module_index, uint32_t global_index) {}
    INGlobal* IN_WASM_ENVIRONMENT::LoadMemoryIndexJIT(uint32_t module_index, uint32_t memory_index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::CompileScript(const uint8_t* data, size_t sz, bool always_compile, const char* output) {}
    IN_ERROR IN_WASM_ENVIRONMENT::SerializeModule(size_t m, const char* out, size_t* len, bool emitdebug) {}
    IN_ERROR IN_WASM_ENVIRONMENT::LoadSourceMap(unsigned int m, const char* path, size_t len) {}
    IN_ERROR IN_WASM_ENVIRONMENT::InsertModuleSection(Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::DeleteModuleSection(Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::SetByteArray(ByteArray* bytearray, const void* data, varuint32 size) {}
    IN_ERROR IN_WASM_ENVIRONMENT::SetIdentifier(Identifier* identifier, const char* str) {}
    IN_ERROR IN_WASM_ENVIRONMENT::InsertModuleLocal(FunctionBody* body, varuint32 index, varsint7 local, varuint32 count,
                          DebugInfo* info) {}
    IN_ERROR IN_WASM_ENVIRONMENT::RemoveModuleLocal(FunctionBody* body, varuint32 index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::InsertModuleInstruction(FunctionBody* body, varuint32 index, Instruction* ins) {}
    IN_ERROR IN_WASM_ENVIRONMENT::RemoveModuleInstruction(FunctionBody* body, varuint32 index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::InsertModuleParam(FunctionType* func, FunctionDesc* desc, varuint32 index, varsint7 param,
                          DebugInfo* name) {}
    IN_ERROR IN_WASM_ENVIRONMENT::RemoveModuleParam(FunctionType* func, FunctionDesc* desc, varuint32 index) {}
    IN_ERROR IN_WASM_ENVIRONMENT::InsertModuleReturn(FunctionType* func, varuint32 index, varsint7 result) {}
    IN_ERROR IN_WASM_ENVIRONMENT::RemoveModuleReturn(FunctionType* func, varuint32 index) {}
    size_t IN_WASM_ENVIRONMENT::ReserveModule(IN_ERROR* err) {}
    IN_ERROR IN_WASM_ENVIRONMENT::SetupWASI(enum IN_WASI_VERSION version, bool debug) {}