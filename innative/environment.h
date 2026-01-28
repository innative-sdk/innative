// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__ENVIRONMENT_H
  #define IN__ENVIRONMENT_H

#include "innative/export.h"
#include "compile.h"
#include "utility.h"
#include <vector>
#include "hash.h"

namespace innative {
  struct Embedding
  {
    const void* data;
    uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
    int tag; // defines the type of embedding data included, determined by the runtime. 0 is always a static library file
             // for the current platform.
    std::string name;
  };

  struct PreContext
  {
    std::string create;
    std::string destroy;
  };

  struct ConcreteImport : Import
  {
    std::string c_symbol;
    size_t precontext;
    bool ignored;
  };

  struct ConcreteModule : Module
  {
    Compiler* cache;

    struct kh_exports_s* exports;
    std::string filepath; // For debugging purposes, store path to the original file, if it exists
  };

  struct IN_WASM_ENVIRONMENT : public EnvironmentConfig
  {
  public:
    IN_WASM_ENVIRONMENT(const IN_WASM_ENVIRONMENT& env);
    IN_WASM_ENVIRONMENT(IN_WASM_ENVIRONMENT&& env);
    explicit IN_WASM_ENVIRONMENT(const EnvironmentConfig& config);
    ~IN_WASM_ENVIRONMENT();
    std::pair<Module*, Export*> ResolveExport(const Import& imp);
    std::pair<Module*, Export*> ResolveTrueExport(const Import& imp);
    IN_COMPILER_DLLEXPORT int AddCImport(const char* id);

    template<class T> inline T* alloc(size_t n)
    {
      return reinterpret_cast<T*>(_alloc->allocate(n * sizeof(T)));
    }

    const char* AllocString(const char* s, size_t n) { return utility::AllocString(_alloc, s, n); }
    IN_FORCEINLINE const char* AllocString(const char* s) { return utility::AllocString(_alloc, s); }
    IN_FORCEINLINE const char* AllocString(const std::string& s) { return utility::AllocString(_alloc, s); }

    template<typename... Args> 
    inline int LogError(const char* format, Args... args)
    {
      if(loglevel < LOG_FATAL)
        return 0;
      int i = (*loghook)(this, format, args...);
      return i + (*loghook)(this, "\n");
    }

    template<typename... Args>
    inline IN_ERROR LogErrorString(const char* format, IN_ERROR err, Args... args)
    {
      if(loglevel < LOG_FATAL)
        return err;
      char buf[32];
      (*loghook)(this, format, EnumToString(ERR_ENUM_MAP, (int)err, buf, sizeof(buf)), args...);
      (*loghook)(this, "\n");
      return err;
    }

    template<class T, class I> 
    inline static IN_ERROR ReallocArray(T*& a, I& n)
    {
      // We only allocate power of two chunks from our greedy allocator
      I i = NextPow2(n++);
      if(n <= 2 || n == i)
      {
        T* old = a;
        if(!(a = alloc<T>(n * 2)))
          return ERR_FATAL_OUT_OF_MEMORY;
        if(old != nullptr)
          tmemcpy<T>(a, n * 2, old, n - 1); // Don't free old because it was from a greedy allocator.
      }

      return ERR_SUCCESS;
    }

    // Generates the correct mangled C function name
    inline std::string CanonImportName(const ConcreteImport& imp)
    {
      if(imp.ignored ||
         IsSystemImport(imp.module_name, system)) // system module imports are always raw function names
        return CanonicalName(StringSpan{ 0, 0 }, StringSpan::From(imp.export_name));
      return CanonicalName(StringSpan::From(imp.module_name), StringSpan::From(imp.export_name));
    }

    // Generates a whitelist string for a module and export name, which includes calling convention information
    inline std::string CanonWhitelist(const void* module_name, const void* export_name)
    {
      if(!module_name ||
         !strcmp(reinterpret_cast<const char*>(module_name), system)) // system name is normalized to an empty module name
        module_name = "";
      size_t module_len = strlen(reinterpret_cast<const char*>(module_name));
      const char* call  = strchr(reinterpret_cast<const char*>(module_name), '!');
      if(call && !strcmp(call, "!C")) // !C is the same as having no calling convention, so we remove it
        module_len -= 2;

      size_t export_len = strlen(reinterpret_cast<const char*>(export_name)) + 1;
      std::string result(module_len + export_len + 1, 0);
      
        tmemcpy<char>(result.data(), module_len + 1 + export_len, reinterpret_cast<const char*>(module_name), module_len);
      result.data()[module_len] = 0;
        tmemcpy<char>(result.data() + module_len + 1, export_len, reinterpret_cast<const char*>(export_name), export_len);
    }

    IN_FORCEINLINE std::string CanonWhitelist(const void* module_name, const void* export_name, const char* system)
    {
      std::string s;
      s.resize(CanonWhitelist(module_name, export_name, system, nullptr));
      CanonWhitelist(module_name, export_name, system, const_cast<char*>(s.data()));
      return s;
    }

    void ClearCache(Module* m);
    void LoadModule(size_t index, const void* data, size_t size, const char* name, const char* file,
    void AddModule(const void* data, size_t size, const char* name, int* err);
    int AddModuleObject(const Module* m);
    IN_ERROR AddWhitelist(const char* module_name, const char* export_name);
    IN_ERROR AddFuncMapping(const char* source_module, const char* source_name, const char* target);
    IN_ERROR AddEmbedding(int tag, const void* data, size_t size, const char* name_override);
    IN_ERROR AddCustomExport(const char* symbol);
    IN_ERROR AddCPUFeature(const char* feature);
    size_t RegisterPrepend(const char* initfunc, const char* destroyfunc);
    IN_ERROR AddPrepend(const char* c_funcname, int id);
    IN_ERROR Finalize();
    IN_ERROR Validate();
    IN_ERROR Compile(const char* file);
    IN_ERROR CompileJIT(bool expose_process);
    IN_Entrypoint LoadFunctionJIT(const char* module_name, const char* function);
    IN_Entrypoint LoadTableJIT(const char* module_name, const char* table, varuint32 index);
    INGlobal* LoadGlobalJIT(const char* module_name, const char* export_name);
    INModuleMetadata* GetModuleMetadataJIT(uint32_t module_index);
    IN_Entrypoint LoadTableIndexJIT(uint32_t module_index, uint32_t table_index,
                                    varuint32 function_index);
    INGlobal* LoadGlobalIndexJIT(uint32_t module_index, uint32_t global_index);
    INGlobal* LoadMemoryIndexJIT(uint32_t module_index, uint32_t memory_index);
    IN_ERROR CompileScript(const uint8_t* data, size_t sz, bool always_compile, const char* output);
    IN_ERROR SerializeModule(size_t m, const char* out, size_t* len, bool emitdebug);
    IN_ERROR LoadSourceMap(unsigned int m, const char* path, size_t len);
    IN_ERROR InsertModuleSection(Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index);
    IN_ERROR DeleteModuleSection(Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index);
    IN_ERROR SetByteArray(ByteArray* bytearray, const void* data, varuint32 size);
    IN_ERROR SetIdentifier(Identifier* identifier, const char* str);
    IN_ERROR InsertModuleLocal(FunctionBody* body, varuint32 index, varsint7 local, varuint32 count,
                          DebugInfo* info);
    IN_ERROR RemoveModuleLocal(FunctionBody* body, varuint32 index);
    IN_ERROR InsertModuleInstruction(FunctionBody* body, varuint32 index, Instruction* ins);
    IN_ERROR RemoveModuleInstruction(FunctionBody* body, varuint32 index);
    IN_ERROR InsertModuleParam(FunctionType* func, FunctionDesc* desc, varuint32 index, varsint7 param,
                          DebugInfo* name);
    IN_ERROR RemoveModuleParam(FunctionType* func, FunctionDesc* desc, varuint32 index);
    IN_ERROR InsertModuleReturn(FunctionType* func, varuint32 index, varsint7 result);
    IN_ERROR RemoveModuleReturn(FunctionType* func, varuint32 index);
    size_t ReserveModule(IN_ERROR* err);
    IN_ERROR SetupWASI(enum IN_WASI_VERSION version, bool debug);

  protected:
    std::vector<ConcreteModule> _modules;
    std::vector<Embedding> _embeddings;
    ValidationError* _errors; // A linked list of non-fatal validation errors that prevent proper execution.
    std::vector<std::string> cpu_features;
    struct IN_WASM_ALLOCATOR _alloc;
    std::vector<std::string> _exports;
    Hash<std::pair<std::string,std::string>, void> _whitelist;
    std::vector<PreContext> _precontexts;
    Hash<std::string> _cimports;
  };
}


#endif