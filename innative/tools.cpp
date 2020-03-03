// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "parse.h"
#include "validate.h"
#include "link.h"
#include "tools.h"
#include "wast.h"
#include "serialize.h"
#include <atomic>
#include <thread>
#include <fstream>
#include <stdio.h>
#include <sstream>

using namespace innative;
using namespace utility;

Environment* innative::CreateEnvironment(unsigned int modules, unsigned int maxthreads, const char* arg0)
{
  Environment* env = reinterpret_cast<Environment*>(calloc(1, sizeof(Environment)));
  if(env)
  {
    env->modulemap = kh_init_modules();
    env->whitelist = kh_init_modulepair();
    env->cimports  = kh_init_cimport();
    env->modules   = trealloc<Module>(0, modules);
    env->alloc     = new IN_WASM_ALLOCATOR();

    if(!env->modules)
    {
      free(env);
      return nullptr;
    }

    env->capacity   = modules;
    env->flags      = ENV_SANDBOX;
    env->optimize   = ENV_OPTIMIZE_O3;
    env->features   = ENV_FEATURE_ALL;
    env->maxthreads = maxthreads;
    env->linker     = 0;
    env->log        = stdout;
    env->loglevel   = LOG_WARNING;
    env->rootpath   = AllocString(*env, GetProgramPath(arg0).parent_path().u8string());
    env->libpath    = env->rootpath;
    if(!env->libpath) // Out of memory
    {
      free(env);
      return nullptr;
    }

    env->objpath  = 0;
    env->system   = "";
    env->wasthook = 0;
  }
  return env;
}

void innative::ClearEnvironmentCache(Environment* env, Module* m)
{
  assert(env != nullptr);

  if(m)
    DeleteCache(*env, *m);
  else
    DeleteContext(*env, false); // We can't actually shutdown LLVM here because that permanently shuts it down and
                                // there is no way to restore it.
}

void innative::DestroyEnvironment(Environment* env)
{
  if(!env)
    return;

  ClearEnvironmentCache(env, 0);
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    kh_destroy_exports(env->modules[i].exports);
    assert(!env->modules[i].cache);
  }

  delete env->alloc;
  kh_destroy_modulepair(env->whitelist);
  kh_destroy_modules(env->modulemap);
  kh_destroy_cimport(env->cimports);
  free(env->modules);
  free(env);
}

void innative::LoadModule(Environment* env, size_t index, const void* data, size_t size, const char* name, const char* file,
                          int* err)
{
  Stream s = { reinterpret_cast<const uint8_t*>(data), size, 0 };
  std::string fallback;
  if(!name)
  {
    fallback = "m" + std::to_string(index);
    name     = fallback.data();
  }

  if((env->flags & ENV_ENABLE_WAT) && size > 0 && s.data[0] != 0)
  {
    env->modules[index] = { 0 };
    *err = innative::ParseWatModule(*env, file, env->modules[index], s.data, size, StringSpan{ name, strlen(name) });
  }
  else
    *err = ParseModule(s, file, *env, env->modules[index], ByteArray::Identifier(name, strlen(name)), env->errors);

  ((std::atomic<size_t>&)env->n_modules).fetch_add(1, std::memory_order_release);
}

size_t innative::ReserveModule(Environment* env, int* err)
{
  if((env->flags & ENV_MULTITHREADED) != 0 && env->maxthreads > 0)
  {
    while(((std::atomic<size_t>&)env->size).load(std::memory_order_relaxed) -
            ((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed) >=
          env->maxthreads)
      std::this_thread::sleep_for(std::chrono::milliseconds(
        2)); // If we're using maxthreads, block until one finishes (we must block up here or we risk a deadlock)
  }

  size_t index = ((std::atomic<size_t>&)env->size).fetch_add(1, std::memory_order_acq_rel);
  if(index >= ((std::atomic<size_t>&)env->capacity).load(std::memory_order_acquire))
  {
    while(((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed) != index)
      std::this_thread::sleep_for(
        std::chrono::milliseconds(5)); // If we've exceeded our capacity, block until all other threads have finished

    env->modules = trealloc<Module>(env->modules, index * 2);
    if(!env->modules)
    {
      *err = ERR_FATAL_OUT_OF_MEMORY;
      return (size_t)~0;
    }
    ((std::atomic<size_t>&)env->capacity).store(index * 2, std::memory_order_release);
  }
  return index;
}

void innative::AddModule(Environment* env, const void* data, size_t size, const char* name, int* err)
{
  *err = ERR_SUCCESS;
  if(!env || !err)
  {
    *err = ERR_FATAL_NULL_POINTER;
    return;
  }

  const char* file = nullptr;
  std::unique_ptr<uint8_t[]> data_module;
  if(!size)
  {
    file        = reinterpret_cast<const char*>(data);
    data_module = LoadFile(file, size);
    if(data_module.get() == nullptr)
    {
      *err = ERR_FATAL_FILE_ERROR;
      return;
    }
    data = data_module.get();
  }

  size_t index = ReserveModule(env, err);
  if(*err < 0)
    return;

  if(env->flags & ENV_MULTITHREADED)
    std::thread(LoadModule, env, index, data, size, name, file, err).detach();
  else
    LoadModule(env, index, data, size, name, file, err);
}

int innative::AddModuleObject(Environment* env, const Module* m)
{
  if(!env || !m)
    return ERR_FATAL_NULL_POINTER;

  int err      = 0;
  size_t index = ReserveModule(env, &err);
  if(err < 0)
    return err;

  env->modules[index] = *m;

  ((std::atomic<size_t>&)env->n_modules).fetch_add(1, std::memory_order_release);
  return ERR_SUCCESS;
}

IN_ERROR innative::AddWhitelist(Environment* env, const char* module_name, const char* export_name)
{
  if(!export_name)
    return ERR_PARSE_INVALID_NAME;

  char* whitelist = tmalloc<char>(*env, CanonWhitelist(module_name, export_name, env->system, nullptr));
  if(!whitelist)
    return ERR_FATAL_OUT_OF_MEMORY;

  CanonWhitelist(module_name, export_name, env->system, whitelist);

  int r;
  auto iter = kh_put_modulepair(env->whitelist, whitelist, &r);
  // kh_val(env->whitelist, iter) = !ftype ? FunctionType{ TE_NONE, 0, 0, 0, 0 } : *ftype;
  return ERR_SUCCESS;
}

IN_ERROR innative::AddEmbedding(Environment* env, int tag, const void* data, size_t size, const char* name_override)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  Embedding* embed = tmalloc<Embedding>(*env, 1);
  if(!embed)
    return ERR_FATAL_OUT_OF_MEMORY;

  embed->tag      = tag;
  embed->data     = data;
  embed->size     = size;
  embed->name     = name_override;
  embed->next     = env->embeddings;
  env->embeddings = embed;

  return ERR_SUCCESS;
}

IN_ERROR innative::AddCustomExport(Environment* env, const char* symbol)
{
  if(!env || !symbol)
    return ERR_FATAL_NULL_POINTER;

  IN_ERROR err = ReallocArray(*env, env->exports, env->n_exports);
  if(err < 0)
    return err;

  env->exports[env->n_exports - 1] = AllocString(*env, symbol);
  if(!env->exports[env->n_exports - 1])
    return ERR_FATAL_OUT_OF_MEMORY;
  return ERR_SUCCESS;
}

IN_ERROR innative::FinalizeEnvironment(Environment* env)
{
  if(env->cimports)
  {
    for(Embedding* embed = env->embeddings; embed != nullptr; embed = embed->next)
    {
      std::vector<std::string> symbols;

#ifdef IN_PLATFORM_WIN32
      LLD_FORMAT format = LLD_FORMAT::COFF;
#else
      LLD_FORMAT format = LLD_FORMAT::ELF;
#endif

      if(embed->size)
        symbols = GetSymbols(reinterpret_cast<const char*>(embed->data), (size_t)embed->size, env->log, format);
      else
      {
        auto testpath = [](FILE* f, const path& file, path& out) -> FILE* {
          if(!f)
          {
            out = file;
            FOPEN(f, out.c_str(), "rb");
          }
          return f;
        };

        path envpath(GetPath(env->libpath));
        path rootpath(GetPath(env->rootpath));
        path src(GetPath(reinterpret_cast<const char*>(embed->data)));
        path out;
        FILE* f = 0;

        f = testpath(f, envpath / src, out);
        f = testpath(f, src, out);
        f = testpath(f, rootpath / src, out);

#ifdef IN_PLATFORM_POSIX
        if(CURRENT_ARCH_BITS == 64)
          f = testpath(f, rootpath.parent_path() / "lib64" / src, out);
        f = testpath(f, rootpath.parent_path() / "lib" / src, out);

        if(CURRENT_ARCH_BITS == 64)
          f = testpath(f, path("/usr/lib64/") / src, out);
        f = testpath(f, path("/usr/lib/") / src, out);
#endif
        if(!f)
        {
          fprintf(env->log, "Error loading file: %s\n", src.u8string().c_str());
          return ERR_FATAL_FILE_ERROR;
        }
        fclose(f);

        std::string buf = out.u8string();
        char* tmp       = tmalloc<char>(*env, buf.size() + 1);
        if(!tmp)
          return ERR_FATAL_OUT_OF_MEMORY;
        tmemcpy<char>(tmp, buf.size() + 1, buf.c_str(), buf.size() + 1);
        embed->data = tmp;

        symbols = GetSymbols(out.u8string().c_str(), 0, env->log, format);
      }

      int r;
      for(auto symbol : symbols)
      {
        Identifier id;
        id.resize(static_cast<varuint32>(symbol.size()), true, *env);
        if(!id.get() || symbol.size() > std::numeric_limits<varuint32>::max())
          return ERR_FATAL_OUT_OF_MEMORY;

        memcpy(id.get(), symbol.data(), symbol.size());
        Identifier key = id;
        if(embed->name) // if we have a name, we are pretending all functions have name_WASM_function formatting
        {
          std::string s(key.str()); // check if the function name already has name_WASM_
          if(s.compare(0, strlen(embed->name), embed->name) || s.compare(strlen(embed->name), 6, "_WASM_"))
          {
            // if not, allocate a new identifier and append it
            s = embed->name + ("_WASM_" + s);
            key.resize(static_cast<varuint32>(s.size()), true, *env);
            if(!key.get() || s.size() > std::numeric_limits<varuint32>::max())
              return ERR_FATAL_OUT_OF_MEMORY;

            memcpy(key.get(), s.data(), s.size());
          }
        }

        auto iter                     = kh_put_cimport(env->cimports, key, &r);
        kh_value(env->cimports, iter) = key.get() != id.get();
        // On windows, because .lib files map to DLLs, they can have duplicate symbols from the dependent DLLs
        // that the DLL itself depends on. As a result, we cannot enforce this check until the linker resolves the symbols.
#ifndef IN_PLATFORM_WIN32
        if(!r)
          return ERR_INVALID_EMBEDDING;
#endif
      }
    }
  }

  while(((std::atomic<size_t>&)env->size).load(std::memory_order_relaxed) >
        ((std::atomic<size_t>&)env->n_modules).load(std::memory_order_relaxed))
    std::this_thread::sleep_for(std::chrono::milliseconds(5)); // Spin until all modules have loaded

  return ERR_SUCCESS;
}

IN_ERROR innative::Validate(Environment* env)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  // Before validating, add all modules to the modulemap. We must do this outside of LoadModule for multithreading reasons.
  // kh_clear_modules(env->modulemap);
  for(size_t i = 0; i < env->n_modules; ++i)
  {
    int r;
    khiter_t iter = kh_put_modules(env->modulemap, env->modules[i].name, &r);
    if(!r)
      return ERR_FATAL_DUPLICATE_MODULE_NAME;
    kh_val(env->modulemap, iter) = i;
  }

  ValidateEnvironment(*env);
  if(env->errors)
  {
    internal::ReverseErrorList(env->errors); // Reverse error list so it appears in chronological order
    return ERR_VALIDATION_ERROR;
  }

  return ERR_SUCCESS;
}

IN_ERROR innative::Compile(Environment* env, const char* file)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  IN_ERROR err = Validate(env);
  if(err != ERR_SUCCESS)
    return err;

  return CompileEnvironment(env, file);
}
IN_Entrypoint innative::LoadFunction(void* assembly, const char* module_name, const char* function)
{
  auto canonical = CanonicalName(StringSpan::From(module_name), StringSpan::From(function));
  return (IN_Entrypoint)LoadDLLFunction(assembly, !function ? IN_INIT_FUNCTION : canonical.c_str());
}

IN_Entrypoint innative::LoadTable(void* assembly, const char* module_name, const char* table, varuint32 index)
{
  INGlobal* ref = reinterpret_cast<INGlobal*>(
    LoadDLLFunction(assembly, CanonicalName(StringSpan::From(module_name), StringSpan::From(table)).c_str()));
  if(ref != nullptr && index < (ref->table.size / sizeof(INTableEntry)))
    return ref->table.entries[index].func;
  return nullptr;
}

INGlobal* innative::LoadGlobal(void* assembly, const char* module_name, const char* export_name)
{
  return reinterpret_cast<INGlobal*>(
    LoadDLLFunction(assembly, CanonicalName(StringSpan::From(module_name), StringSpan::From(export_name)).c_str()));
}

void* innative::LoadAssembly(const char* file)
{
  if(!file)
    return 0;

  path envpath = GetPath(file);

  return envpath.is_absolute() ? LoadDLL(envpath) : LoadDLL(GetWorkingDir() / envpath);
}

void innative::FreeAssembly(void* assembly) { FreeDLL(assembly); }

const char* innative::GetTypeEncodingString(int type_encoding)
{
  return EnumToString(TYPE_ENCODING_MAP, type_encoding, 0, 0);
}

const char* innative::GetErrorString(int error_code) { return EnumToString(ERR_ENUM_MAP, error_code, 0, 0); }

int innative::CompileScript(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output)
{
  int err = ERR_SUCCESS;
  char buf[40];
  snprintf(buf, 40, "memory%p", data);
  const char* target = buf;
  std::string dumpfile;

  // Load the module
  std::unique_ptr<uint8_t[]> data_module;
  if(!sz)
  {
    target      = reinterpret_cast<const char*>(data);
    data_module = LoadFile(GetPath(target), sz);
    if(data_module.get() == nullptr)
      return ERR_FATAL_FILE_ERROR;
    data = data_module.get();
  }
  else if(env->flags & ENV_DEBUG)
  {
    path out = u8path(output) / buf;
    if(!DumpFile(out, data, sz))
      return ERR_FATAL_FILE_ERROR;
    dumpfile = out.u8string();
    target   = dumpfile.c_str();
  }

  if(!env)
  {
    fputs("Environment cannot be null.\n", stderr);
    return ERR_FATAL_NULL_POINTER;
  }

  if(err < 0)
  {
    char buf[10];
    if(env->loglevel >= LOG_FATAL)
      FPRINTF(env->log, "Error loading environment: %s\n", EnumToString(ERR_ENUM_MAP, err, buf, 10));
    return err;
  }

  err = ParseWast(*env, data, sz, GetPath(target), always_compile, output);

  if(env->loglevel >= LOG_ERROR && err < 0)
  {
    char buf[10];
    FPRINTF(env->log, "Error loading modules: %s\n", EnumToString(ERR_ENUM_MAP, err, buf, 10));
  }

  if(env->loglevel >= LOG_NOTICE && target)
    FPRINTF(env->log, "Finished Script: %s\n", target);
  return err;
}

int innative::SerializeModule(Environment* env, size_t m, const char* out, size_t* len, bool emitdebug)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  if(m >= env->n_modules)
    return ERR_FATAL_INVALID_MODULE;

  Serializer serializer(*env, env->modules[m], 0);
  serializer.TokenizeModule(emitdebug);
  int err = CheckWatTokens(*env, env->errors, serializer.tokens, "");
  if(err < 0)
    return err;

  std::string name = env->modules[m].name.str();
  if(!len)
  {
    if(out != nullptr)
      name = out;
    else
      name += ".wat";
  }
  else
  {
    std::ostringstream ss;
    serializer.WriteTokens(ss);
    ss << std::endl;
    if(ss.tellp() < 0)
      return ERR_FATAL_FILE_ERROR;

    size_t pos = (size_t)ss.tellp();
    if(*len < pos)
    {
      *len = pos;
      return ERR_INSUFFICIENT_BUFFER;
    }

    tmemcpy<char>(const_cast<char*>(out), *len, ss.str().data(), pos);
    *len = pos;
    return ERR_SUCCESS;
  }

  std::ofstream f(name, std::ios_base::binary | std::ios_base::out | std::ios_base::trunc);
  if(f.bad())
    return ERR_FATAL_FILE_ERROR;

  serializer.WriteTokens(f);
  f << std::endl;
  return ERR_SUCCESS;
}
int innative::LoadSourceMap(Environment* env, unsigned int m, const char* path, size_t len)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;
  if(m >= env->n_modules)
    return ERR_UNKNOWN_MODULE;
  env->modules[m].sourcemap = tmalloc<SourceMap>(*env, 1);
  if(!env->modules[m].sourcemap)
    return ERR_FATAL_OUT_OF_MEMORY;
  int r = ParseSourceMap(env, env->modules[m].sourcemap, path, len);
  if(r < 0)
    env->modules[m].sourcemap = 0;
  return r;
}

template<class T, typename INT> int InsertModuleType(Environment* env, T*& root, INT& sz, INT index, const T& init)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;
  if(index > sz)
    return ERR_FATAL_INVALID_INDEX;

  INT n = sz + 1;
  T* p  = tmalloc<T>(*env, n);
  if(!p)
    return ERR_FATAL_OUT_OF_MEMORY;

  if(root)
  {
    if(index == sz)
      tmemcpy<T>(p, n, root, sz);
    else if(!index)
      tmemcpy<T>(p + 1, n - 1, root, sz);
    else
    {
      tmemcpy<T>(p, n, root, index);
      tmemcpy<T>(p + index + 1, n - index - 1, root + index, sz - index);
    }
    // memset(p + index, 0, sizeof(T));
  }
  else
    memset(p, 0, sizeof(T) * n);

  p[index] = init;
  sz       = n;
  root     = p;
  return ERR_SUCCESS;
}

int innative::InsertModuleSection(Environment* env, Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index)
{
  if(!env || !m)
    return ERR_FATAL_NULL_POINTER;

  switch(field)
  {
  case WASM_MODULE_IMPORT_TABLE: index += m->importsection.functions; break;
  case WASM_MODULE_IMPORT_MEMORY: index += m->importsection.tables; break;
  case WASM_MODULE_IMPORT_GLOBAL: index += m->importsection.memories; break;
  }

  switch(field)
  {
  case WASM_MODULE_TYPE:
    m->knownsections |= (1 << WASM_SECTION_TYPE);
    return InsertModuleType<FunctionType>(env, m->type.functypes, m->type.n_functypes, index, { 0 });
  case WASM_MODULE_IMPORT_FUNCTION: ++m->importsection.functions;
  case WASM_MODULE_IMPORT_TABLE: ++m->importsection.tables;
  case WASM_MODULE_IMPORT_MEMORY: ++m->importsection.memories;
  case WASM_MODULE_IMPORT_GLOBAL:
    m->knownsections |= (1 << WASM_SECTION_IMPORT);
    return InsertModuleType(env, m->importsection.imports, m->importsection.n_import, index, Import{});
  case WASM_MODULE_FUNCTION:
    m->knownsections |= (1 << WASM_SECTION_FUNCTION);
    return InsertModuleType<FunctionDesc>(env, m->function.funcdecl, m->function.n_funcdecl, index, { 0 });
  case WASM_MODULE_TABLE:
    m->knownsections |= (1 << WASM_SECTION_TABLE);
    return InsertModuleType<TableDesc>(env, m->table.tables, m->table.n_tables, index, { 0 });
  case WASM_MODULE_MEMORY:
    m->knownsections |= (1 << WASM_SECTION_MEMORY);
    return InsertModuleType<MemoryDesc>(env, m->memory.memories, m->memory.n_memories, index, { 0 });
  case WASM_MODULE_GLOBAL:
    m->knownsections |= (1 << WASM_SECTION_GLOBAL);
    return InsertModuleType<GlobalDecl>(env, m->global.globals, m->global.n_globals, index, { 0 });
  case WASM_MODULE_EXPORT:
    m->knownsections |= (1 << WASM_SECTION_EXPORT);
    return InsertModuleType(env, m->exportsection.exports, m->exportsection.n_exports, index, Export{});
  case WASM_MODULE_ELEMENT:
    m->knownsections |= (1 << WASM_SECTION_ELEMENT);
    return InsertModuleType<TableInit>(env, m->element.elements, m->element.n_elements, index, { 0 });
  case WASM_MODULE_CODE:
    m->knownsections |= (1 << WASM_SECTION_CODE);
    return InsertModuleType<FunctionBody>(env, m->code.funcbody, m->code.n_funcbody, index, { 0 });
  case WASM_MODULE_DATA:
    m->knownsections |= (1 << WASM_SECTION_DATA);
    return InsertModuleType<DataInit>(env, m->data.data, m->data.n_data, index, { 0 });
  case WASM_MODULE_CUSTOM: return InsertModuleType<CustomSection>(env, m->custom, m->n_custom, (size_t)index, { 0 });
  }

  return ERR_FATAL_INVALID_MODULE;
}

template<class T, typename INT> int DeleteModuleType(Environment* env, T*& root, INT& sz, INT index)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;
  if(!sz)
    return ERR_FATAL_INVALID_INDEX;

  --sz;
  if(index > sz)
    return ERR_FATAL_INVALID_INDEX;

  if(index < sz)
    memmove(root + index, root + index + 1, sizeof(T) * (sz - index));

  return ERR_SUCCESS;
}

int innative::DeleteModuleSection(Environment* env, Module* m, enum WASM_MODULE_SECTIONS field, varuint32 index)
{
  if(!env || !m)
    return ERR_FATAL_NULL_POINTER;

  switch(field)
  {
  case WASM_MODULE_IMPORT_TABLE: index += m->importsection.functions; break;
  case WASM_MODULE_IMPORT_MEMORY: index += m->importsection.tables; break;
  case WASM_MODULE_IMPORT_GLOBAL: index += m->importsection.memories; break;
  }

  switch(field)
  {
  case WASM_MODULE_TYPE: return DeleteModuleType<FunctionType>(env, m->type.functypes, m->type.n_functypes, index);
  case WASM_MODULE_IMPORT_FUNCTION: --m->importsection.functions;
  case WASM_MODULE_IMPORT_TABLE: --m->importsection.tables;
  case WASM_MODULE_IMPORT_MEMORY: --m->importsection.memories;
  case WASM_MODULE_IMPORT_GLOBAL: return DeleteModuleType(env, m->importsection.imports, m->importsection.n_import, index);
  case WASM_MODULE_FUNCTION:
    return DeleteModuleType<FunctionDesc>(env, m->function.funcdecl, m->function.n_funcdecl, index);
  case WASM_MODULE_TABLE: return DeleteModuleType<TableDesc>(env, m->table.tables, m->table.n_tables, index);
  case WASM_MODULE_MEMORY: return DeleteModuleType<MemoryDesc>(env, m->memory.memories, m->memory.n_memories, index);
  case WASM_MODULE_GLOBAL: return DeleteModuleType<GlobalDecl>(env, m->global.globals, m->global.n_globals, index);
  case WASM_MODULE_EXPORT: return DeleteModuleType(env, m->exportsection.exports, m->exportsection.n_exports, index);
  case WASM_MODULE_ELEMENT: return DeleteModuleType<TableInit>(env, m->element.elements, m->element.n_elements, index);
  case WASM_MODULE_CODE: return DeleteModuleType<FunctionBody>(env, m->code.funcbody, m->code.n_funcbody, index);
  case WASM_MODULE_DATA: return DeleteModuleType<DataInit>(env, m->data.data, m->data.n_data, index);
  case WASM_MODULE_CUSTOM: return DeleteModuleType<CustomSection>(env, m->custom, m->n_custom, (size_t)index);
  }

  return ERR_FATAL_INVALID_MODULE;
}

int innative::SetByteArray(Environment* env, ByteArray* bytearray, const void* data, varuint32 size)
{
  if(!env || !bytearray)
    return ERR_FATAL_NULL_POINTER;
  bytearray->resize(size, false, *env);
  tmemcpy<uint8_t>(bytearray->get(), bytearray->size(), (const uint8_t*)data, size);
  return ERR_SUCCESS;
}

int innative::SetIdentifier(Environment* env, Identifier* identifier, const char* str)
{
  if(!env || !identifier)
    return ERR_FATAL_NULL_POINTER;
  size_t len = strlen(str);
  identifier->resize(static_cast<varuint32>(len), true, *env);
  tmemcpy<char>(reinterpret_cast<char*>(identifier->get()), identifier->size(), str, len);
  return ERR_SUCCESS;
}

int innative::InsertModuleLocal(Environment* env, FunctionBody* body, varuint32 index, varsint7 type, varuint32 count,
                                DebugInfo* info)
{
  if(!body)
    return ERR_FATAL_NULL_POINTER;

  FunctionLocal local = { count, type };

  if(info)
    local.debug = *info;

  return InsertModuleType(env, body->locals, body->n_locals, index, local);
}

int innative::RemoveModuleLocal(Environment* env, FunctionBody* body, varuint32 index)
{
  if(!body)
    return ERR_FATAL_NULL_POINTER;
  return DeleteModuleType(env, body->locals, body->n_locals, index);
}

int innative::InsertModuleInstruction(Environment* env, FunctionBody* body, varuint32 index, Instruction* ins)
{
  if(!body || !ins)
    return ERR_FATAL_NULL_POINTER;
  return InsertModuleType(env, body->body, body->n_body, index, *ins);
}

int innative::RemoveModuleInstruction(Environment* env, FunctionBody* body, varuint32 index)
{
  if(!body)
    return ERR_FATAL_NULL_POINTER;
  return DeleteModuleType(env, body->body, body->n_body, index);
}

int innative::InsertModuleParam(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index, varsint7 param,
                                DebugInfo* name)
{
  if(!func)
    return ERR_FATAL_NULL_POINTER;
  if(name || desc)
  {
    auto n  = func->n_params;
    int err = InsertModuleType(env, desc->param_debug, n, index, !name ? DebugInfo{ 0 } : *name);
    if(err < 0)
      return err;
  }
  return InsertModuleType(env, func->params, func->n_params, index, param);
}

int innative::RemoveModuleParam(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index)
{
  if(!func)
    return ERR_FATAL_NULL_POINTER;
  if(desc && desc->param_debug)
  {
    auto n  = func->n_params;
    int err = DeleteModuleType(env, desc->param_debug, n, index);
    if(err < 0)
      return err;
  }
  return DeleteModuleType(env, func->params, func->n_params, index);
}

int innative::InsertModuleReturn(Environment* env, FunctionType* func, varuint32 index, varsint7 result)
{
  if(!func)
    return ERR_FATAL_NULL_POINTER;
  return InsertModuleType(env, func->returns, func->n_returns, index, result);
}

int innative::RemoveModuleReturn(Environment* env, FunctionType* func, varuint32 index)
{
  if(!func)
    return ERR_FATAL_NULL_POINTER;
  return DeleteModuleType(env, func->returns, func->n_returns, index);
}

INModuleMetadata* innative::GetModuleMetadata(void* assembly, uint32_t module_index)
{
  return reinterpret_cast<INModuleMetadata*>(
    LoadDLLFunction(assembly, CanonicalName(StringSpan(), StringSpan::From(IN_METADATA_PREFIX), module_index).c_str()));
}
IN_Entrypoint innative::LoadTableIndex(void* assembly, uint32_t module_index, uint32_t table_index,
                                       varuint32 function_index)
{
  auto metadata = GetModuleMetadata(assembly, module_index);
  if(!metadata || table_index >= metadata->n_tables)
    return nullptr;
  INGlobal* table = reinterpret_cast<INGlobal*>(metadata->tables[table_index]);
  if(function_index < (table->table.size / sizeof(INTableEntry)))
    return table->table.entries[function_index].func;
  return nullptr;
}
INGlobal* innative::LoadGlobalIndex(void* assembly, uint32_t module_index, uint32_t global_index)
{
  auto metadata = GetModuleMetadata(assembly, module_index);
  if(!metadata || global_index >= metadata->n_globals)
    return nullptr;
  return metadata->globals[global_index];
}
INGlobal* innative::LoadMemoryIndex(void* assembly, uint32_t module_index, uint32_t memory_index)
{
  auto metadata = GetModuleMetadata(assembly, module_index);
  if(!metadata || memory_index >= metadata->n_memories)
    return nullptr;
  return reinterpret_cast<INGlobal*>(metadata->memories[memory_index]);
}
int innative::ReplaceTableFuncPtr(void* assembly, uint32_t module_index, uint32_t table_index, const char* function,
                                  IN_Entrypoint replace)
{
  auto metadata = GetModuleMetadata(assembly, module_index);
  if(!metadata)
    return ERR_FATAL_INVALID_MODULE;
  if(table_index >= metadata->n_tables)
    return ERR_INVALID_TABLE_INDEX;
  auto target =
    (IN_Entrypoint)LoadDLLFunction(assembly,
                                   CanonicalName(StringSpan::From(metadata->name), StringSpan::From(function)).c_str());
  if(!target)
    return ERR_INVALID_FUNCTION_INDEX;
  auto& table = metadata->tables[table_index];
  for(uint64_t i = 0; i < table->size; ++i)
    if(table->entries[i].func == target)
    {
      table->entries[i].func = replace;
      return ERR_SUCCESS;
    }

  return ERR_FUNCTION_BODY_MISMATCH;
}
