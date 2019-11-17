// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "parse.h"
#include "validate.h"
#include "compile.h"
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
  Environment* env = (Environment*)calloc(1, sizeof(Environment));
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
    env->rootpath   = utility::AllocString(*env, GetProgramPath(arg0).parent_path().u8string());
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

void innative::LoadModule(Environment* env, size_t index, const void* data, uint64_t size, const char* name,
                          const char* file, int* err)
{
  Stream s = { (uint8_t*)data, (size_t)size, 0 };
  std::string fallback;
  if(!name)
  {
    fallback = "m" + std::to_string(index);
    name     = fallback.data();
  }

  if((env->flags & ENV_ENABLE_WAT) && size > 0 && s.data[0] != 0)
  {
    env->modules[index] = { 0 };
    *err = innative::ParseWatModule(*env, env->modules[index], s.data, (size_t)size, StringRef{ name, strlen(name) });
  }
  else
    *err = ParseModule(s, *env, env->modules[index], ByteArray((uint8_t*)name, (varuint32)strlen(name)), env->errors);

  env->modules[index].path = utility::AllocString(*env, file);
  ((std::atomic<size_t>&)env->n_modules).fetch_add(1, std::memory_order_release);
}

void innative::AddModule(Environment* env, const void* data, uint64_t size, const char* name, int* err)
{
  if(!env || !err)
  {
    *err = ERR_FATAL_NULL_POINTER;
    return;
  }

  const char* file = nullptr;
  std::unique_ptr<uint8_t[]> data_module;
  if(!size)
  {
    long sz     = 0;
    file        = (const char*)data;
    data_module = utility::LoadFile(file, sz);
    if(data_module.get() == nullptr)
    {
      *err = ERR_FATAL_FILE_ERROR;
      return;
    }
    size = sz;
    data = data_module.get();
  }

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
      return;
    }
    ((std::atomic<size_t>&)env->capacity).store(index * 2, std::memory_order_release);
  }

  if(env->flags & ENV_MULTITHREADED)
    std::thread(LoadModule, env, index, data, size, name, file, err).detach();
  else
    LoadModule(env, index, data, size, name, file, err);
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

IN_ERROR innative::AddEmbedding(Environment* env, int tag, const void* data, uint64_t size)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  Embedding* embed = tmalloc<Embedding>(*env, 1);
  if(!embed)
    return ERR_FATAL_OUT_OF_MEMORY;

  embed->tag      = tag;
  embed->data     = data;
  embed->size     = size;
  embed->next     = env->embeddings;
  env->embeddings = embed;

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
        symbols = GetSymbols((const char*)embed->data, (size_t)embed->size, env->log, format);
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

        path envpath(utility::GetPath(env->libpath));
        path rootpath(utility::GetPath(env->rootpath));
        path src(utility::GetPath((const char*)embed->data));
        path out;
        FILE* f = 0;

        f = testpath(f, envpath / src, out);
        f = testpath(f, src, out);
        f = testpath(f, rootpath / src, out);

#ifdef IN_PLATFORM_POSIX
        if(CURRENT_ARCH_BITS == 64)
          f = testpath(f, rootpath.parent_path() / "lib64", out);
        f = testpath(f, rootpath.parent_path() / "lib", out);

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
        id.resize((varuint32)symbol.size(), true, *env);
        if(!id.get() || symbol.size() > std::numeric_limits<varuint32>::max())
          return ERR_FATAL_OUT_OF_MEMORY;

        memcpy(id.get(), symbol.data(), symbol.size());
        kh_put_cimport(env->cimports, id, &r);
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
  auto canonical = utility::CanonicalName(StringRef::From(module_name), StringRef::From(function));
  return (IN_Entrypoint)LoadDLLFunction(assembly, !function ? IN_INIT_FUNCTION : canonical.c_str());
}

struct IN_TABLE
{
  IN_Entrypoint func;
  varuint32 type;
};

IN_Entrypoint innative::LoadTable(void* assembly, const char* module_name, const char* table, varuint32 index)
{
  IN_TABLE* ref =
    (IN_TABLE*)LoadDLLFunction(assembly,
                               utility::CanonicalName(StringRef::From(module_name), StringRef::From(table)).c_str());
  return !ref ? nullptr : ref[index].func;
}

IRGlobal* innative::LoadGlobal(void* assembly, const char* module_name, const char* export_name)
{
  return (IRGlobal*)LoadDLLFunction(
    assembly, utility::CanonicalName(StringRef::From(module_name), StringRef::From(export_name)).c_str());
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
  return utility::EnumToString(utility::TYPE_ENCODING_MAP, type_encoding, 0, 0);
}

const char* innative::GetErrorString(int error_code)
{
  return utility::EnumToString(utility::ERR_ENUM_MAP, error_code, 0, 0);
}

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
    long len;
    target      = reinterpret_cast<const char*>(data);
    data_module = utility::LoadFile(utility::GetPath(target), len);
    if(data_module.get() == nullptr)
      return ERR_FATAL_FILE_ERROR;
    data = data_module.get();
    sz   = len;
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
      FPRINTF(env->log, "Error loading environment: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
    return err;
  }

  err = ParseWast(*env, data, sz, utility::GetPath(target), always_compile, output);

  if(env->loglevel >= LOG_ERROR && err < 0)
  {
    char buf[10];
    FPRINTF(env->log, "Error loading modules: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
  }

  if(env->loglevel >= LOG_NOTICE && target)
    FPRINTF(env->log, "Finished Script: %s\n", target);
  return err;
}

int innative::SerializeModule(Environment* env, size_t m, const char* out, size_t* len)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  if(m >= env->n_modules)
    return ERR_FATAL_INVALID_MODULE;

  Queue<WatToken> tokens;
  wat::TokenizeModule(*env, tokens, env->modules[m]);
  int err = CheckWatTokens(*env, env->errors, tokens, "");
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
    wat::WriteTokens(tokens, ss);
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

  wat::WriteTokens(tokens, f);
  return ERR_SUCCESS;
}
