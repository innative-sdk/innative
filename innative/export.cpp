// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "innative/path.h"
#include "serialize.h"
#include "tools.h"
#include "wast.h"
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>

using namespace innative;

// Return pointers to all our internal functions
void innative_runtime(IRExports* exports)
{
  exports->CreateEnvironment = &CreateEnvironment;
  exports->AddModule = &AddModule;
  exports->AddWhitelist = &AddWhitelist;
  exports->AddEmbedding = &AddEmbedding;
  exports->FinalizeEnvironment = &FinalizeEnvironment;
  exports->Compile = &Compile;
  exports->LoadFunction = &LoadFunction;
  exports->LoadGlobal = &LoadGlobal;
  exports->LoadAssembly = &LoadAssembly;
  exports->FreeAssembly = &FreeAssembly;
  exports->ClearEnvironmentCache = &ClearEnvironmentCache;
  exports->DestroyEnvironment = &DestroyEnvironment;
}

int innative_compile_file(
  const char* file, 
  const char* out, 
  uint64_t flags, 
  uint64_t optimize,
  uint64_t features,
  bool dynamic,
  const struct _IN_WHITELIST* whitelist, 
  unsigned int n_whitelist,
  const char* arg0)
{
  // Then create the runtime environment with the module count.
  Environment* env = CreateEnvironment(1, 0, arg0);
  env->flags = flags;
  env->optimize = optimize;
  env->features = features;

  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -1;
  }

  if(n_whitelist < 0) // This indicates we should enforce a whitelist that is empty, forbidding all C imports
    AddWhitelist(env, nullptr, nullptr);
  else
  {
    for(unsigned int i = 0; i < n_whitelist; ++i)
      AddWhitelist(env, whitelist[i].module_name, whitelist[i].export_name);
  }

  // Load the module
  long sz = 0;
  int err = ERR_SUCCESS;
  auto data_module = utility::LoadFile(file, sz);
  Path name(file);

  if(sz > 0)
    AddModule(env, data_module.get(), sz, name.RemoveExtension().c_str(), &err);
  else
    err = -1;

  if(err < 0)
  {
    char buf[10];
    if(env->loglevel >= LOG_FATAL)
      fprintf(env->log, "Error loading modules: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
    return err;
  }

  // Add all embedding environments that are included with this runtime
  err = AddEmbedding(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

  if(err < 0)
  {
    char buf[10];
    if(env->loglevel >= LOG_FATAL)
      fprintf(env->log, "Error loading environment: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
    return err;
  }

  FinalizeEnvironment(env);

  // Attempt to compile. If an error happens, output it and any validation errors to the log
  err = Compile(env, out);
  if(err < 0)
  {
    if(env->loglevel >= LOG_ERROR)
    {
      char buf[10];
      fprintf(env->log, "Compile error: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));

      for(ValidationError* e = env->errors; e != nullptr; e = e->next)
        fprintf(env->log, "Error %s: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, e->code, buf, 10), e->error);
    }

    getchar();
    return err;
  }

  // Destroy environment now that compilation is complete
  DestroyEnvironment(env);
  void* assembly = LoadAssembly(out);
  if(!assembly)
    return ERR_FATAL_FILE_ERROR;
  IN_Entrypoint start = LoadFunction(assembly, 0, 0);
  if(!start)
    return ERR_INVALID_START_FUNCTION;
  (*start)();
  return ERR_SUCCESS;
}

int innative_build_loader(struct _IN_CHUNK* chunks, const char* out, bool dynamic)
{
  return -1;
}

int innative_compile_script(const uint8_t* data, size_t sz, Environment* env, bool always_compile)
{
  int err = ERR_SUCCESS;
  char buf[40];
#ifdef IN_PLATFORM_WIN32
  snprintf(buf, 40, "memory--%p", data);
#else
  snprintf(buf, 40, "/memory--%p", data);
#endif
  const char* path = buf;

  // Load the module
  std::unique_ptr<uint8_t[]> data_module;
  if(!sz)
  {
    long len;
    path = reinterpret_cast<const char*>(data);
    data_module = utility::LoadFile(path, len);
    if(data_module.get() == nullptr)
      return ERR_FATAL_FILE_ERROR;
    data = data_module.get();
    sz = len;
  }

  if(!env)
  {
    fputs("Environment cannot be null.\n", stderr);
    return -1;
  }

  if(err < 0)
  {
    char buf[10];
    if(env->loglevel >= LOG_FATAL)
      FPRINTF(env->log, "Error loading environment: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
    return err;
  }

  err = wat::ParseWast(*env, data, sz, path, always_compile);

  if(env->loglevel >= LOG_ERROR && err < 0)
  {
    char buf[10];
    FPRINTF(env->log, "Error loading modules: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
  }

  if(env->loglevel >= LOG_NOTICE && path)
    FPRINTF(env->log, "Finished Script: %s\n", path);
  return err;
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).BaseDir().c_str());
}

int innative_install(const char* arg0, bool full)
{
  return utility::Install(arg0, full);
}

int innative_uninstall()
{
  return utility::Uninstall();
}
const char* innative_type_encoding_string(int type_encoding)
{
  return utility::EnumToString(utility::TYPE_ENCODING_MAP, type_encoding, 0, 0);
}
const char* innative_error_string(int error_code)
{
  return utility::EnumToString(utility::ERR_ENUM_MAP, error_code, 0, 0);
}

int innative_serialize_module(Environment* env, size_t m, const char* out)
{
  if(!env)
    return ERR_FATAL_NULL_POINTER;

  if(m >= env->n_modules)
    return ERR_FATAL_INVALID_MODULE;

  Queue<wat::WatToken> tokens;
  TokenizeModule(*env, tokens, env->modules[m]);
  int err = wat::CheckWatTokens(*env, env->errors, tokens, "");
  if(err < 0)
    return err;

  std::string name = env->modules[m].name.str();
  if(out != nullptr)
    name = out;
  else
    name += ".wat";

  std::ofstream f(name, std::ios_base::binary|std::ios_base::out|std::ios_base::trunc);
  if(f.bad())
    return ERR_FATAL_FILE_ERROR;

  WriteTokens(tokens, f);
  return ERR_SUCCESS;
}
