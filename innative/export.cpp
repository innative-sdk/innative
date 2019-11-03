// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
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
  exports->CreateEnvironment     = &CreateEnvironment;
  exports->AddModule             = &AddModule;
  exports->AddWhitelist          = &AddWhitelist;
  exports->AddEmbedding          = &AddEmbedding;
  exports->FinalizeEnvironment   = &FinalizeEnvironment;
  exports->Compile               = &Compile;
  exports->LoadFunction          = &LoadFunction;
  exports->LoadTable             = &LoadTable;
  exports->LoadGlobal            = &LoadGlobal;
  exports->LoadAssembly          = &LoadAssembly;
  exports->FreeAssembly          = &FreeAssembly;
  exports->ClearEnvironmentCache = &ClearEnvironmentCache;
  exports->DestroyEnvironment    = &DestroyEnvironment;
}

int innative_compile_script(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output)
{
  int err = ERR_SUCCESS;
  char buf[40];
#ifdef IN_PLATFORM_WIN32
  snprintf(buf, 40, "memory--%p", data);
#else
  snprintf(buf, 40, "/memory--%p", data);
#endif
  const char* target = buf;

  // Load the module
  std::unique_ptr<uint8_t[]> data_module;
  if(!sz)
  {
    long len;
    target      = reinterpret_cast<const char*>(data);
    data_module = utility::LoadFile(u8path(target), len);
    if(data_module.get() == nullptr)
      return ERR_FATAL_FILE_ERROR;
    data = data_module.get();
    sz   = len;
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

  err = ParseWast(*env, data, sz, u8path(target), always_compile, output);

  if(env->loglevel >= LOG_ERROR && err < 0)
  {
    char buf[10];
    FPRINTF(env->log, "Error loading modules: %s\n", utility::EnumToString(utility::ERR_ENUM_MAP, err, buf, 10));
  }

  if(env->loglevel >= LOG_NOTICE && target)
    FPRINTF(env->log, "Finished Script: %s\n", target);
  return err;
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).parent_path().u8string().c_str());
}

int innative_install(const char* arg0, bool full) { return utility::Install(arg0, full); }

int innative_uninstall() { return utility::Uninstall(); }
const char* innative_type_encoding_string(int type_encoding)
{
  return utility::EnumToString(utility::TYPE_ENCODING_MAP, type_encoding, 0, 0);
}
const char* innative_error_string(int error_code) { return utility::EnumToString(utility::ERR_ENUM_MAP, error_code, 0, 0); }

int innative_serialize_module(Environment* env, size_t m, const char* out)
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
  if(out != nullptr)
    name = out;
  else
    name += ".wat";

  std::ofstream f(name, std::ios_base::binary | std::ios_base::out | std::ios_base::trunc);
  if(f.bad())
    return ERR_FATAL_FILE_ERROR;

  wat::WriteTokens(tokens, f);
  return ERR_SUCCESS;
}
