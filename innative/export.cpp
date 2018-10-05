// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "innative/path.h"
#include "tools.h"
#include "wast.h"
#include <stdio.h>
#include <iostream>
#include <memory>
#include <string>

using namespace innative;

// Return pointers to all our internal functions
void innative_runtime(IRExports* exports)
{
  exports->CreateEnvironment = &CreateEnvironment;
  exports->AddModule = &AddModule;
  exports->AddWhitelist = &AddWhitelist;
  exports->WaitForLoad = &WaitForLoad;
  exports->AddEmbedding = &AddEmbedding;
  exports->Compile = &Compile;
  exports->LoadFunction = &LoadFunction;
  exports->LoadGlobal = &LoadGlobal;
  exports->LoadAssembly = &LoadAssembly;
  exports->DestroyEnvironment = &DestroyEnvironment;
}

int innative_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const struct _IR_WHITELIST* whitelist, unsigned int n_whitelist, const char* arg0)
{
  // Then create the runtime environment with the module count.
  Environment* env = CreateEnvironment(flags, 1, 0, arg0);
  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -1;
  }

  if(n_whitelist < 0) // This indicates we should enforce a whitelist that is empty, forbidding all C imports
    AddWhitelist(env, nullptr, nullptr, nullptr);
  else
  {
    for(unsigned int i = 0; i < n_whitelist; ++i)
      AddWhitelist(env, whitelist[i].module_name, whitelist[i].export_name, &whitelist[i].sig);
  }

  // Load the module
  long sz = 0;
  int err = ERR_SUCCESS;
  auto data_module = utility::LoadFile(file, sz);
  Path name(file);

  if(sz > 0)
    AddModule(env, data_module.get(), sz, name.RemoveExtension().Get().c_str(), &err);
  else
    err = -1;

  if(err < 0)
  {
    fprintf(stderr, "Error loading modules: 0x%x\n", -err);
    return err;
  }

  // Add all embedding environments that are included with this runtime
  err = AddEmbedding(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

  if(err < 0)
  {
    fprintf(stderr, "Error loading environment: %i\n", err);
    return err;
  }

  // Attempt to compile. If an error happens, output it and any validation errors to stderr
  err = Compile(env, out);
  if(err < 0)
  {
    fprintf(stderr, "Compile error: %i\n", err);

    for(ValidationError* err = env->errors; err != nullptr; err = err->next)
      fprintf(stderr, "Error %i: %s\n", err->code, err->error);

    getchar();
    return err;
  }

  // Destroy environment now that compilation is complete
  DestroyEnvironment(env);
  void* assembly = LoadAssembly(flags, out);
  if(!assembly)
    return ERR_FATAL_FILE_ERROR;
  IR_Entrypoint start = LoadFunction(assembly, 0, 0, 0);
  if(!start)
    return ERR_INVALID_START_FUNCTION;
  (*start)();
  return ERR_SUCCESS;
}

int innative_build_loader(struct _IR_CHUNK* chunks, const char* out, bool dynamic)
{
  return -1;
}

int innative_compile_script(const uint8_t* data, size_t sz, Environment* env)
{
  int err = ERR_SUCCESS;
  char buf[40];
  sprintf(buf, "/[memory:%p]", data);
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
    fprintf(stderr, "Environment cannot be null.\n");
    return -1;
  }

  if(err < 0)
  {
    fprintf(stderr, "Error loading environment: %i\n", err);
    return err;
  }

  err = wat::ParseWast(*env, data, sz, path);

  if(err < 0)
    fprintf(stderr, "Error loading modules: %i\n", err);

  if(path)
    std::cout << "Finished Script: " << path << std::endl;
  return err;
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).BaseDir().Get().c_str());
}
