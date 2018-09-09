// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "tools.h"
#include "path.h"
#include "wast.h"
#include <stdio.h>
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

int innative_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const _IR_WHITELIST* whitelist, int n_whitelist)
{
  // Then create the runtime environment with the module count.
  Environment* env = CreateEnvironment(flags, 1, 0);
  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -1;
  }

  if(n_whitelist < 0) // This indicates we should enforce a whitelist that is empty, forbidding all C imports
    AddWhitelist(env, nullptr, nullptr, nullptr);
  else
  {
    for(int i = 0; i < n_whitelist; ++i)
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
#ifdef IR_DEBUG
  err = AddEmbedding(env, 0, (void*)"innative-env_d.lib", 0);
#else
  err = AddEmbedding(env, 0, (void*)"innative-env.lib", 0);
#endif

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

    int i = 0;
    scanf_s("%i", &i);
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

  if(!env)
  {
    fprintf(stderr, "Environment cannot be null.\n");
    return -1;
  }

  // Add all embedding environments that are included with this runtime
#ifdef IR_DEBUG
  err = AddEmbedding(env, 0, (void*)"innative-env_d.lib", 0);
#else
  err = AddEmbedding(env, 0, (void*)"innative-env.lib", 0);
#endif

  if(err < 0)
  {
    fprintf(stderr, "Error loading environment: %i\n", err);
    return err;
  }

  err = wat::ParseWast(*env, data, sz);

  if(err < 0)
    fprintf(stderr, "Error loading modules: %i\n", err);

  return err;
}

int innative_compile_script_file(const char* file, Environment* env)
{
  // Load the module
  long sz = 0;
  auto data_module = utility::LoadFile(file, sz);
  if(data_module.get() == nullptr)
    return ERR_FATAL_FILE_ERROR;

  return innative_compile_script(data_module.get(), sz, env);
}

void innative_set_work_dir_to_bin()
{
  utility::SetWorkingDir(utility::GetProgramPath().Get().c_str());
}
