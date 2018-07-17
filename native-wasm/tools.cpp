// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include "tools.h"
#include "path.h"
#include <stdio.h>
#include <memory>
#include <string>

std::unique_ptr<uint8_t[]> loadfile(const char* file, long& sz)
{
  FILE* f = 0;
  fopen_s(&f, file, "rb");
  if(!f)
    return nullptr;
  fseek(f, 0, SEEK_END);
  sz = ftell(f);
  fseek(f, 0, SEEK_SET);
  std::unique_ptr<uint8_t[]> data(new uint8_t[sz]);
  sz = (long)fread(data.get(), 1, sz, f);
  fclose(f);
  return data;
}

int native_wasm_compile_file(const char* file, const char* out, unsigned int flags, bool dynamic, const _NW_WHITELIST* whitelist, int n_whitelist)
{
  // Then create the runtime environment with the module count.
  Environment* env = CreateEnvironment(flags, 1, 0);
  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -1;
  }

  if(n_whitelist < 0) // This indicates we should enforce a whitelist that is empty, forbidding all C imports
    AddWhitelist(env, 0, 0, 0);
  else
  {
    for(int i = 0; i < n_whitelist; ++i)
      AddWhitelist(env, whitelist[i].module_name, whitelist[i].export_name, &whitelist[i].sig);
  }

  // Load the module
  long sz = 0;
  int err = 0;
  auto data_module = loadfile(file, sz);
  NWPath name(file);

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
#ifdef NW_DEBUG
  err = AddEmbedding(env, 0, (void*)"native-wasm-env_d.lib", 0);
#else
  err = AddEmbedding(env, 0, (void*)"native-wasm-env.lib", 0);
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

    for(ValidationError* err = env->errors; err != 0; err = err->next)
      fprintf(stderr, "Error %i: %s\n", err->code, err->error);

    int i = 0;
    scanf_s("%i", &i);
    return err;
  }

  // Destroy environment now that compilation is complete
  DestroyEnvironment(env);
  //cache = LoadCache(flags);

  return Run(0);
}
int native_wasm_build_loader(struct _NW_CHUNK* chunks, const char* out, bool dynamic)
{
  return -1;
}