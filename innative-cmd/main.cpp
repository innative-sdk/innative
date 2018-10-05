// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "innative/path.h"
#include <iostream>
#include <vector>
#include <fstream>
#include <unordered_map>
#include <functional>
#include <algorithm>

static const std::unordered_map<std::string, unsigned int> flag_map = {
  { "strict", ENV_STRICT },
  { "multithreaded", ENV_MULTITHREADED },
  { "debug", ENV_DEBUG },
  { "dll", ENV_DLL },
  { "wat", ENV_ENABLE_WAT },
  { "homogenize", ENV_HOMOGENIZE_FUNCTIONS },
  { "opt_inline", ENV_OPTIMIZE_INLINE },
  { "opt_analysis", ENV_OPTIMIZE_ANALYSIS },
  { "opt_vectorize", ENV_OPTIMIZE_VECTORIZE },
  { "opt_all", ENV_OPTIMIZE_ALL },
};

int main(int argc, char *argv[])
{
  std::vector<const char*> inputs;
  std::vector<const char*> embeddings;
  std::vector<const char*> whitelist;
  unsigned int flags = ENV_ENABLE_WAT; // Always enable WAT 
  std::string out;
  std::vector<const char*> wast; // WAST files will be executed in the order they are specified, after all other modules are injected into the environment
  const char* sdkpath = 0;
  const char* linker = 0;
  bool run = false;
  bool generate = false;
  int err = ERR_SUCCESS;
  IRExports exports;
  innative_runtime(&exports);

  for(int i = 1; i < argc; ++i) // skip first argument, which is the program path
  {
    if(argv[i][0] == '-')
    {
      switch(argv[i][1])
      {
      case 'r': // run immediately
        run = true;
        flags |= ENV_DLL;
        break;
      case 'f': // flag
      {
        auto raw = std::string(argv[i] + 2);
        std::transform(raw.begin(), raw.end(), raw.begin(), ::tolower);
        auto flag = flag_map.lower_bound(raw);
        if(flag == flag_map.end())
        {
          std::cout << "Unknown flag: " << argv[i] + 2 << std::endl;
          err = -4;
        }
        else
          flags |= flag->second;
        break;
      }
      case 'l': // lib
        embeddings.push_back(argv[i] + 2);
        break;
      case 'o': // out
        out = argv[i] + 2;
        break;
      case 'g': // generate loader
        generate = true;
        break;
      case 'w': // whitelist
        break;
      case 'a': // animal sacrifice (specify an alternative linker)
        linker = argv[i] + 2;
        break;
      case 'd': // Specify EXE directory, or any directory containing SDK libraries.
        sdkpath = argv[i] + 2;
        break;
      default:
        std::cout << "Unknown command line option: " << argv[i] << std::endl;
        err = -5;
        break;
      }

    }
    else if(!STRICMP(innative::Path(argv[i]).Extension().c_str(), "wast")) // Check if this is a .wast script
      wast.push_back(argv[i]);
    else // Everything else is an input file
      inputs.push_back(argv[i]);
  }

  if(err < 0)
  {
    std::cout << "Error parsing command line arguments: " << err << ", aborting." << std::endl;
    return err;
  }

  if(!inputs.size())
  {
    std::cout << "No input files specified!" << std::endl;
    return -3;
  }

  // If we are generating a loader EXE instead of compiling, we go down a completely different code path.
  if(generate)
  {
    // TODO
  }

  if(out.empty()) // If no out is specified, default to name of first input file
    out = innative::Path(inputs[0]).RemoveExtension().Get() + ((flags&ENV_DLL) ? ".dll" : ".exe");

  // Then create the runtime environment with the module count.
  Environment* env = (*exports.CreateEnvironment)(flags, inputs.size(), 0, (!argc ? 0 : argv[0]));
  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -2;
  }

  if(sdkpath)
    env->sdkpath = sdkpath;
  if(linker)
    env->linker = linker;

  /*if(whitelist != nullptr)
  {
    if(!whitelist[0]) // This indicates we should enforce a whitelist that is empty, forbidding all C imports
      (*exports.AddWhitelist)(env, nullptr, nullptr, nullptr);
    else
    {
      char* ctx;
      char* token = STRTOK(whitelist, ",", &ctx);
      while(token)
      {
        (*exports.AddWhitelist)(env, "", token, &whitelist[i].sig);
        token = STRTOK(NULL, ",", &ctx);
      }
    }
  }*/

  // Load all modules
  for(size_t i = 0; i < inputs.size(); ++i)
    (*exports.AddModule)(env, inputs[i], 0, innative::Path(inputs[i]).File().RemoveExtension().Get().c_str(), &err);

  if(err < 0)
  {
    fprintf(stderr, "Error loading modules: 0x%x\n", -err);
    return err;
  }

  // Add all embedding environments, plus the default environment
  embeddings.push_back(INNATIVE_DEFAULT_ENVIRONMENT);
  
  for(size_t i = 0; i < embeddings.size(); ++i)
    (*exports.AddEmbedding)(env, 0, embeddings[i], 0);

  if(err < 0)
  {
    fprintf(stderr, "Error loading environment: %i\n", err);
    return err;
  }

  // Check if this is a .wast file, which must be handled differently because it's an entire environment
  if(wast.size() > 0)
  {
    for(size_t i = 0; i < wast.size() && !err; ++i)
      err = innative_compile_script((const uint8_t*)wast[i], 0, env);
  }
  else // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env, out.c_str());

  if(err < 0)
  {
    fprintf(stderr, "Compile error: %i\n", err);

    for(ValidationError* err = env->errors; err != nullptr; err = err->next)
      fprintf(stderr, "Error %i: %s\n", err->code, err->error);

    getchar();
    return err;
  }

  // Destroy environment now that compilation is complete
  (*exports.DestroyEnvironment)(env);

  // Automatically run the assembly, but only if there were no wast scripts (which are always executed)
  if(run && !wast.size())
  {
    void* assembly = (*exports.LoadAssembly)(flags, out.c_str()); // This automatically calls the start function on load
    if(!assembly)
      return ERR_FATAL_FILE_ERROR;
    //IR_Entrypoint start = (*exports.LoadFunction)(assembly, 0, 0, 0);
    //if(!start)
    //  return ERR_INVALID_START_FUNCTION;
    //(*start)();
    return ERR_SUCCESS;
  }

  return err;
}