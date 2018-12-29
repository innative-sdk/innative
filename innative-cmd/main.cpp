// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "innative/path.h"
#include <iostream>
#include <vector>
#include <fstream>
#include <unordered_map>
#include <functional>
#include <algorithm>

#ifdef IR_PLATFORM_WIN32
#include "../innative/win32.h"

inline std::unique_ptr<uint8_t[]> LoadFile(const char* file, long& sz)
{
  FILE* f = nullptr;
  FOPEN(f, file, "rb");
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
#endif

static const std::unordered_map<std::string, unsigned int> flag_map = {
  { "strict", ENV_STRICT },
  { "sandbox", ENV_SANDBOX },
  { "multithreaded", ENV_MULTITHREADED },
  { "debug", ENV_DEBUG },
  { "library", ENV_LIBRARY },
  { "wat", ENV_ENABLE_WAT },
  { "llvm", ENV_EMIT_LLVM },
  { "noinit", ENV_NO_INIT },
  { "homogenize", ENV_HOMOGENIZE_FUNCTIONS },
};

void usage()
{
  std::cout << "Usage: innative-cmd [-r] [-f FLAG] [-l FILE] [-o FILE] [-a FILE] [-d PATH]\n"
    "  -r : Run the compiled result immediately and display output. Requires a start function.\n"
    "  -f : Set a supported flag to true. Flags:\n";
 
  for(auto& f : flag_map)
    std::cout << "         " << f.first << "\n";

  std::cout << "\n"
    "  -l <FILE> : Links the input files against <FILE>, which must be a static library.\n"
    "  -s <FILE> : Links the input files against <FILE>, which must be an ELF shared library.\n"
    "  -o <FILE> : Sets the output path for the resulting executable or library.\n"
    "  -a <FILE> : Specifies an alternative linker to use instead of LLD.\n"
    "  -d <PATH> : Sets the directory that contains the SDK library and data files.\n"
    "  -w <[MODULE:]FUNCTION> : whitelists a given C import, does name-mangling if the module is specified.\n"
    "  -i : Installs this innative SDK to the host operating system.\n"
    "  -u : Uninstalls and deregisters this SDK from the host operating system.\n"
    "  -v : Turns on verbose logging."
    << std::endl;
}

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
  bool verbose = true;
  int err = ERR_SUCCESS;

  for(int i = 1; i < argc; ++i) // skip first argument, which is the program path
  {
    if(argv[i][0] == '-')
    {
      switch(argv[i][1])
      {
      case 'r': // run immediately
        run = true;
        flags |= ENV_LIBRARY| ENV_NO_INIT;
        break;
      case 'f': // flag
      {
        auto raw = std::string(argv[i] + 2);
        std::transform(raw.begin(), raw.end(), raw.begin(), ::tolower);
        auto flag = flag_map.find(raw);
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
        whitelist.push_back(argv[i] + 2);
        break;
      case 'a': // animal sacrifice (specify an alternative linker)
        linker = argv[i] + 2;
        break;
      case 'd': // Specify EXE directory, or any directory containing SDK libraries.
        sdkpath = argv[i] + 2;
        break;
      case 'i': // install
        std::cout << "Installing inNative Runtime..." << std::endl;
        err = innative_install(argv[0], true);
        if(err < 0)
          std::cout << "Installation failed!" << std::endl;
        else
          std::cout << "Installation succeeded!" << std::endl;
        return err;
      case 'u': // uninstall
        std::cout << "Uninstalling inNative Runtime..." << std::endl;
        err = innative_uninstall();
        if(err < 0)
          std::cout << "Failed to uninstalled runtime!" << std::endl;
        else
          std::cout << "Successfully uninstalled runtime!" << std::endl;
        return err;
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
    usage();
    return err;
  }

  if(!inputs.size())
  {
    std::cout << "No input files specified!" << std::endl;
    usage();
    return -3;
  }

  if(out.empty()) // If no out is specified, default to name of first input file
    out = innative::Path(inputs[0]).RemoveExtension().Get() + ((flags&ENV_LIBRARY) ? IR_LIBRARY_EXTENSION : IR_EXE_EXTENSION);

  IRExports exports = { 0 };
  if(generate) // If we are generating a loader, we replace all of the normal functions to reroute the resources into the EXE file
  {
    if(run)
    {
      std::cout << "You can't run a file dynamically and also generate a loader, make up your mind!" << std::endl;
      return -8;
    }

#ifdef IR_PLATFORM_WIN32
    exports.CreateEnvironment = [](unsigned int modules, unsigned int maxthreads, const char* arg0) ->Environment* {
      Environment* env = (Environment*)calloc(1, sizeof(Environment));
      env->log = stdout;
      return env;
    };
    exports.AddModule = [](Environment* env, const void* data, uint64_t size, const char* name, int* err) {
      if(!size)
      {
        long sz = 0;
        auto file = LoadFile((const char*)data, sz);
        if(!sz)
          *err = ERR_FATAL_FILE_ERROR;
        else
          UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, file.get(), sz);
      }
      else
        UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, (void*)data, size);
    };
    exports.AddWhitelist = [](Environment* env, const char* module_name, const char* export_name) {
      UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_WHITELIST, module_name, 0, (void*)export_name, strlen(export_name) + 1);
    };
    exports.AddEmbedding = [](Environment* env, int tag, const void* data, uint64_t size)->enum IR_ERROR {
      char buf[20];
      _itoa_s(tag, buf, 10);
      if(!size)
      {
        long sz = 0;
        auto file = LoadFile((const char*)data, sz);
        if(!sz)
          return ERR_FATAL_FILE_ERROR;
        else
          UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, file.get(), sz);
      }
      else
        UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, (void*)data, size);
      return ERR_SUCCESS;
    };
    exports.WaitForLoad = [](Environment* env) {};
    exports.Compile = [](Environment* env, const char* file)->enum IR_ERROR { return ERR_SUCCESS; };
    exports.DestroyEnvironment = [](Environment* env) { EndUpdateResourceA((HANDLE)env->alloc, FALSE); };

    std::ifstream src("loader.exe", std::ios::binary);
    std::ofstream dst(out.c_str(), std::ios::binary);
    dst << src.rdbuf();
#endif
  }
  else
    innative_runtime(&exports);

  // Then create the runtime environment with the module count.
  Environment* env = (*exports.CreateEnvironment)(inputs.size(), 0, (!argc ? 0 : argv[0]));
  env->flags = flags;
  env->features = ENV_FEATURE_ALL;
  env->optimize = (env->flags & ENV_DEBUG) ? 0 : ENV_OPTIMIZE_ALL;

#ifdef IR_PLATFORM_WIN32
  if(generate)
    env->alloc = (__WASM_ALLOCATOR*)BeginUpdateResourceA(out.c_str(), TRUE);
#endif

  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return -2;
  }

  if(verbose)
    env->loglevel = LOG_NOTICE;
  if(sdkpath)
    env->sdkpath = sdkpath;
  if(linker)
    env->linker = linker;

  std::string whitebuf;
  for(auto item : whitelist)
  {
    whitebuf = item; // We have to make a copy of the string because the actual argv string isn't necessarily mutable
    char* ctx;
    char* first = STRTOK((char*)whitebuf.data(), ":", &ctx);
    char* second = STRTOK(NULL, ":", &ctx);

    if(!second)
      (*exports.AddWhitelist)(env, nullptr, first);
    else
      (*exports.AddWhitelist)(env, first, second);
  }

  // Load all modules
  for(size_t i = 0; i < inputs.size(); ++i)
    (*exports.AddModule)(env, inputs[i], 0, innative::Path(inputs[i]).File().RemoveExtension().c_str(), &err);

  if(err < 0)
  {
    if(env->loglevel >= LOG_FATAL)
      fprintf(env->log, "Error loading modules: 0x%x\n", -err);
    return err;
  }

  // Add all embedding environments, plus the default environment
  embeddings.push_back(INNATIVE_DEFAULT_ENVIRONMENT);
  
  for(size_t i = 0; i < embeddings.size(); ++i)
    (*exports.AddEmbedding)(env, 0, embeddings[i], 0);

  if(err < 0)
  {
    if(env->loglevel >= LOG_FATAL)
      fprintf(env->log, "Error loading environment: %i\n", err);
    return err;
  }

  // Ensure all modules are loaded, in case we have multithreading enabled
  (*exports.WaitForLoad)(env);

  // Check if this is a .wast file, which must be handled differently because it's an entire environment
  if(wast.size() > 0)
  {
    for(size_t i = 0; i < wast.size() && !err; ++i)
      err = innative_compile_script((const uint8_t*)wast[i], 0, env, true);
  }
  else // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env, out.c_str());

  if(err < 0)
  {
    if(env->loglevel >= LOG_ERROR)
    {
      fprintf(env->log, "Compile error: %i\n", err);

      for(ValidationError* error = env->errors; error != nullptr; error = error->next)
        fprintf(env->log, "Error %i: %s\n", error->code, error->error);
    }

    getchar();
    return err;
  }

  // Destroy environment now that compilation is complete
  (*exports.DestroyEnvironment)(env);

  // Automatically run the assembly, but only if there were no wast scripts (which are always executed)
  if(run && !wast.size())
  {
    void* assembly = (*exports.LoadAssembly)(out.c_str()); // This automatically calls the start function on load
    if(!assembly)
      return ERR_FATAL_FILE_ERROR;
    IR_Entrypoint start = (*exports.LoadFunction)(assembly, 0, IR_INIT_FUNCTION);
    IR_Entrypoint exit = (*exports.LoadFunction)(assembly, 0, IR_EXIT_FUNCTION);
    if(!start)
      return ERR_INVALID_START_FUNCTION;
    (*start)();
    if(exit)
      (*exit)();
    return ERR_SUCCESS;
  }

  return err;
}