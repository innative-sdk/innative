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

#ifdef IN_PLATFORM_WIN32
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

std::string GetProgramPath()
{
  std::string programpath;
  programpath.resize(MAX_PATH);
  programpath.resize(GetModuleFileNameA(NULL, const_cast<char*>(programpath.data()), (DWORD)programpath.capacity()));
  programpath.resize(strrchr(programpath.data(), '\\') - programpath.data());
  return programpath;
}

#endif

static const std::unordered_map<std::string, unsigned int> flag_map = {
  { "strict", ENV_STRICT },
  { "sandbox", ENV_SANDBOX },
  { "whitelist", ENV_WHITELIST },
  { "multithreaded", ENV_MULTITHREADED },
  { "debug", ENV_DEBUG },
  { "library", ENV_LIBRARY },
  { "llvm", ENV_EMIT_LLVM },
  { "homogenize", ENV_HOMOGENIZE_FUNCTIONS },
  { "noinit", ENV_NO_INIT },
  { "check_stack_overflow", ENV_CHECK_STACK_OVERFLOW },
  { "check_float_trunc", ENV_CHECK_FLOAT_TRUNC },
  { "check_memory_access", ENV_CHECK_MEMORY_ACCESS },
  { "check_indirect_call", ENV_CHECK_INDIRECT_CALL },
  { "check_int_division", ENV_CHECK_INT_DIVISION },
  { "disable_tail_call", ENV_DISABLE_TAIL_CALL },
};

static const std::unordered_map<std::string, unsigned int> optimize_map = {
  { "o0", ENV_OPTIMIZE_O0 },
  { "o1", ENV_OPTIMIZE_O1 },
  { "o2", ENV_OPTIMIZE_O2 },
  { "o3", ENV_OPTIMIZE_O3 },
  { "os", ENV_OPTIMIZE_Os },
  { "fastmath", ENV_OPTIMIZE_FAST_MATH },
};

void usage()
{
  std::cout << "Usage: innative-cmd [-r] [-c] [-i [lite]] [-u] [-v] [-f FLAG...] [-l FILE] [-L FILE] [-o FILE] [-a FILE] [-d PATH] [-j PATH] [-s [FILE]] [-w [MODULE:]FUNCTION] FILE...\n"
    "  -r : Run the compiled result immediately and display output. Requires a start function.\n"
    "  -f <FLAG>: Set a supported flag to true. Flags:\n         ";

  for(auto& f : flag_map)
    std::cout << ((&f == &*flag_map.begin()) ? "" : ", ") << f.first;
  std::cout << "\n         ";
  for(auto& f : optimize_map)
    std::cout << ((&f == &*optimize_map.begin()) ? "" : ", ") << f.first;

  std::cout << "\n"
    "  -l <FILE> : Links the input files against <FILE>, which must be a static library.\n"
    "  -L <FILE> : Links the input files against <FILE>, which must be an ELF shared library.\n"
    "  -o <FILE> : Sets the output path for the resulting executable or library.\n"
    "  -a <FILE> : Specifies an alternative linker to use instead of LLD.\n"
    "  -d <PATH> : Sets the directory that contains the SDK library and data files.\n"
    "  -j <PATH> : Sets the directory for temporary object files and intermediate compilation results.\n"
    "  -e <MODULE> : Sets the environment/system module name. Any functions with the module name will have the module name stripped when linking with C functions.\n"
    "  -s [<FILE>] : Serializes all modules to .wat files. <FILE> can specify the output if only one module is present.\n"
    "  -w <[MODULE:]FUNCTION> : whitelists a given C import, does name-mangling if the module is specified.\n"
#ifdef IN_PLATFORM_WIN32
    "  -g : Instead of compiling immediately, creates a loader embedded with all the modules, environments, and settings, which compiles the modules on-demand when run."
#endif
    "  -c : Assumes the input files are actually LLVM IR files and compiles them into a single webassembly module.\n"
    "  -i [lite]: Installs this SDK to the host operating system. On Windows, also updates file associations unless 'lite' is specified.\n"
    "  -u : Uninstalls and deregisters this SDK from the host operating system.\n"
    "  -v : Turns on verbose logging."
    << std::endl;
}

void printerr(FILE * f, const char* prefix, int err)
{
  const char* errstring = innative_error_string(err);
  if(errstring)
    fprintf(f, "%s: %s\n", prefix, errstring);
  else
    fprintf(f, "%s: %i\n", prefix, err);
}

void dump_validation_errors(Environment* env)
{
  for(ValidationError* error = env->errors; error != nullptr; error = error->next)
  {
    const char* errstring = innative_error_string(error->code);
    if(errstring)
      fprintf(env->log, "Error %s: %s\n", errstring, error->error);
    else
      fprintf(env->log, "Error %i: %s\n", error->code, error->error);
  }
}

template<typename F>
void checkarg(int i, int argc, char* argv[], int& err, F && f)
{
  if(i >= argc)
  {
    std::cout << "Missing command line argument parameter for " << argv[i - 1] << std::endl;
    err = ERR_MISSING_COMMAND_LINE_PARAMETER;
  }
  else
    f();
}

int main(int argc, char* argv[])
{
  std::vector<const char*> inputs;
  std::vector<std::pair<const char*, int>> embeddings;
  std::vector<const char*> whitelist;
  unsigned int flags = ENV_ENABLE_WAT; // Always enable WAT
  unsigned int optimize = 0;
  innative::Path out;
  std::vector<const char*> wast; // WAST files will be executed in the order they are specified, after all other modules are injected into the environment
  const char* libpath = nullptr;
  const char* objpath = nullptr;
  const char* linker = nullptr;
  const char* serialize = nullptr;
  const char* system = nullptr;
  bool run = false;
  bool generate = false;
  bool verbose = false;
  bool reverse = false;
  int err = ERR_SUCCESS;

  for(int i = 1; i < argc; ++i) // skip first argument, which is the program path
  {
    if(argv[i][0] == '-')
    {
      if(argv[i][2] != 0)
      {
        std::cout << "Unknown command line option: " << argv[i] << std::endl;
        err = ERR_UNKNOWN_COMMAND_LINE;
      }
      else
      {
        switch(argv[i][1])
        {
        case 'r': // run immediately
          run = true;
          flags |= ENV_LIBRARY | ENV_NO_INIT;
          break;
        case 'f': // flag
          ++i; // Increment to next position
          if(i >= argc || argv[i][0] == '-') // If this isn't valid, there were no valid flags, so log an error
          {
            std::cout << "Missing command line argument parameter for " << argv[--i] << std::endl;
            err = ERR_MISSING_COMMAND_LINE_PARAMETER;
            break;
          }

          do
          {
            auto raw = std::string(argv[i]);
            std::transform(raw.begin(), raw.end(), raw.begin(), ::tolower);
            auto flag = flag_map.find(raw);

            if(flag == flag_map.end())
            {
              flag = optimize_map.find(raw);

              if(flag == optimize_map.end())
              {
                std::cout << "Unknown flag: " << argv[i] << std::endl;
                err = ERR_UNKNOWN_FLAG;
              }
              else
                optimize |= flag->second;
            }
            else
              flags |= flag->second;

            ++i; // Increment and check if we've either hit the end or hit another parameter
          } while(i < argc && argv[i][0] != '-');

          --i; // We must decrement after we hit another parameter so it gets processed correctly
          break;
        case 'l': // lib
          checkarg(++i, argc, argv, err, [&]() { embeddings.push_back({ argv[i], IN_TAG_ANY }); });
          break;
        case 'L': // shared lib
          checkarg(++i, argc, argv, err, [&]() { embeddings.push_back({ argv[i], IN_TAG_DYNAMIC }); });
          break;
        case 'o': // out
          checkarg(++i, argc, argv, err, [&]() { out = argv[i]; });
          break;
        case 's': // serialize
          serialize = (i + 1 < argc && argv[i + 1][0] != '-') ? argv[++i] : "";
          break;
#ifdef IN_PLATFORM_WIN32
        case 'g': // generate loader
          generate = true;
          break;
#endif
        case 'v': // verbose logging
          verbose = true;
          break;
        case 'w': // whitelist
          checkarg(++i, argc, argv, err, [&]() { whitelist.push_back(argv[i]); });
          break;
        case 'e': // environment/system module
          checkarg(++i, argc, argv, err, [&]() { system = argv[i]; });
          break;
        case 'a': // alternative linker
          checkarg(++i, argc, argv, err, [&]() { linker = argv[i]; });
          break;
        case 'd': // Specify library directory
          checkarg(++i, argc, argv, err, [&]() { libpath = argv[i]; });
          break;
        case 'j': // Specify intermediate directory
          checkarg(++i, argc, argv, err, [&]() { objpath = argv[i]; });
          break;
        case 'i': // install
          std::cout << "Installing inNative Runtime..." << std::endl;
		  {
			  bool lite = (i + 1 < argc) && !STRICMP(argv[i + 1], "lite");
			  err = innative_install(argv[0], !lite);
			  if(lite) // Only consume this argument if it was actually processed
				  ++i;
		  }
          if(err < 0)
            std::cout << "Installation failed! [" << err << "]" << std::endl;
          else
            std::cout << "Installation succeeded!" << std::endl;
          return err;
        case 'u': // uninstall
          std::cout << "Uninstalling inNative Runtime..." << std::endl;
          err = innative_uninstall();
          if(err < 0)
            std::cout << "Failed to uninstall runtime! [" << err << "]" << std::endl;
          else
            std::cout << "Successfully uninstalled runtime!" << std::endl;
          return err;
        case 'c': // Compile LLVM IR into webassembly
          reverse = true;
          break;
        default:
          std::cout << "Unknown command line option: " << argv[i] << std::endl;
          err = ERR_UNKNOWN_COMMAND_LINE;
          break;
        }
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

  if(!inputs.size() && !wast.size())
  {
    std::cout << "No input files specified!" << std::endl;
    usage();
    return ERR_NO_INPUT_FILES;
  }

  if(out.Get().empty()) // If no out is specified, default to name of first input file
  {
    out = !inputs.size() ? wast[0] : inputs[0];
    out = out.RemoveExtension();
    if(reverse)
      out.AppendString(".wasm");
    else if(flags & ENV_LIBRARY)
      out.AppendString(IN_LIBRARY_EXTENSION);
    else
      out.AppendString(IN_EXE_EXTENSION);
  }

  if(reverse) // If we're compiling LLVM IR instead of webassembly, we divert to another code path
    return innative_compile_llvm(inputs.data(), inputs.size(), flags, out.c_str(), stdout);

  IRExports exports = { 0 };
  if(generate) // If we are generating a loader, we replace all of the normal functions to reroute the resources into the EXE file
  {
    if(run)
    {
      std::cout << "You can't run a file dynamically and also generate a loader, make up your mind!" << std::endl;
      return ERR_COMMAND_LINE_CONFLICT;
    }

#ifdef IN_PLATFORM_WIN32
    exports.CreateEnvironment = [](unsigned int modules, unsigned int maxthreads, const char* arg0) ->Environment * {
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
          if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, file.get(), sz))
            *err = ERR_FATAL_RESOURCE_ERROR;
      }
      else
        if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, (void*)data, size))
          *err = ERR_FATAL_RESOURCE_ERROR;
    };
    exports.AddWhitelist = [](Environment* env, const char* module_name, const char* export_name) {
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_WHITELIST, module_name, 0, (void*)export_name, strlen(export_name) + 1))
        std::cout << "Failed to add whitelist entry: " << (!module_name ? "" : module_name) << "|" << export_name << std::endl;
    };
    exports.AddEmbedding = [](Environment* env, int tag, const void* data, uint64_t size)->enum IN_ERROR {
      char buf[20];
      _itoa_s(tag, buf, 10);
      if(!size)
      {
        long sz = 0;
        innative::Path path((const char*)data);
        FILE* f;
        FOPEN(f, path.c_str(), "rb");
        if(!f)
        {
          path = innative::Path(!env->libpath ? GetProgramPath().c_str() : env->libpath) + path;
        }
        else
          fclose(f);

        auto file = LoadFile(path.c_str(), sz);
        if(!sz)
          return ERR_FATAL_FILE_ERROR;
        if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, file.get(), sz))
          return ERR_FATAL_RESOURCE_ERROR;
      }
      else
        if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, (void*)data, size))
          return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS;
    };
    exports.FinalizeEnvironment = [](Environment* env) -> enum IN_ERROR { 
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "flags", 0, &env->flags, sizeof(env->flags)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "optimize", 0, &env->optimize, sizeof(env->optimize)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "features", 0, &env->features, sizeof(env->features)))
        return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS; 
    };
    exports.Compile = [](Environment* env, const char* file)->enum IN_ERROR { return ERR_SUCCESS; };
    exports.DestroyEnvironment = [](Environment * env) { if(!EndUpdateResourceA((HANDLE)env->alloc, FALSE)) std::cout << "Failed to end resource update!" << std::endl; };

#ifdef IN_DEBUG
    std::string exe = "innative-loader-d" IN_EXE_EXTENSION;
#else
    std::string exe = "innative-loader" IN_EXE_EXTENSION;
#endif
    if(!CopyFileA((innative::Path(GetProgramPath()) + exe).c_str(), out.c_str(), FALSE))
    {
      std::cout << "Could not find or copy loader EXE!" << std::endl;
      return ERR_MISSING_LOADER;
    }
#endif
  }
  else
    innative_runtime(&exports);

  // Then create the runtime environment with the module count.
  Environment* env = (*exports.CreateEnvironment)(inputs.size(), 0, (!argc ? 0 : argv[0]));
  env->flags = flags;
  env->features = ENV_FEATURE_ALL;
  env->optimize = optimize;

#ifdef IN_PLATFORM_WIN32
  if(generate)
    env->alloc = (IN_WASM_ALLOCATOR*)BeginUpdateResourceA(out.c_str(), TRUE);
  if(!env->alloc)
  {
    std::cout << "Failed to begin resource update!" << std::endl;
    return ERR_MISSING_LOADER;
  }
#endif

  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return ERR_UNKNOWN_ENVIRONMENT_ERROR;
  }

  if(verbose)
    env->loglevel = LOG_NOTICE;
  if(libpath)
    env->libpath = libpath;
  if(objpath)
    env->objpath = objpath;
  if(linker)
    env->linker = linker;
  if(system)
    env->system = system;

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
    {
      printerr(env->log, "Error loading modules", err);
      dump_validation_errors(env);
    }
    return err;
  }

  // Add all embedding environments, plus the default environment
  embeddings.push_back({ INNATIVE_DEFAULT_ENVIRONMENT, 0 });

  for(size_t i = 0; i < embeddings.size(); ++i)
    if((*exports.AddEmbedding)(env, embeddings[i].second, embeddings[i].first, 0) == ERR_FATAL_FILE_ERROR)
    {
      fprintf(env->log, "Error loading file: %s\n", embeddings[i].first);
      return ERR_FATAL_FILE_ERROR;
    }

  // Ensure all modules are loaded, in case we have multithreading enabled
  if(err >= 0)
    err = (*exports.FinalizeEnvironment)(env);

  if(err < 0)
  {
    if(env->loglevel >= LOG_FATAL)
    {
      printerr(env->log, "Error loading environment", err);
      dump_validation_errors(env);
    }
    return err;
  }

  if(serialize != nullptr) // If you want to serialize the results, we do so now that the modules have been loaded
  {
    if(serialize[0]) // If a name was specified, verify only one module exists
    {
      if(env->n_modules != 1)
        fprintf(stderr, "If you have more than one module, you cannot specify an output file for serialization. Use [-s] by itself, instead.\n");
    }

    for(size_t i = 0; i < env->n_modules; ++i)
    {
      std::string target = env->modules[i].name.str();
      target += ".wat";

      if(serialize != nullptr)
        target = serialize;

      innative_serialize_module(env, i, target.c_str());
    }
  }

  // Check if this is a .wast file, which must be handled differently because it's an entire environment
  if(wast.size() > 0)
  {
    for(size_t i = 0; i < wast.size() && !err; ++i)
      err = innative_compile_script((const uint8_t*)wast[i], 0, env, true, out.BaseDir().c_str());
  }
  else // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env, out.c_str());

  if(err < 0)
  {
    if(env->loglevel >= LOG_ERROR)
    {
      printerr(env->log, "Compile error", err);
      dump_validation_errors(env);
    }

    return err;
  }

  // Destroy environment now that compilation is complete
  (*exports.DestroyEnvironment)(env);

  // Automatically run the assembly, but only if there were no wast scripts (which are always executed)
  if(!wast.size())
  {
    if(run)
    {
      void* assembly = (*exports.LoadAssembly)(out.c_str());
      if(!assembly)
        return ERR_FATAL_FILE_ERROR;
      IN_Entrypoint start = (*exports.LoadFunction)(assembly, 0, IN_INIT_FUNCTION);
      IN_Entrypoint exit = (*exports.LoadFunction)(assembly, 0, IN_EXIT_FUNCTION);
      if(!start)
      {
        (*exports.FreeAssembly)(assembly);
        return ERR_INVALID_START_FUNCTION;
      }

      (*start)();
      if(exit)
        (*exit)();

      (*exports.FreeAssembly)(assembly);
      return ERR_SUCCESS;
    }
    else
      std::cout << "Successfully built " << out.Get() << std::endl;
  }

  return err;
}