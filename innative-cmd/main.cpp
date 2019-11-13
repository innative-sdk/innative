// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include <iostream>
#include <vector>
#include <fstream>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include "../innative/filesys.h"

#ifdef IN_PLATFORM_WIN32
#include "../innative/win32.h"

inline std::unique_ptr<uint8_t[]> LoadFile(const path& file, long& sz)
{
  FILE* f = nullptr;
  FOPEN(f, file.c_str(), "rb");
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

inline path GetProgramPath()
{
  std::wstring programpath;
  programpath.resize(MAX_PATH);
  programpath.resize(GetModuleFileNameW(NULL, const_cast<wchar_t*>(programpath.data()), (DWORD)programpath.capacity()));
  programpath.resize(wcsrchr(programpath.data(), '\\') - programpath.data());
  return path(programpath);
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
  { "o0", ENV_OPTIMIZE_O0 }, { "o1", ENV_OPTIMIZE_O1 }, { "o2", ENV_OPTIMIZE_O2 },
  { "o3", ENV_OPTIMIZE_O3 }, { "os", ENV_OPTIMIZE_Os }, { "fastmath", ENV_OPTIMIZE_FAST_MATH },
};

void usage()
{
  std::cout
    << "Usage: innative-cmd [-r] [-c] [-i [lite]] [-u] [-v] [-f FLAG...] [-l FILE] [-L FILE] [-o FILE] [-a FILE] [-d PATH] [-j PATH] [-s [FILE]] [-w [MODULE:]FUNCTION] FILE...\n"
       "  -r : Run the compiled result immediately and display output. Requires a start function.\n"
       "  -f <FLAG>: Set a supported flag to true. Flags:\n         ";

  for(auto& f : flag_map)
    std::cout << ((&f == &*flag_map.begin()) ? "" : ", ") << f.first;
  std::cout << "\n         ";
  for(auto& f : optimize_map)
    std::cout << ((&f == &*optimize_map.begin()) ? "" : ", ") << f.first;

  std::cout
    << "\n"
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
       "  -g : Instead of compiling immediately, creates a loader embedded with all the modules, environments, and settings, which compiles the modules on-demand when run.\n"
#endif
       "  -c : Assumes the input files are actually LLVM IR files and compiles them into a single webassembly module.\n"
       "  -i [lite]: Installs this SDK to the host operating system. On Windows, also updates file associations unless 'lite' is specified.\n"
       "  -u : Uninstalls and deregisters this SDK from the host operating system.\n"
       "  -v : Turns on verbose logging.\n"
       "\n"
       "  Example usage: innative-cmd -r your-module.wasm"
    << std::endl;
}

void printerr(IRExports& exports, FILE* f, const char* prefix, const char* postfix, int err)
{
  const char* errstring = (*exports.GetErrorString)(err);
  if(errstring)
    fprintf(f, "%s%s: %s\n", prefix, postfix, errstring);
  else
    fprintf(f, "%s%s: %i\n", prefix, postfix, err);
}

void dump_validation_errors(IRExports& exports, Environment* env)
{
  for(ValidationError* error = env->errors; error != nullptr; error = error->next)
  {
    const char* errstring = (*exports.GetErrorString)(error->code);
    if(errstring)
      fprintf(env->log, "Error %s: %s\n", errstring, error->error);
    else
      fprintf(env->log, "Error %i: %s\n", error->code, error->error);
  }
}

bool checkarg(int i, int argc, char* argv[], int& err)
{
  if(i >= argc)
  {
    std::cout << "Missing command line argument parameter for " << argv[i - 1] << std::endl;
    err = ERR_MISSING_COMMAND_LINE_PARAMETER;
    return false;
  }
  return true;
}

int main(int argc, char* argv[])
{
  std::vector<const char*> inputs;
  std::vector<std::pair<const char*, int>> embeddings;
  std::vector<const char*> whitelist;
  unsigned int flags    = ENV_ENABLE_WAT;  // Always enable WAT
  unsigned int optimize = ENV_OPTIMIZE_O3; // Default to O3
  path out;
  std::vector<const char*> wast; // WAST files will be executed in the order they are specified, after all other modules are
                                 // injected into the environment
  const char* libpath   = nullptr;
  const char* objpath   = nullptr;
  const char* linker    = nullptr;
  const char* serialize = nullptr;
  const char* system    = nullptr;
  bool run              = false;
  bool generate         = false;
  bool verbose          = false;
  bool reverse          = false;
  int err               = ERR_SUCCESS;

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
        case 'f':                            // flag
          ++i;                               // Increment to next position
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
              else if(flag->second == ENV_OPTIMIZE_O0 || (flag->second & ENV_OPTIMIZE_OMASK))
                optimize = (optimize & ~ENV_OPTIMIZE_OMASK) | flag->second;
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
          if(checkarg(++i, argc, argv, err))
            embeddings.push_back({ argv[i], IN_TAG_ANY });
          break;
        case 'L': // shared lib
          if(checkarg(++i, argc, argv, err))
            embeddings.push_back({ argv[i], IN_TAG_DYNAMIC });
          break;
        case 'o': // out
          if(checkarg(++i, argc, argv, err))
            out = u8path(argv[i]);
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
          if(checkarg(++i, argc, argv, err))
            whitelist.push_back(argv[i]);
          break;
        case 'e': // environment/system module
          if(checkarg(++i, argc, argv, err))
            system = argv[i];
          break;
        case 'a': // alternative linker
          if(checkarg(++i, argc, argv, err))
            linker = argv[i];
          break;
        case 'd': // Specify library directory
          if(checkarg(++i, argc, argv, err))
            libpath = argv[i];
          break;
        case 'j': // Specify intermediate directory
          if(checkarg(++i, argc, argv, err))
            objpath = argv[i];
          break;
        case 'i': // install
          std::cout << "Installing inNative Runtime..." << std::endl;
          {
            bool lite = (i + 1 < argc) && !STRICMP(argv[i + 1], "lite");
            err       = innative_install(argv[0], !lite);
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
    else if(!STRICMP(u8path(argv[i]).extension().u8string().c_str(), "wast")) // Check if this is a .wast script
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

  if(out.empty()) // If no out is specified, default to name of first input file
  {
    out = u8path(!inputs.size() ? wast[0] : inputs[0]);

    if(reverse)
      out.replace_extension(".wasm");
    else if(flags & ENV_LIBRARY)
      out.replace_extension(IN_LIBRARY_EXTENSION);
    else
      out.replace_extension(IN_EXE_EXTENSION);
  }

  if(reverse) // If we're compiling LLVM IR instead of webassembly, we divert to another code path
    return innative_compile_llvm(inputs.data(), inputs.size(), flags, out.u8string().c_str(), stdout);

  IRExports exports = { 0 };

  // If we are generating a loader, we replace all of the normal functions to reroute the resources into the EXE file
  if(generate)
  {
    if(run)
    {
      std::cout << "You can't run a file dynamically and also generate a loader, make up your mind!" << std::endl;
      return ERR_COMMAND_LINE_CONFLICT;
    }
#ifdef IN_PLATFORM_WIN32
    exports.CreateEnvironment = [](unsigned int modules, unsigned int maxthreads, const char* arg0) -> Environment* {
      Environment* env = (Environment*)calloc(1, sizeof(Environment));
      if(env)
        env->log = stdout;
      return env;
    };
    exports.AddModule = [](Environment* env, const void* data, uint64_t size, const char* name, int* err) {
      if(!size)
      {
        long sz   = 0;
        auto file = LoadFile((const char*)data, sz);
        if(!sz)
          *err = ERR_FATAL_FILE_ERROR;
        else if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, file.get(), sz))
          *err = ERR_FATAL_RESOURCE_ERROR;
      }
      else if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_MODULE, name, 0, (void*)data, size))
        *err = ERR_FATAL_RESOURCE_ERROR;
    };

    exports.AddWhitelist = [](Environment* env, const char* module_name, const char* export_name) -> IN_ERROR {
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_WHITELIST, module_name, 0, (void*)export_name,
                          strlen(export_name) + 1))
      {
        std::cout << "Failed to add whitelist entry: " << (!module_name ? "" : module_name) << "|" << export_name
                  << std::endl;
        return ERR_FATAL_FILE_ERROR;
      }
      return ERR_SUCCESS;
    };
    exports.AddEmbedding = [](Environment* env, int tag, const void* data, uint64_t size) -> IN_ERROR {
      char buf[20];
      _itoa_s(tag, buf, 10);
      if(!size)
      {
        long sz  = 0;
        path src = u8path((const char*)data);
        FILE* f;
        FOPEN(f, src.c_str(), "rb");

        if(!f)
        {
          src = (!env->libpath ? GetProgramPath() : u8path(env->libpath)) / src;
        }
        else
          fclose(f);

        auto file = LoadFile(src.c_str(), sz);
        if(!sz)
          return ERR_FATAL_FILE_ERROR;
        if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, file.get(), sz))
          return ERR_FATAL_RESOURCE_ERROR;
      }
      else if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_EMBEDDING, buf, 0, (void*)data, size))
        return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS;
    };
    exports.FinalizeEnvironment = [](Environment* env) -> IN_ERROR {
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "flags", 0, &env->flags, sizeof(env->flags)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "optimize", 0, &env->optimize, sizeof(env->optimize)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA((HANDLE)env->alloc, WIN32_RESOURCE_FLAGS, "features", 0, &env->features, sizeof(env->features)))
        return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS;
    };
    exports.Compile            = [](Environment* env, const char* file) -> IN_ERROR { return ERR_SUCCESS; };
    exports.DestroyEnvironment = [](Environment* env) {
      if(!EndUpdateResourceA((HANDLE)env->alloc, FALSE))
        std::cout << "Failed to end resource update!" << std::endl;
    };

#ifdef IN_DEBUG
    std::string exe = "innative-loader-d" IN_EXE_EXTENSION;
#else
    std::string exe = "innative-loader" IN_EXE_EXTENSION;
#endif
    if(!copy_file(GetProgramPath() / exe, out, copy_options::overwrite_existing))
    {
      std::cout << "Could not find or copy loader EXE!" << std::endl;
      return ERR_MISSING_LOADER;
    }
#endif
  }
  else
    innative_runtime(&exports);

  // Make sure the functions we're going to use actually exist
  if(!exports.CreateEnvironment || !exports.DestroyEnvironment || !exports.AddModule || !exports.AddWhitelist ||
     !exports.AddEmbedding || !exports.FinalizeEnvironment || !exports.Compile || !exports.LoadAssembly ||
     !exports.FreeAssembly || !exports.GetErrorString || !exports.CompileScript || !exports.SerializeModule)
    return ERR_UNKNOWN_ENVIRONMENT_ERROR;

  // Then create the runtime environment with the module count.
  Environment* env = (*exports.CreateEnvironment)(inputs.size(), 0, (!argc ? 0 : argv[0]));
  env->flags       = flags;
  env->features    = ENV_FEATURE_ALL;
  env->optimize    = optimize;

  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return ERR_UNKNOWN_ENVIRONMENT_ERROR;
  }

#ifdef IN_PLATFORM_WIN32
  if(generate)
    env->alloc = (IN_WASM_ALLOCATOR*)BeginUpdateResourceA(out.u8string().c_str(), TRUE);
  if(!env->alloc)
  {
    std::cout << "Failed to begin resource update!" << std::endl;
    (*exports.DestroyEnvironment)(env);
    return ERR_MISSING_LOADER;
  }
#endif

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
    char* first  = STRTOK((char*)whitebuf.data(), ":", &ctx);
    char* second = STRTOK(NULL, ":", &ctx);

    if(!second)
      err = (*exports.AddWhitelist)(env, nullptr, first);
    else
      err = (*exports.AddWhitelist)(env, first, second);
    if(err < 0)
      printerr(exports, env->log, "Error adding whitelist ", item, err);
  }

  // Add all embedding environments, plus the default environment
  embeddings.push_back({ INNATIVE_DEFAULT_ENVIRONMENT, 0 });

  for(size_t i = 0; i < embeddings.size(); ++i)
  {
    err = (*exports.AddEmbedding)(env, embeddings[i].second, embeddings[i].first, 0);
    if(err < 0)
    {
      printerr(exports, env->log, "Error loading embedding ", embeddings[i].first, err);
      (*exports.DestroyEnvironment)(env);
      return ERR_FATAL_FILE_ERROR;
    }
  }

  // Load all modules
  std::unique_ptr<int[]> errs(new int[inputs.size()]);
  for(size_t i = 0; i < inputs.size(); ++i)
    (*exports.AddModule)(env, inputs[i], 0, u8path(inputs[i]).stem().u8string().c_str(), &errs[i]);

  // Ensure all modules are loaded, in case we have multithreading enabled
  err = (*exports.FinalizeEnvironment)(env);

  for(size_t i = 0; i < inputs.size(); ++i)
  {
    if(errs[i] < 0)
    {
      printerr(exports, env->log, "Error loading module ", inputs[i], errs[i]);
      if(err >= 0)
        err = ERR_FATAL_INVALID_MODULE;
    }
  }

  if(err < 0)
  {
    if(env->loglevel >= LOG_FATAL)
    {
      printerr(exports, env->log, "Error loading environment", "", err);
      dump_validation_errors(exports, env);
    }

    (*exports.DestroyEnvironment)(env);
    return err;
  }

  if(serialize != nullptr) // If you want to serialize the results, we do so now that the modules have been loaded
  {
    if(serialize[0]) // If a name was specified, verify only one module exists
    {
      if(env->n_modules != 1)
        fprintf(
          stderr,
          "If you have more than one module, you cannot specify an output file for serialization. Use [-s] by itself, instead.\n");
    }

    for(size_t i = 0; i < env->n_modules; ++i)
    {
      std::string target = env->modules[i].name.str();
      target += ".wat";

      if(serialize != nullptr)
        target = serialize;

      (*exports.SerializeModule)(env, i, target.c_str(), 0);
    }
  }

  // Check if this is a .wast file, which must be handled differently because it's an entire environment
  if(wast.size() > 0)
  {
    for(size_t i = 0; i < wast.size() && !err; ++i)
      err = (*exports.CompileScript)((const uint8_t*)wast[i], 0, env, true, out.parent_path().u8string().c_str());
  }
  else // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env, out.u8string().c_str());

  if(err < 0)
  {
    if(env->loglevel >= LOG_ERROR)
    {
      printerr(exports, env->log, "Compile error", "", err);
      dump_validation_errors(exports, env);
    }

    (*exports.DestroyEnvironment)(env);
    return err;
  }

  // Destroy environment now that compilation is complete
  (*exports.DestroyEnvironment)(env);

  // Automatically run the assembly, but only if there were no wast scripts (which are always executed)
  if(!wast.size())
  {
    if(run)
    {
      void* assembly = (*exports.LoadAssembly)(out.u8string().c_str());
      if(!assembly)
      {
        fprintf(env->log, "Generated file cannot be found! Does innative-cmd have permissions for this directory?\n");
        return ERR_FATAL_FILE_ERROR;
      }
      IN_Entrypoint start = (*exports.LoadFunction)(assembly, 0, IN_INIT_FUNCTION);
      IN_Entrypoint exit  = (*exports.LoadFunction)(assembly, 0, IN_EXIT_FUNCTION);
      if(!start)
      {
        fprintf(env->log, "Start function is invalid or cannot be found!\n");
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
      std::cout << "Successfully built " << out.u8string().c_str() << std::endl;
  }

  return err;
}