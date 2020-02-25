// Copyright (c)2020 Black Sphere Studios
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

KHASH_INIT(flags, const char*, unsigned int, 1, kh_str_hash_funcins, kh_str_hash_insequal);

static kh_flags_t* env_flags    = kh_init_flags();
static kh_flags_t* env_optimize = kh_init_flags();

const static std::initializer_list<std::pair<const char*, unsigned int>> FLAG_MAP = {
  { "strict", ENV_STRICT },
  { "sandbox", ENV_SANDBOX },
  { "whitelist", ENV_WHITELIST },
  { "multithreaded", ENV_MULTITHREADED },
  { "debug", ENV_DEBUG },
  { "debug_pdb", ENV_DEBUG_PDB },
  { "debug_dwarf", ENV_DEBUG_DWARF },
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

const static std::initializer_list<std::pair<const char*, unsigned int>> OPTIMIZE_MAP = {
  { "o0", ENV_OPTIMIZE_O0 }, { "o1", ENV_OPTIMIZE_O1 }, { "o2", ENV_OPTIMIZE_O2 },
  { "o3", ENV_OPTIMIZE_O3 }, { "os", ENV_OPTIMIZE_Os }, { "fastmath", ENV_OPTIMIZE_FAST_MATH },
};

struct OptBase
{
  OptBase(const char* desc, const char* param) : description(desc), parameter(param) {}
  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) = 0;
  virtual std::string Param() { return !parameter ? "" : parameter; }

  const char* description;
  const char* parameter;
};

template<class T> struct Opt;
template<> struct Opt<bool> : OptBase
{
  Opt(const char* desc, const char* param = 0) : OptBase(desc, param), value(false) {}
  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) override
  {
    value = true;
    return ERR_SUCCESS;
  }
  bool value;
};

template<class T, void (*ASSIGN)(T&, const char*)> struct OptValue : OptBase
{
  OptValue(const char* desc, const char* param = 0) : OptBase(desc, param) {}
  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) override
  {
    if(pos < argc && argv[pos][0] != '-')
      ASSIGN(value, argv[pos++]);
    else
    {
      std::cout << "Missing command line argument parameter for " << argv[pos - 1] << std::endl;
      return ERR_MISSING_COMMAND_LINE_PARAMETER;
    }
    return ERR_SUCCESS;
  }
  T value;
};

inline void STRING_ASSIGN(std::string& v, const char* s) { v = s; }

template<> struct Opt<std::string> : OptValue<std::string, &STRING_ASSIGN>
{
  Opt(const char* desc, const char* param = 0) : OptValue(desc, param) {}
};

inline void PATH_ASSIGN(path& v, const char* s) { v = u8path(s); }

template<> struct Opt<path> : OptValue<path, &PATH_ASSIGN>
{
  Opt(const char* desc, const char* param = 0) : OptValue(desc, param) {}
};

template<class T> struct optional // fake optional class so we don't need to rely on C++17 std::optional
{};
template<class T> struct Opt<optional<T>> : Opt<T>
{
  Opt(const char* desc, const char* param = 0) : Opt<T>(desc, param), set(false), has_value(false) {}
  virtual std::string Param() override { return "[" + Opt<T>::Param() + "]"; }
  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) override
  {
    set = true;
    if(pos < argc && argv[pos][0] != '-')
    {
      IN_ERROR err = Opt<T>::Parse(argc, argv, pos);
      if(err < 0)
        return err;
      has_value = true;
    }
    return ERR_SUCCESS;
  }

  bool set;
  bool has_value;
};

template<class T> struct Opt<std::vector<T>> : Opt<T>
{
  Opt(const char* desc, const char* param = 0) : Opt<T>(desc, param) {}
  virtual std::string Param() override { return Opt<T>::Param() + " ... "; }
  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) override
  {
    IN_ERROR err = ERR_SUCCESS;
    if(pos >= argc || argv[pos][0] == '-')
    {
      std::cout << "Missing command line argument parameter for " << argv[pos - 1] << std::endl;
      return ERR_MISSING_COMMAND_LINE_PARAMETER;
    }

    while(pos < argc && argv[pos][0] != '-')
    {
      err = std::min(Opt<T>::Parse(argc, argv, pos), err);
      ++pos;
      values.push_back(Opt<T>::value);
    }
    return err;
  }
  std::vector<T> values;
};

template<> struct Opt<WASM_ENVIRONMENT_FLAGS> : OptBase
{
  Opt(const char* desc, const char* param = 0) : OptBase(desc, param)
  {
    help = description;
    help += " Flags:\n    ";

    for(auto& f : FLAG_MAP)
    {
      help += f.first;
      help += "\n    ";
    }
    for(auto& f : OPTIMIZE_MAP)
    {
      help += f.first;
      help += "\n    ";
    }
    description = help.c_str();
  }

  virtual IN_ERROR Parse(int argc, char* argv[], int& pos) override
  {
    if(pos >= argc || argv[pos][0] == '-')
    {
      std::cout << "Missing command line argument parameter for " << argv[pos - 1] << std::endl;
      return ERR_MISSING_COMMAND_LINE_PARAMETER;
    }

    IN_ERROR err = ERR_SUCCESS;
    while(pos < argc && argv[pos][0] != '-')
    {
      khint_t iter = kh_get_flags(env_flags, argv[pos]);

      if(kh_exist2(env_flags, iter))
        value |= kh_value(env_flags, iter);
      else
      {
        iter = kh_get_flags(env_optimize, argv[pos]);

        if(kh_exist2(env_optimize, iter))
        {
          auto v = kh_value(env_optimize, iter);
          if(v == ENV_OPTIMIZE_O0 || (v & ENV_OPTIMIZE_OMASK))
            optimize = (optimize & ~ENV_OPTIMIZE_OMASK) | v;
          else
            optimize |= v;
        }
        else
        {
          std::cout << "Unknown flag: " << argv[pos] << std::endl;
          err = ERR_UNKNOWN_FLAG;
        }
      }
      ++pos;
    }

    return err;
  }

  unsigned int value;
  unsigned int optimize;
  std::string help;
};

KHASH_INIT(commands, const char*, OptBase*, 1, kh_str_hash_funcins, kh_str_hash_insequal);

struct CommandLine
{
  CommandLine() :
    commands(kh_init_commands()),
    shortusage("Usage: innative-cmd"),
    last_cmd(nullptr),
    run("Run the compiled result immediately and display output. Requires a start function."),
    flags("Set a supported flag to true.", "<FLAG>"),
    libs("Links the input files against <FILE>, which must be a static library.", "<FILE>"),
    shared_libs("Links the input files against <FILE>, which must be an ELF shared library.", "<FILE>"),
    output_file("Sets the output path for the resulting executable or library.", "<FILE>"),
    serialize(
      "Serializes all modules to .wat files in addition to compiling them. <FILE> can specify the output if only one module is present, or 'emitdebug' will emit debug information",
      "<FILE>|emitdebug"),
    generate_loader(
      "Instead of compiling immediately, creates a loader embedded with all the modules, environments, and settings, which compiles the modules on-demand when run."),
    verbose("Turns on verbose logging."),
    build_sourcemap(
      "Assumes input files are ELF object files or binaries that contain DWARF debugging information, and creates a source map from them."),
    whitelist("whitelists a given C import, does name-mangling if the module is specified.", "<[MODULE:]FUNCTION>"),
    system(
      "Sets the environment/system module name. Any functions with the module name will have the module name stripped when linking with C functions",
      "<MODULE>"),
    linker("Specifies an alternative linker executable to use instead of LLD."),
    install(
      "Installs this SDK to the host operating system. On Windows, also updates file associations unless 'lite' is specified.",
      "lite"),
    uninstall("Uninstalls and deregisters this SDK from the host operating system."),
    library_dir("Sets the directory that contains the SDK library and data files.", "<DIR>"),
    object_dir("Sets the directory for temporary object files and intermediate compilation results.", "<DIR>"),
    compile_llvm("Assumes the input files are LLVM IR files and compiles them into a single webassembly module.")
  {
    flags.value    = ENV_ENABLE_WAT;
    flags.optimize = ENV_OPTIMIZE_O3;

    Register("r", &run);
    Register("run", &run);
    Register("f", &flags);
    Register("flag", &flags);
    Register("flags", &flags);
    Register("l", &libs);
    Register("lib", &libs);
    Register("libs", &libs);
    Register("library", &libs);
    Register("shared-lib", &shared_libs);
    Register("shared-libs", &shared_libs);
    Register("shared-library", &shared_libs);
    Register("o", &output_file);
    Register("out", &output_file);
    Register("output", &output_file);
    Register("serialize", &serialize);
#ifdef IN_PLATFORM_WIN32
    Register("generate-loader", &generate_loader);
#endif
    Register("v", &verbose);
    Register("verbose", &verbose);
    Register("build-sourcemap", &build_sourcemap);
    Register("w", &whitelist);
    Register("whitelist", &whitelist);
    Register("sys", &system);
    Register("system", &system);
    Register("linker", &linker);
    Register("i", &install);
    Register("install", &install);
    Register("u", &uninstall);
    Register("uninstall", &uninstall);
    Register("sdk", &library_dir);
    Register("library-dir", &library_dir);
    Register("obj", &object_dir);
    Register("obj-dir", &object_dir);
    Register("object-dir", &object_dir);
    Register("intermediate-dir", &object_dir);
    Register("compile-llvm", &compile_llvm);

    usage += "\n\n  Example usage: innative-cmd -r your-module.wasm";
  }

  void Register(const char* shortcut, OptBase* command)
  {
    int err;
    khiter_t iter            = kh_put_commands(commands, shortcut, &err);
    kh_value(commands, iter) = command;

    if(last_cmd != command)
    {
      if(last_cmd)
      {
        if(last_cmd->parameter)
          usage += " " + last_cmd->Param();
        usage += ": ";
        usage += last_cmd->description;
      }

      last_cmd = command;
      shortusage += " [-";
      shortusage += shortcut;
      if(command->parameter)
        shortusage += " " + command->Param();
      shortusage += ']';
      usage += "\n ";
    }

    usage += " -";
    usage += shortcut;
  }

  IN_ERROR Parse(int argc, char* argv[])
  {
    IN_ERROR err = ERR_SUCCESS;
    int pos      = 0;
    while(pos < argc)
    {
      if(argv[pos][0] == '-')
      {
        khiter_t iter = kh_get_commands(commands, argv[pos] + 1);
        if(kh_exist2(commands, iter))
          err = std::min(kh_value(commands, iter)->Parse(argc, argv, ++pos), err);
        else
        {
          std::cout << "Unknown command line option: " << argv[pos++] << std::endl;
          err = ERR_UNKNOWN_COMMAND_LINE;
        }
      }
      else if(!STRICMP(u8path(argv[pos]).extension().u8string().c_str(), "wast")) // Check if this is a .wast script
        wast.push_back(argv[pos++]);
      else // Everything else is an input file
        inputs.push_back(argv[pos++]);
    }
    return err;
  }

  void ResolveOutput()
  {
    if(output_file.value.empty()) // If no out is specified, default to name of first input file
    {
      output_file.value = u8path(!inputs.size() ? wast[0] : inputs[0]);

      if(compile_llvm.value)
        output_file.value.replace_extension(".wasm");
      else if(build_sourcemap.value)
        output_file.value += ".map";
      else if(flags.value & ENV_LIBRARY)
        output_file.value.replace_extension(IN_LIBRARY_EXTENSION);
      else
        output_file.value.replace_extension(IN_EXE_EXTENSION);
    }
  }

  Opt<bool> run;
  Opt<WASM_ENVIRONMENT_FLAGS> flags;
  Opt<std::vector<std::string>> libs;
  Opt<std::vector<std::string>> shared_libs;
  Opt<path> output_file;
  Opt<optional<std::string>> serialize;
  Opt<bool> generate_loader;
  Opt<bool> verbose;
  Opt<bool> build_sourcemap;
  Opt<std::vector<std::string>> whitelist;
  Opt<std::string> system;
  Opt<std::string> linker;
  Opt<optional<std::string>> install;
  Opt<bool> uninstall;
  Opt<std::string> library_dir;
  Opt<std::string> object_dir;
  Opt<bool> compile_llvm;

  std::vector<const char*> inputs;
  std::vector<const char*> wast; // WAST files will be executed in the order they are specified, after all other modules are
                                 // injected into the environment
  kh_commands_s* commands;
  std::string shortusage;
  std::string usage;
  OptBase* last_cmd;
};

void printerr(INExports& exports, FILE* f, const char* prefix, const char* postfix, int err)
{
  const char* errstring = (*exports.GetErrorString)(err);
  if(errstring)
    fprintf(f, "%s%s: %s\n", prefix, postfix, errstring);
  else
    fprintf(f, "%s%s: %i\n", prefix, postfix, err);
}

void dump_validation_errors(INExports& exports, Environment* env)
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

int main(int argc, char* argv[])
{
  int err;
  for(auto& f : FLAG_MAP)
  {
    khiter_t iter             = kh_put_flags(env_flags, f.first, &err);
    kh_value(env_flags, iter) = f.second;
  }
  for(auto& f : OPTIMIZE_MAP)
  {
    khiter_t iter                  = kh_put_flags(::env_optimize, f.first, &err);
    kh_value(::env_optimize, iter) = f.second;
  }

  CommandLine commandline;
  err = commandline.Parse(argc - 1, argv + 1); // skip 0th parameter
  if(err < 0)
  {
    std::cout << commandline.shortusage << commandline.usage << std::endl;
    return err;
  }

  if(commandline.run.value)
    commandline.flags.value |= ENV_LIBRARY | ENV_NO_INIT;

  if(commandline.uninstall.value)
  {
    std::cout << "Uninstalling inNative Runtime..." << std::endl;
    err = innative_uninstall();
    if(err < 0)
    {
      std::cout << "Failed to uninstall runtime! [" << err << "]" << std::endl;
      return err;
    }
    std::cout << "Successfully uninstalled runtime!" << std::endl;
  }

  if(commandline.install.set)
  {
    std::cout << "Installing inNative Runtime..." << std::endl;
    bool lite = commandline.install.has_value ? !STRICMP(commandline.install.value.c_str(), "lite") : false;
    err       = innative_install(argv[0], !lite);
    if(err < 0)
      std::cout << "Installation failed! [" << err << "]" << std::endl;
    else
      std::cout << "Installation succeeded!" << std::endl;
  }

  if(commandline.install.set || commandline.uninstall.value)
    return err;

  if(!commandline.inputs.size() && !commandline.wast.size())
  {
    std::cout << "No input files specified!" << std::endl;
    std::cout << commandline.shortusage << commandline.usage << std::endl;
    return ERR_NO_INPUT_FILES;
  }

  commandline.ResolveOutput();

  // If we're compiling LLVM IR instead of webassembly, we divert to another code path
  if(commandline.compile_llvm.value)
    return innative_compile_llvm(commandline.inputs.data(), commandline.inputs.size(), commandline.flags.value,
                                 commandline.output_file.value.u8string().c_str(), stdout);

  INExports exports = { 0 };

  // If we are generating a loader, we replace all of the normal functions to reroute the resources into the EXE file
  if(commandline.generate_loader.value)
  {
    if(commandline.run.value)
    {
      std::cout << "You can't run a file dynamically and also generate a loader, make up your mind!" << std::endl;
      return ERR_COMMAND_LINE_CONFLICT;
    }
#ifdef IN_PLATFORM_WIN32
    exports.CreateEnvironment = [](unsigned int modules, unsigned int maxthreads, const char* arg0) -> Environment* {
      Environment* env = reinterpret_cast<Environment*>(calloc(1, sizeof(Environment)));
      if(env)
        env->log = stdout;
      return env;
    };
    exports.AddModule = [](Environment* env, const void* data, size_t size, const char* name, int* err) {
      if(!size)
      {
        long sz   = 0;
        auto file = LoadFile(reinterpret_cast<const char*>(data), sz);
        if(!sz)
          *err = ERR_FATAL_FILE_ERROR;
        else if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_MODULE, name, 0, file.get(), static_cast<DWORD>(sz)))
          *err = ERR_FATAL_RESOURCE_ERROR;
      }
      else if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_MODULE, name, 0, const_cast<void*>(data), static_cast<DWORD>(size)))
        *err = ERR_FATAL_RESOURCE_ERROR;
    };

    exports.AddWhitelist = [](Environment* env, const char* module_name, const char* export_name) -> IN_ERROR {
      if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_WHITELIST, module_name, 0, const_cast<char*>(export_name),
                          static_cast<DWORD>(strlen(export_name) + 1)))
      {
        std::cout << "Failed to add whitelist entry: " << (!module_name ? "" : module_name) << "|" << export_name
                  << std::endl;
        return ERR_FATAL_FILE_ERROR;
      }
      return ERR_SUCCESS;
    };
    exports.AddEmbedding = [](Environment* env, int tag, const void* data, size_t size,
                              const char* name_override) -> IN_ERROR {
      char buf[20];
      _itoa_s(tag, buf, 10);
      if(!size)
      {
        long sz  = 0;
        path src = u8path(reinterpret_cast<const char*>(data));
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
        if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_EMBEDDING, buf, 0, file.get(), sz))
          return ERR_FATAL_RESOURCE_ERROR;
      }
      else if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_EMBEDDING, buf, 0, const_cast<void*>(data), static_cast<DWORD>(size)))
        return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS;
    };
    exports.FinalizeEnvironment = [](Environment* env) -> IN_ERROR {
      if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_FLAGS, "flags", 0, &env->flags, sizeof(env->flags)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_FLAGS, "optimize", 0, &env->optimize, sizeof(env->optimize)))
        return ERR_FATAL_RESOURCE_ERROR;
      if(!UpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), WIN32_RESOURCE_FLAGS, "features", 0, &env->features, sizeof(env->features)))
        return ERR_FATAL_RESOURCE_ERROR;
      return ERR_SUCCESS;
    };
    exports.Compile            = [](Environment* env, const char* file) -> IN_ERROR { return ERR_SUCCESS; };
    exports.DestroyEnvironment = [](Environment* env) {
      if(!EndUpdateResourceA(reinterpret_cast<HANDLE>(env->alloc), FALSE))
        std::cout << "Failed to end resource update!" << std::endl;
    };

  #ifdef IN_DEBUG
    std::string exe = "innative-loader-d" IN_EXE_EXTENSION;
  #else
    std::string exe = "innative-loader" IN_EXE_EXTENSION;
  #endif
    if(!copy_file(GetProgramPath() / exe, commandline.output_file.value, copy_options::overwrite_existing))
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
  Environment* env = (*exports.CreateEnvironment)(static_cast<unsigned int>(commandline.inputs.size()), 0, (!argc ? 0 : argv[0]));
  env->flags       = commandline.flags.value;
  env->features    = ENV_FEATURE_ALL;
  env->optimize    = commandline.flags.optimize;

  if(!env)
  {
    fprintf(stderr, "Unknown error creating environment.\n");
    return ERR_UNKNOWN_ENVIRONMENT_ERROR;
  }

  // If we're dumping DWARF to source map information, divert to another code path
  if(commandline.build_sourcemap.value)
  {
    SourceMap map = { 0 };
    for(auto& input : commandline.inputs)
    {
      err = ParseDWARF(env, &map, commandline.inputs[0], 0);
      if(err < 0)
        printerr(exports, stderr, "Error parsing file ", commandline.inputs[0], err);
    }
    err = (*exports.SerializeSourceMap)(&map, commandline.output_file.value.u8string().c_str());
    if(err < 0)
      printerr(exports, stderr, "Error saving file ", commandline.output_file.value.u8string().c_str(), err);
    return err;
  }

#ifdef IN_PLATFORM_WIN32
  if(commandline.generate_loader.value)
    env->alloc = reinterpret_cast<IN_WASM_ALLOCATOR*>(BeginUpdateResourceA(commandline.output_file.value.u8string().c_str(), TRUE));
  if(!env->alloc)
  {
    std::cout << "Failed to begin resource update!" << std::endl;
    (*exports.DestroyEnvironment)(env);
    return ERR_MISSING_LOADER;
  }
#endif

  if(commandline.verbose.value)
    env->loglevel = LOG_NOTICE;
  if(!commandline.library_dir.value.empty())
    env->libpath = commandline.library_dir.value.c_str();
  if(!commandline.object_dir.value.empty())
    env->objpath = commandline.object_dir.value.c_str();
  if(!commandline.linker.value.empty())
    env->linker = commandline.linker.value.c_str();
  if(!commandline.system.value.empty())
    env->system = commandline.system.value.c_str();

  for(auto item : commandline.whitelist.values)
  {
    char* ctx;
    char* first  = STRTOK(const_cast<char*>(item.data()), ":", &ctx);
    char* second = STRTOK(NULL, ":", &ctx);

    if(!second)
      err = (*exports.AddWhitelist)(env, nullptr, first);
    else
      err = (*exports.AddWhitelist)(env, first, second);
    if(err < 0)
    {
      STRTOK(NULL, ":", &ctx);
      printerr(exports, env->log, "Error adding whitelist ", item.c_str(), err);
    }
  }

  // Add all embedding environments, plus the default environment
  commandline.libs.values.push_back(INNATIVE_DEFAULT_ENVIRONMENT);

  auto add_embeds = [](int tag, std::vector<std::string>& values, Environment* environment, INExports& exp) -> int {
    IN_ERROR result = ERR_SUCCESS;
    for(auto& embedding : values)
    {
      IN_ERROR e = (*exp.AddEmbedding)(environment, tag, embedding.c_str(), 0, 0);
      if(e < 0)
      {
        printerr(exp, environment->log, tag ? "Error loading shared library " : "Error loading embedding ",
                 embedding.c_str(), e);
        result = e;
      }
    }
    return result;
  };

  err = add_embeds(IN_TAG_ANY, commandline.libs.values, env, exports);
  err = std::min(err, add_embeds(IN_TAG_DYNAMIC, commandline.shared_libs.values, env, exports));

  if(err < 0)
  {
    (*exports.DestroyEnvironment)(env);
    return ERR_FATAL_FILE_ERROR;
  }

  // Load all modules
  std::unique_ptr<int[]> errs(new int[commandline.inputs.size()]);
  for(size_t i = 0; i < commandline.inputs.size(); ++i)
    (*exports.AddModule)(env, commandline.inputs[i], 0, u8path(commandline.inputs[i]).stem().u8string().c_str(), &errs[i]);

  // Ensure all modules are loaded, in case we have multithreading enabled
  err = (*exports.FinalizeEnvironment)(env);

  for(size_t i = 0; i < commandline.inputs.size(); ++i)
  {
    if(errs[i] < 0)
    {
      printerr(exports, env->log, "Error loading module ", commandline.inputs[i], errs[i]);
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

  if(commandline.serialize.set) // If you want to serialize the results, we do so now that the modules have been loaded
  {
    bool emitdebug = false;
    if(commandline.serialize.has_value) // If a name was specified, verify only one module exists
    {
      if(!STRICMP(commandline.serialize.value.c_str(), "emitdebug"))
        emitdebug = true;
      else if(env->n_modules != 1)
        fprintf(
          stderr,
          "If you have more than one module, you cannot specify an output file for serialization. Use [-s] by itself, instead.\n");
    }

    for(size_t i = 0; i < env->n_modules; ++i)
    {
      std::string target = env->modules[i].name.str();
      target += ".wat";

      if(!emitdebug && commandline.serialize.has_value)
        target = commandline.serialize.value;

      (*exports.SerializeModule)(env, i, target.c_str(), 0, emitdebug);
    }
  }

  // Check if this is a .wast file, which must be handled differently because it's an entire environment
  if(commandline.wast.size() > 0)
  {
    for(size_t i = 0; i < commandline.wast.size() && !err; ++i)
      err = (*exports.CompileScript)(reinterpret_cast<const uint8_t*>(commandline.wast[i]), 0, env, true,
                                     commandline.output_file.value.parent_path().u8string().c_str());
  }
  else // Attempt to compile. If an error happens, output it and any validation errors to stderr
    err = (*exports.Compile)(env, commandline.output_file.value.u8string().c_str());

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
  if(!commandline.wast.size())
  {
    if(commandline.run.value)
    {
      void* assembly = (*exports.LoadAssembly)(commandline.output_file.value.u8string().c_str());
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
      std::cout << "Successfully built " << commandline.output_file.value.u8string().c_str() << std::endl;
  }

  return err;
}