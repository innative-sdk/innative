// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "utility.h"
#include "link.h"
#include "compile.h"
#include "innative/export.h"

using namespace innative;

IN_ERROR innative::OutputObjectFile(Compiler& context, const path& out)
{
  std::error_code EC;
  llvm::raw_fd_ostream dest(out.u8string(), EC, llvm::sys::fs::F_None);

  if(EC)
  {
    if(context.env.loglevel >= LOG_FATAL)
    {
      fputs("Could not open file: ", context.env.log);
      fputs(EC.message().c_str(), context.env.log);
    }
    return ERR_FATAL_FILE_ERROR;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::TargetMachine::CGFT_ObjectFile;
  llvm::TargetLibraryInfoImpl TLII(context.machine->getTargetTriple());
  pass.add(new llvm::TargetLibraryInfoWrapperPass(TLII));
  pass.add(createTargetTransformInfoWrapperPass(context.machine->getTargetIRAnalysis()));

  if(context.machine->addPassesToEmitFile(pass, dest, nullptr, FileType))
  {
    if(context.env.loglevel >= LOG_FATAL)
      fputs("TheTargetMachine can't emit a file of this type", context.env.log);
    return ERR_FATAL_FILE_ERROR;
  }

  pass.run(*context.mod);
  dest.flush();
  return ERR_SUCCESS;
}

path innative::GetLinkerObjectPath(const Environment& env, Module& m, const path& outfile)
{
  if(m.cache != nullptr && !m.cache->objfile.empty())
    return m.cache->objfile;
  if(!m.name.size())
    return path();

  path objpath = !env.objpath ? (outfile.empty() ? "" : outfile.parent_path()) : utility::GetPath(env.objpath);

  // The default module name must be an entire path to gaurantee uniqueness, but we need to write it in the objpath
  // directory
  std::string file(m.name.str());
  std::replace(file.begin(), file.end(), '\\', '_');
  std::replace(file.begin(), file.end(), '/', '_');
  std::replace(file.begin(), file.end(), '.', '_');

  objpath /= file;
#ifdef IN_PLATFORM_WIN32
  objpath += ".obj";
#else
  objpath += ".o";
#endif
  return objpath;
}

IN_ERROR innative::GenerateLinkerObjects(const Environment& env, std::vector<std::string>& cache)
{
  for(size_t i = 0; i < env.n_modules; ++i)
  {
    assert(env.modules[i].cache != 0);
    assert(env.modules[i].name.get() != nullptr);
    path objfile = GetLinkerObjectPath(env, env.modules[i], path());
    cache.emplace_back(objfile.u8string());

    FILE* f;
    FOPEN(f, objfile.c_str(), "rb");
    if(f)
      fclose(f);
    else
    {
      IN_ERROR err = OutputObjectFile(*env.modules[i].cache, objfile);
      if(err < 0)
        return err;
    }

#ifdef IN_PLATFORM_POSIX
    if(i == 0)
    { // https://stackoverflow.com/questions/9759880/automatically-executed-functions-when-loading-shared-libraries
      if(!(env.flags &
           ENV_LIBRARY)) // If this isn't a shared library, we must specify an entry point instead of an init function
        cache.emplace_back("--entry=" IN_INIT_FUNCTION);
      else if(!(env.flags & ENV_NO_INIT)) // Otherwise only specify entry functions if we actually want them
      {
        cache.emplace_back("-init=" IN_INIT_FUNCTION);
        cache.emplace_back("-fini=" IN_EXIT_FUNCTION);
      }
    }
#endif
  }

  return ERR_SUCCESS;
}

int innative::CallLinker(const Environment* env, std::vector<const char*>& linkargs, LLD_FORMAT format)
{
  int err = ERR_SUCCESS;

  // Link object code
  if(env->linker != 0)
  {
#ifdef IN_PLATFORM_WIN32
    const char* quote = "\"";
#elif defined(IN_PLATFORM_POSIX)
    const char* quote = "";
#endif
    std::string cmd;
    cmd += quote; // for windows we have to double quote the entire bloody command because system() actually calls "cmd
                  // /c <string>"
    cmd += quote;
    cmd += env->linker;
    cmd += quote;

    size_t sz = cmd.size();
    for(auto arg : linkargs)
      sz += strlen(arg);
    cmd.reserve(sz + 1 + linkargs.size() * 3);
    for(auto arg : linkargs)
    {
      cmd += ' ';
      cmd += quote;
      cmd += arg;
      cmd += quote;
    }

    cmd += quote;

    if(env->loglevel >= LOG_DEBUG)
      FPRINTF(env->log, "Executing external linker command: %s\n", cmd.c_str());
    err = system(cmd.c_str());
  }
  else
  {
    if(env->loglevel >= LOG_DEBUG)
    {
      fputs("Executing internal linker command: ", env->log);
      for(auto arg : linkargs)
      {
        fputc(' ', env->log);
        fputc('"', env->log);
        fputs(arg, env->log);
        fputc('"', env->log);
      }
      fputc('\n', env->log);
      fflush(env->log);
    }

    linkargs.insert(linkargs.begin(), "lld");
    std::string outbuf;
    llvm::raw_fd_ostream fdo(1, false, true);
    llvm::raw_string_ostream sso(outbuf);
    llvm::raw_ostream& llvm_stream = (env->loglevel >= LOG_NOTICE) ? static_cast<llvm::raw_ostream&>(fdo) :
                                                                     static_cast<llvm::raw_ostream&>(sso);
    bool result = false;
    switch(format)
    {
    case LLD_FORMAT::COFF: result = lld::coff::link(linkargs, false, llvm_stream); break;
    case LLD_FORMAT::ELF: result = lld::elf::link(linkargs, false, llvm_stream); break;
    }

    if(!result)
    {
      llvm_stream.flush(); // In certain error cases, the stream will not have been flushed properly
      if(env->loglevel < LOG_NOTICE)
        fwrite(outbuf.data(), 1, outbuf.size(), env->log);
      err = ERR_FATAL_LINK_ERROR;
    }
  }

  return err;
}

void innative::DeleteCache(const Environment& env, Module& m)
{
  // Certain error conditions can result in us clearing the cache of an invalid module.
  if(m.name.size() > 0)                          // Prevent an error from happening if the name is invalid.
    remove(GetLinkerObjectPath(env, m, path())); // Always remove the file if it exists

  if(m.cache != nullptr)
  {
    auto context = static_cast<Compiler*>(m.cache);
    kh_destroy_importhash(context->importhash);
    delete context->mod;
    delete context;
    m.cache = nullptr;
  }
}

void innative::DeleteContext(Environment& env, bool shutdown)
{
  for(varuint32 i = 0; i < env.n_modules; ++i)
  {
    DeleteCache(env, env.modules[i]);
  }

  delete env.context;
  env.context = nullptr;

  if(shutdown)
    llvm::llvm_shutdown();
}

std::vector<std::string> innative::GetSymbols(const char* file, size_t size, FILE* log, LLD_FORMAT format)
{
  std::string outbuf;
  llvm::raw_string_ostream sso(outbuf);

  uint8_t kind = 0;
  std::vector<std::string> symbols;
  switch(format)
  {
  case LLD_FORMAT::COFF:
    lld::coff::iterateSymbols(
      file, size, [](void* state, const char* s) { reinterpret_cast<std::vector<std::string>*>(state)->push_back(s); },
      &symbols, sso);
    break;
  case LLD_FORMAT::ELF:
    switch(CURRENT_ARCH_BITS | (CURRENT_LITTLE_ENDIAN << 15))
    {
    case 32 | (1 << 15): kind = 1; break;
    case 32 | (0 << 15): kind = 2; break;
    case 64 | (1 << 15): kind = 3; break;
    case 64 | (0 << 15): kind = 4; break;
    default: fputs("ERROR: unknown arch bits or endian!\n", log); return symbols;
    }

    lld::elf::iterateSymbols(
      file, size, [](void* state, const char* s) { reinterpret_cast<std::vector<std::string>*>(state)->push_back(s); },
      &symbols,
      std::make_tuple(kind, (uint16_t)CURRENT_ARCH, (CURRENT_ABI == ABI::FreeBSD) ? (uint8_t)CURRENT_ABI : (uint8_t)0),
      sso);
    break;
  }

  if(!symbols.size())
    fputs(outbuf.c_str(), log);
  return symbols;
}

void innative::AppendIntrinsics(Environment& env)
{
  int r;
  if(env.cimports)
    for(auto intrinsic : Compiler::intrinsics)
      kh_put_cimport(env.cimports, ByteArray::Identifier(intrinsic.name, strlen(intrinsic.name)), &r);
}

int innative::GetParameterBytes(const IN_WASM_MODULE& m, const Import& imp)
{
  int total = 0;
  if(imp.kind == WASM_KIND_FUNCTION && imp.func_desc.type_index < m.type.n_functypes)
  {
    auto& sig = m.type.functypes[imp.func_desc.type_index];
    for(varuint32 i = 0; i < sig.n_params; ++i)
    {
      switch(sig.params[i])
      {
      case TE_f32:
      case TE_i32: total += 4; break;
      case TE_i64:
      case TE_f64: total += 8; break;
      case TE_funcref:
      case TE_cref:
#ifdef IN_32BIT
        total += 4;
#else
        total += 8;
#endif
        break;
      }
    }
  }
  return total;
}

std::string innative::ABIMangle(const std::string& src, ABI abi, int convention, int bytes)
{
  if(abi == ABI::Win32)
  {
    switch(convention)
    {
    case llvm::CallingConv::C: return "_" + src;
    case llvm::CallingConv::X86_StdCall: return "_" + src + "@" + std::to_string(bytes);
    case llvm::CallingConv::X86_FastCall: return "@" + src + "@" + std::to_string(bytes);
    }
  }

  return src;
}

IN_ERROR innative::LinkEnvironment(const Environment* env, const path& file)
{
  path workdir   = utility::GetWorkingDir();
  path libpath   = utility::GetPath(env->libpath);
  path objpath   = !env->objpath ? file.parent_path() : utility::GetPath(env->objpath);
  bool UseNatVis = false;

  // Finalize all modules
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    env->modules[i].cache->debugger->Finalize();
    UseNatVis = UseNatVis || !env->modules[i].cache->natvis.empty();

    if(env->flags & ENV_EMIT_LLVM)
    {
      std::error_code EC;
      llvm::raw_fd_ostream dest(((file.parent_path() / env->modules[i].cache->mod->getName().str()) += ".llvm").u8string(),
                                EC, llvm::sys::fs::F_None);
      env->modules[i].cache->mod->print(dest, nullptr);
    }

    // Verify module
    llvm::raw_fd_ostream dest(1, false, true);
    if(llvm::verifyModule(*env->modules[i].cache->mod, &dest))
      return ERR_FATAL_INVALID_MODULE;
  }

  {
#ifdef IN_PLATFORM_WIN32
    LLD_FORMAT format = LLD_FORMAT::COFF;

    // /PDB:"D:\code\innative\bin\innative-d.pdb" /IMPLIB:"D:\code\innative\bin\innative-d.lib"
    std::vector<const char*> linkargs = {
      "/ERRORREPORT:QUEUE",
      "/INCREMENTAL:NO",
      "/NOLOGO",
      "/NODEFAULTLIB",
      /*"/MANIFESTUAC:level=asInvoker", "/MANIFEST:EMBED",*/ "/SUBSYSTEM:CONSOLE",
      "/VERBOSE",
      "/OPT:REF",
      "/DYNAMICBASE",
      "/NXCOMPAT",
  #ifdef IN_CPU_x86_64
      "/MACHINE:X64",
  #elif defined(IN_CPU_x86)
      "/MACHINE:X86",
      "/LARGEADDRESSAWARE",
  #elif defined(IN_CPU_ARM) || defined(IN_CPU_ARM64)
      "/MACHINE:ARM",
  #endif
    };

    if(env->flags & ENV_LIBRARY)
    {
      linkargs.push_back("/DLL");
      linkargs.push_back("/ENTRY:" IN_INIT_FUNCTION "-stub");
    }
    else
      linkargs.push_back("/ENTRY:" IN_INIT_FUNCTION);

    if(env->flags & ENV_DEBUG)
    {
      linkargs.push_back("/DEBUG");
      linkargs.push_back("/OPT:NOICF");
    }
    else
      linkargs.push_back("/OPT:ICF");

    std::vector<std::string> cache = { std::string("/OUT:") + file.u8string(), "/LIBPATH:" + libpath.u8string(),
                                       "/LIBPATH:" + workdir.u8string() };

    if(UseNatVis)
    {
      auto embed = file.filename();
      embed      = objpath / embed.replace_extension("natvis");
      cache.emplace_back("/NATVIS:" + embed.u8string());
      FILE* f;
      FOPEN(f, embed.c_str(), "wb");
      if(!f)
        return ERR_FATAL_FILE_ERROR;

      const char prologue[] =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?><AutoVisualizer xmlns=\"http://schemas.microsoft.com/vstudio/debugger/natvis/2010\">";
      const char epilogue[] = "</AutoVisualizer>";

      fwrite(prologue, 1, sizeof(prologue) - 1, f);

      for(varuint32 i = 0; i < env->n_modules; ++i)
        fwrite(env->modules[i].cache->natvis.data(), 1, env->modules[i].cache->natvis.size(), f);

      fwrite(epilogue, 1, sizeof(epilogue) - 1, f);
      fclose(f);
    }

    for(varuint32 i = 0; i < env->n_exports; ++i)
      cache.push_back(std::string("/EXPORT:") + env->exports[i]);

#elif defined(IN_PLATFORM_POSIX)
    LLD_FORMAT format                 = LLD_FORMAT::ELF;
    std::vector<const char*> linkargs = {};

    if(env->flags & ENV_LIBRARY)
      linkargs.push_back("-shared");
    if(!(env->flags & ENV_DEBUG))
      linkargs.push_back("--strip-debug");

    std::vector<std::string> cache = { std::string("--output=") + file.u8string(), "-L" + libpath.u8string(),
                                       "-L" + workdir.u8string() };
#else
  #error unknown platform
#endif
    std::vector<path> garbage;

    // Defer lambda deleting temporary files
    utility::DeferLambda<std::function<void()>> deferclean([&garbage]() {
      for(auto& v : garbage)
        remove(v);
    });

    // Generate object code
    IN_ERROR err = GenerateLinkerObjects(*env, cache);
    if(err < 0)
      return err;

    // Write all in-memory environments to cache files
    for(Embedding* cur = env->embeddings; cur != nullptr; cur = cur->next)
    {
      if(cur->size > 0) // If the size is greater than 0, this is an in-memory embedding
      {
        union
        {
          Embedding* p;
          size_t z;
        } u        = { cur };
        auto embed = objpath / std::to_string(u.z);
        embed += utility::IN_ENV_EXTENSION;
        embed += IN_STATIC_EXTENSION;
        cache.emplace_back(embed.u8string());
        FILE* f;
        FOPEN(f, embed.c_str(), "wb");
        if(!f)
          return ERR_FATAL_FILE_ERROR;

        fwrite(cur->data, 1, (size_t)cur->size, f);
        fclose(f);
        garbage.emplace_back(embed);
      }
      else
        cache.emplace_back(reinterpret_cast<const char*>(cur->data));

#ifdef IN_PLATFORM_POSIX
      if(cur->tag == 2)
        cache.back().insert(0, (cache.back()[0] == '/') ? "-l:" : "-l");
#endif
    }

    for(auto& v : cache) // We can only do this after we're finished adding everything to cache
      linkargs.push_back(v.c_str());

    if(CallLinker(env, linkargs, format) != 0)
      return ERR_FATAL_LINK_ERROR;
  }
  return ERR_SUCCESS;
}
