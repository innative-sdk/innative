// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "utility.h"
#include "link.h"
#include "compile.h"
#include "innative/export.h"
#include "jit.h"
#include "dump_symbols.h"
#include "lld/Common/ErrorHandler.h"

using namespace innative;
using namespace utility;

IN_ERROR innative::OutputObjectFile(Compiler& context, const path& out)
{
  std::error_code EC;
  llvm::raw_fd_ostream dest(out.u8string(), EC, llvm::sys::fs::OF_None);

  if(EC)
    return LogErrorString(context.env, "%s: Could not open file: %s", ERR_FATAL_FILE_ERROR, EC.message().c_str());

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CGFT_ObjectFile;
  llvm::TargetLibraryInfoImpl TLII(context.machine->getTargetTriple());
  pass.add(new llvm::TargetLibraryInfoWrapperPass(TLII));
  pass.add(createTargetTransformInfoWrapperPass(context.machine->getTargetIRAnalysis()));

  if(context.machine->addPassesToEmitFile(pass, dest, nullptr, FileType))
    return LogErrorString(context.env, "%s: %s can't emit an object file.", ERR_FATAL_FORMAT_ERROR,
                          context.machine->getTargetTriple().str().c_str());

  pass.run(*context.mod);
  dest.flush();
  return ERR_SUCCESS;
}

path innative::GetLinkerObjectPath(const Environment& env, Module& m, const path& outfile, LLD_FORMAT format)
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
  if(format == LLD_FORMAT::COFF)
    objpath += ".obj";
  else
    objpath += ".o";

  return objpath;
}

IN_ERROR innative::GenerateLinkerObjects(const Environment& env, std::vector<std::string>& cache, LLD_FORMAT format)
{
  for(size_t i = 0; i < env.n_modules; ++i)
  {
    assert(env.modules[i].cache != 0);
    assert(env.modules[i].name.get() != nullptr);
    path objfile = GetLinkerObjectPath(env, env.modules[i], path(), format);
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

    if(format == LLD_FORMAT::ELF && i == 0)
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
      (*env->loghook)(env, "Executing external linker command: %s\n", cmd.c_str());
    err = system(cmd.c_str());
  }
  else
  {
    if(env->loglevel >= LOG_DEBUG)
    {
      (*env->loghook)(env, "Executing internal linker command: ");

      for(auto arg : linkargs)
        (*env->loghook)(env, " \"%s\"", arg);

      (*env->loghook)(env, "\n");
    }

    std::string outbuf;
    llvm::raw_fd_ostream fdo(1, false, true);
    llvm::raw_string_ostream sso(outbuf);
    llvm::raw_ostream& llvm_stream = (env->loglevel >= LOG_NOTICE) ? static_cast<llvm::raw_ostream&>(fdo) :
                                                                     static_cast<llvm::raw_ostream&>(sso);
    bool result = false;
    switch(format)
    {
    case LLD_FORMAT::COFF: result = lld::coff::link(linkargs, false, llvm_stream, llvm_stream); break;
    case LLD_FORMAT::ELF: result = lld::elf::link(linkargs, false, llvm_stream, llvm_stream); break;
    }

    lld::errorHandler().reset();
    if(!result)
    {
      llvm_stream.flush(); // In certain error cases, the stream will not have been flushed properly
      if(env->loglevel < LOG_NOTICE)
        (*env->loghook)(env, "%.*s", outbuf.size(), outbuf.data());
      err = ERR_FATAL_LINK_ERROR;
    }
  }

  return err;
}

void innative::DeleteCache(const Environment& env, Module& m)
{
  // Certain error conditions can result in us clearing the cache of an invalid module.
  if(m.name.size() > 0) // Prevent an error from happening if the name is invalid.
    remove(GetLinkerObjectPath(env, m, path(),
                               env.abi == IN_ABI_Windows ? LLD_FORMAT::COFF :
                                                           LLD_FORMAT::ELF)); // Always remove the file if it exists

  if(m.cache != nullptr)
  {
    auto context = static_cast<Compiler*>(m.cache);
    if(env.jit)
    {
      if(!(env.flags & ENV_NO_INIT) && context->exit)
      {
        auto exit_name = CanonicalName(StringSpan::From(m.name), StringSpan::From(IN_EXIT_POSTFIX));
        if(auto sym = env.jit->Lookup(exit_name.c_str()))
        {
          reinterpret_cast<IN_Entrypoint>(sym.get().getAddress())();
        }
        else
        {
          std::string errMsg;
          llvm::raw_string_ostream buf{ errMsg };
          buf << sym.takeError();
          auto str = buf.str();
          (*env.loghook)(&env, "Error looking up JIT symbol `%s`: %s\n", exit_name.c_str(), str.c_str());
        }
      }
    }
    else
      delete context->mod;

    kh_destroy_importhash(context->importhash);
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

  if(env.jit)
    delete env.jit;
  else if(env.context)
    delete env.context;
  env.context = nullptr;
  env.jit     = nullptr;

  if(shutdown)
    llvm::llvm_shutdown();
}

std::vector<std::string> innative::GetSymbols(const char* file, size_t size, const Environment* env, LLD_FORMAT format)
{
  std::string outbuf;
  llvm::raw_string_ostream sso(outbuf);

  std::vector<std::string> symbols;
  auto result = dumpSymbolNamesFromFile(file, size);
  if(!result)
  {
    handleAllErrors(std::move(result.takeError()),
                    [&](const llvm::ErrorInfoBase& EI) { (*env->loghook)(env, EI.message().c_str()); });
    return symbols;
  }

  for(auto& i : result.get())
    symbols.push_back(std::move(i.Name));

  if(!symbols.size())
    (*env->loghook)(env, outbuf.c_str());
  return symbols;
}

void innative::AppendIntrinsics(Environment& env)
{
  int r;
  if(env.cimports)
    for(auto intrinsic : Compiler::intrinsics)
    {
      auto mangled = ABIMangle(intrinsic.name, env.abi, env.arch, 0, 0);
      kh_put_cimport(env.cimports, ByteArray::Identifier(AllocString(env, mangled.c_str()), mangled.size()), &r);
    }
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

size_t innative::ABIMangleBuffer(const char* src, char* buffer, size_t count, uint8_t abi, uint8_t arch, int convention,
                                 int bytes)
{
  if(abi == IN_ABI_Windows && arch == IN_ARCH_x86)
  {
    switch(convention)
    {
    case llvm::CallingConv::C: return snprintf(buffer, count, "_%s", src);
    case llvm::CallingConv::X86_StdCall: return snprintf(buffer, count, "_%s@%i", src, bytes);
    case llvm::CallingConv::X86_FastCall: return snprintf(buffer, count, "@%s@%i", src, bytes);
    }
  }

  return snprintf(buffer, count, "%s", src);
}

IN_ERROR innative::LinkEnvironment(const Environment* env, const path& file)
{
  path workdir   = GetWorkingDir();
  path libpath   = GetPath(env->libpath);
  path objpath   = !env->objpath ? file.parent_path() : GetPath(env->objpath);
  bool UseNatVis = false;

  // Finalize all modules
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    env->modules[i].cache->debugger->Finalize();
    UseNatVis = UseNatVis || !env->modules[i].cache->natvis.empty();

    if(env->flags & ENV_EMIT_LLVM)
    {
      std::error_code EC;
      auto path = ((file.parent_path() / env->modules[i].cache->mod->getName().str()) += ".llvm").u8string();
      llvm::raw_fd_ostream dest(path, EC, llvm::sys::fs::OF_None);
      env->modules[i].cache->mod->print(dest, nullptr);
    }

    // Verify module
    llvm::raw_fd_ostream dest(1, false, true);
    if(llvm::verifyModule(*env->modules[i].cache->mod, &dest))
      return LogErrorString(*env, "%s: %s failed validation", ERR_FATAL_INVALID_LLVM_IR, env->modules[i].name.str());
  }

  {
    std::vector<const char*> linkargs;
    std::vector<std::string> cache;
    LLD_FORMAT format = LLD_FORMAT::ELF;

    if(env->abi == IN_ABI_Windows)
    {
      // Note: If we ever allow compiling ELF on windows, all these arguments will have to be changed for the ELF linker
      format = LLD_FORMAT::COFF;

      // /PDB:"D:\code\innative\bin\innative-d.pdb" /IMPLIB:"D:\code\innative\bin\innative-d.lib"
      linkargs = {
        "/ERRORREPORT:QUEUE",
        "/INCREMENTAL:NO",
        "/NOLOGO",
        "/NODEFAULTLIB",
        /*"/MANIFESTUAC:level=asInvoker", "/MANIFEST:EMBED",*/ "/SUBSYSTEM:CONSOLE",
        "/VERBOSE",
        "/OPT:REF",
        "/DYNAMICBASE",
        "/NXCOMPAT",
      };

      switch(env->arch)
      {
      case IN_ARCH_x86:
        linkargs.push_back("/MACHINE:X86");
        linkargs.push_back("/LARGEADDRESSAWARE");
        linkargs.push_back("/SAFESEH:NO");
        break;
      case IN_ARCH_amd64: linkargs.push_back("/MACHINE:X64"); break;
      case IN_ARCH_ARM:
      case IN_ARCH_ARM64: linkargs.push_back("/MACHINE:ARM"); break;
      }

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

      cache = { std::string("/OUT:") + file.u8string(), "/LIBPATH:" + libpath.u8string(),
                "/LIBPATH:" + workdir.u8string() };

      if(UseNatVis)
      {
        auto embed = file.filename();
        embed      = objpath / embed.replace_extension("natvis");
        cache.emplace_back("/NATVIS:" + embed.u8string());
        FILE* f;
        FOPEN(f, embed.c_str(), "wb");
        if(!f)
          return LogErrorString(*env, "%s: Could not open file: %s", ERR_FATAL_FILE_ERROR, embed.c_str());

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
    }
    else
    {
      format = LLD_FORMAT::ELF;

      if(env->flags & ENV_LIBRARY)
        linkargs.push_back("-shared");
      if(!(env->flags & ENV_DEBUG))
        linkargs.push_back("--strip-debug");

      cache = { std::string("--output=") + file.u8string(), "-L" + libpath.u8string(), "-L" + workdir.u8string() };
    }

    std::vector<path> garbage;

    // Defer lambda deleting temporary files
    DeferLambda<std::function<void()>> deferclean([&garbage]() {
      for(auto& v : garbage)
        remove(v);
    });

    // Generate object code
    IN_ERROR err = GenerateLinkerObjects(*env, cache, format);
    if(err < 0)
      return err;

    // Write all in-memory environments to cache files
    for(Embedding* cur = env->embeddings; cur != nullptr; cur = cur->next)
    {
      if(env->abi != IN_ABI_Windows && cur->tag == 2)
      {
        if(cur->size > 0) // not supported
          return LogErrorString(*env, "%s: in-memory shared libraries are not supported.", ERR_FATAL_LINK_ERROR);

        path src = reinterpret_cast<const char*>(cur->data);
        if(!src.has_filename())
        {
          auto str = src.u8string();
          return LogErrorString(*env, "%s: %s is a folder, not a file.", ERR_FATAL_FILE_ERROR, str.c_str());
        }
        auto filename = src.filename().u8string();
        src.remove_filename();
        if(!src.empty())
        {
          cache.emplace_back(src.u8string());
          cache.back().insert(0, "-L");
        }

        cache.emplace_back(filename);
        cache.back().insert(0, "-l:");
      }
      else
      {
        if(cur->size > 0) // If the size is greater than 0, this is an in-memory embedding
        {
          union
          {
            Embedding* p;
            size_t z;
          } u        = { cur };
          auto embed = objpath / std::to_string(u.z);
          embed += IN_ENV_EXTENSION;
          embed += (env->abi == IN_ABI_Windows) ? ".lib" : ".a";
          cache.emplace_back(embed.u8string());
          FILE* f;
          FOPEN(f, embed.c_str(), "wb");
          if(!f)
            return LogErrorString(*env, "%s: Could not open file: %s", ERR_FATAL_FILE_ERROR, embed.c_str());

          fwrite(cur->data, 1, (size_t)cur->size, f);
          fclose(f);
          garbage.emplace_back(embed);
        }
        else
          cache.emplace_back(reinterpret_cast<const char*>(cur->data));
      }
    }
    for(auto& v : cache) // We can only do this after we're finished adding everything to cache
      linkargs.push_back(v.c_str());

    linkargs.insert(linkargs.begin(), ".");

    if(CallLinker(env, linkargs, format) != 0)
      return ERR_FATAL_LINK_ERROR;
  }
  return ERR_SUCCESS;
}
