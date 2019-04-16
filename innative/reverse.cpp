// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "innative/path.h"
#include "util.h"
#pragma warning(push)
#pragma warning(disable : 4146 4267 4141 4244 4624)
#define _SCL_SECURE_NO_WARNINGS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Linker/Linker.h"
#include "llvm/IR/LegacyPassManager.h"
#include "lld/Common/Driver.h"
#pragma warning(pop)
#include <fstream>

using namespace innative;

// Compiles LLVM IR to webassembly
int innative_compile_llvm(const char** files, size_t n, int flags, const char* out, FILE* log, const char* sdkpath, const char* arg0)
{
  // Construct the LLVM environment and current working directories
  llvm::LLVMContext llvm_context;
  Path workdir = utility::GetWorkingDir();
  Path sdkdir(!sdkpath ? utility::GetProgramPath(arg0).BaseDir().c_str() : sdkpath);

  bool has_start = false;
  IN_ERROR err = ERR_SUCCESS;

  // Set up our target architecture, necessary up here so our code generation knows how big a pointer is
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string llvm_err;
  auto arch = llvm::TargetRegistry::lookupTarget("wasm32-unknown-unknown", llvm_err);

  if(!arch)
    return ERR_FATAL_NULL_POINTER;

  llvm::TargetOptions opt;
  auto RM = llvm::Optional<llvm::Reloc::Model>();
#ifdef IN_PLATFORM_POSIX
  if(env->flags&ENV_LIBRARY)
    RM = llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
#endif
  auto machine = arch->createTargetMachine("wasm32-unknown-unknown", "", "", opt, RM, llvm::None);

  // We link everything into one giant module, because wasm currently doesn't work well with multiple modules
  llvm::SMDiagnostic diag;
  auto composite = new llvm::Module("wasm-output", llvm_context);
  composite->setTargetTriple(machine->getTargetTriple().getTriple());
  llvm::Linker link(*composite);

  for(size_t i = 0; i < n; ++i)
  {
    if(!std::ifstream(files[i]))
    {
      fputs(files[i], log);
      fputs(" does not exist!\n", log);
      return ERR_FATAL_FILE_ERROR;
    }

    if(link.linkInModule(llvm::parseIRFile(files[i], diag, llvm_context)))
    {
      fputs("Failed to link module: ", log);
      fputs(files[0], log);
      fputs("\nError: ", log);
      fputs(diag.getMessage().data(), log);
      return ERR_FATAL_LINK_ERROR;
    }
  }
  
  /*if(link.linkInModule(llvm::parseIRFile((sdkdir + "buddy-malloc.ll").Get().c_str(), diag, llvm_context)))
  {
    fputs("Failed to link utility IR\nError: ", log);
    fputs(diag.getMessage().data(), log);
  }*/

  std::error_code EC;
  std::string objfile(out);
  objfile += ".o";
  llvm::raw_fd_ostream dest(objfile, EC, llvm::sys::fs::F_None);

  if(EC)
  {
    fputs("Could not open file: ", log);
    fputs(EC.message().c_str(), log);
    fputs("\n", log);
    return ERR_FATAL_FILE_ERROR;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::TargetMachine::CGFT_ObjectFile;

  if(machine->addPassesToEmitFile(pass, dest, nullptr, FileType))
  {
    fputs("TheTargetMachine can't emit a file of this type.\n", log);
    return ERR_FATAL_FILE_ERROR;
  }

  pass.run(*composite);
  dest.flush();
  std::string outfile("-o");
  outfile += out;
  
  std::vector<const char*> args = { "inNative", "--strip-all", "--export-dynamic", objfile.c_str(), "--allow-undefined", outfile.c_str() };

  if(flags&ENV_LIBRARY)
    args.push_back("--no-entry");

  {
    llvm::raw_fd_ostream fdo(1, false, true);
    if(!lld::wasm::link(args, false, fdo))
      return ERR_FATAL_LINK_ERROR;
  }

  fputs("Successfully compiled monolithic wasm module: ", log);
  fputs(out, log);
  return ERR_SUCCESS;
}
