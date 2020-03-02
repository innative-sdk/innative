// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "innative/export.h"
#include "utility.h"
#include <fstream>

using namespace innative;

// Compiles LLVM IR to webassembly
int innative_compile_llvm(const char** files, size_t n, int flags, const char* out, FILE* log)
{
  // Construct the LLVM environment and current working directories
  llvm::LLVMContext llvm_compiler;

  bool has_start = false;
  IN_ERROR err   = ERR_SUCCESS;

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
  if(flags & ENV_LIBRARY)
    RM = llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
#endif
  auto machine = arch->createTargetMachine("wasm32-unknown-unknown", "", "", opt, RM, llvm::None);

  // We link everything into one giant module, because wasm currently doesn't work well with multiple modules
  llvm::SMDiagnostic diag;
  auto composite = new llvm::Module("wasm-output", llvm_compiler);
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

    if(link.linkInModule(llvm::parseIRFile(files[i], diag, llvm_compiler)))
    {
      fputs("Failed to link module: ", log);
      fputs(files[0], log);
      fputs("\nError: ", log);
      fputs(diag.getMessage().data(), log);
      return ERR_FATAL_LINK_ERROR;
    }
  }

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

  std::vector<const char*> args = { "inNative",      "--strip-all",       "--export-dynamic",
                                    objfile.c_str(), "--allow-undefined", outfile.c_str() };

  if(flags & ENV_LIBRARY)
    args.push_back("--no-entry");

  {
    llvm::raw_fd_ostream fdo(1, false, true);
    if(!lld::wasm::link(args, false, fdo))
      return ERR_FATAL_LINK_ERROR;
  }

  delete composite;
  fputs("Successfully compiled monolithic wasm module: ", log);
  fputs(out, log);
  return ERR_SUCCESS;
}
