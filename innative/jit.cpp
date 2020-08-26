// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "jit.h"
#include "utility.h"
#include "tools.h"
#include "validate.h"

using namespace innative;
using namespace llvm;
using namespace orc;

JITContext::JITContext(JITTargetMachineBuilder JTMB, DataLayout DL, std::unique_ptr<LLVMContext> ctx, Environment* env) :
  jtmb(JTMB),
  OL(ES, []() { return std::make_unique<SectionMemoryManager>(); }),
  CL(ES, OL, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
  TL(ES, CL /*, optimizeModule*/),
  DL(std::move(DL)),
  Mangler(ES, this->DL),
  Ctx(std::move(ctx)),
  MainJD(ES.createJITDylib("<+innative_main>"))
{
  ES.setErrorReporter([env](Error e) {
    if(e)
      llvm::outs() << "JIT Error: " << e << "\n";
    AppendError(*env, env->errors, 0, ERR_RUNTIME_JIT_ERROR, "UNKNOWN JIT ERROR (check stdout)");
  });

  //MainJD.addGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));

  Whitelist = [env](const SymbolStringPtr& s) {
    if(!(env->flags & ENV_WHITELIST))
      return true;
    khiter_t iterexport = kh_get_modulepair(env->whitelist, (*s).data());
    return kh_exist2(env->whitelist, iterexport);
  };
}

Error JITContext::CompileEmbedding(const Embedding* embed)
{
  if(!embed)
    return AddGenerator<DynamicLibrarySearchGenerator>(
      DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix(), Whitelist));

  switch(embed->tag)
  {
  case IN_TAG_ANY:
    return make_error<StringError>("Cannot automatically determine if embedding is static or dynamic in JIT engine!",
                                   llvm::errc::not_supported);
  case IN_TAG_DYNAMIC:
    return AddGenerator<DynamicLibrarySearchGenerator>(
      DynamicLibrarySearchGenerator::Load(reinterpret_cast<const char*>(embed->data), DL.getGlobalPrefix(), Whitelist));
  case IN_TAG_STATIC:
    return AddGenerator<StaticLibraryDefinitionGenerator>(
      StaticLibraryDefinitionGenerator::Load(OL, reinterpret_cast<const char*>(embed->data)));
  }

  return make_error<StringError>("Unknown tag type", llvm::errc::invalid_argument);
}

void innative::DumpJITState(Environment* env)
{
  auto fd = llvm::raw_fd_ostream(1, false); // TODO: try to emit this to env->log
  env->jit->GetSession().dump(fd);
  fd.flush();
}
