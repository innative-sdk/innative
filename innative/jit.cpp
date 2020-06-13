// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "jit.h"
#include "utility.h"
#include "tools.h"

using namespace innative;
using namespace llvm;
using namespace orc;

JITContext::JITContext(llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL, std::unique_ptr<LLVMContext> ctx,
                       struct kh_modulepair_s* whitelist) :
  jtmb(JTMB),
  ObjectLayer(ES, []() { return std::make_unique<SectionMemoryManager>(); }),
  CompileLayer(ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
  TransformLayer(ES, CompileLayer /*, optimizeModule*/),
  DL(std::move(DL)),
  Mangle(ES, this->DL),
  Ctx(std::move(ctx)),
  MainJD(ES.createJITDylib("<+innative_main>")),
  Whitelist(whitelist)
{
  ES.setErrorReporter([](Error e) { assert(!e); });
  MainJD.addGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
}

bool JITContext::WhitelistPredicate(const SymbolStringPtr& s) { return true; }

llvm::Error JITContext::CompileObject(const char* file)
{
  auto pred = [this](const SymbolStringPtr& s) { return WhitelistPredicate(s); };
  auto gen  = !file ? DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix(), pred) :
                     DynamicLibrarySearchGenerator::Load(file, DL.getGlobalPrefix(), pred);
  if(!gen)
    return gen.takeError();

  MainJD.addGenerator(std::move(gen.get()));
  return llvm::Error::success();
}

void innative::DumpJITState(Environment* env)
{
  auto fd = llvm::raw_fd_ostream(1, false); // TODO: try to emit this to env->log
  env->jit->GetSession().dump(fd);
  fd.flush();
}
