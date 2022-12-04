// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "jit.h"
#include "utility.h"
#include "tools.h"
#include "validate.h"

using namespace innative;
using namespace llvm;
using namespace orc;

class DynLibSearchGenerator : public DefinitionGenerator
{
public:
  using SymbolPredicate = std::function<bool(const SymbolStringPtr&)>;
  DynLibSearchGenerator(sys::DynamicLibrary Dylib, char GlobalPrefix, SymbolPredicate Allow = SymbolPredicate());
  static Expected<std::unique_ptr<DynLibSearchGenerator>> Load(const char* FileName, char GlobalPrefix,
                                                               SymbolPredicate Allow = SymbolPredicate());

  static Expected<std::unique_ptr<DynLibSearchGenerator>> GetForCurrentProcess(char GlobalPrefix,
                                                                               SymbolPredicate Allow = SymbolPredicate())
  {
    return Load(nullptr, GlobalPrefix, std::move(Allow));
  }

  Error tryToGenerate(LookupState& LS, LookupKind K, JITDylib& JD, JITDylibLookupFlags JDLookupFlags,
                      const SymbolLookupSet& Symbols) override;

private:
  sys::DynamicLibrary Dylib;
  SymbolPredicate Allow;
  char GlobalPrefix;
};

JITContext::JITContext(std::unique_ptr<llvm::orc::ExecutionSession> es, llvm::orc::JITDylib& dylib, 
                       JITTargetMachineBuilder JTMB, std::unique_ptr<llvm::TargetMachine> tm, llvm::DataLayout dl,
                       std::unique_ptr<LLVMContext> ctx, Environment* env) :
  jtmb(JTMB),
  Ctx(std::move(ctx)),
  ES(std::move(es)),
  DL(std::move(dl)),
  OLL(*ES, []() { return std::make_unique<SectionMemoryManager>(); }),
  OTL(*ES, OLL),
  TM(std::move(tm)),
  CL(*ES, OLL, std::make_unique<SimpleCompiler>(*TM)),
  TL(*ES, CL /*, optimizeModule*/),
  Mangler(*ES, DL), 
  MainJD(dylib)
{
  if(jtmb.getTargetTriple().isOSBinFormatCOFF())
  {
    OLL.setOverrideObjectFlagsWithResponsibilityFlags(true);
    OLL.setAutoClaimResponsibilityForObjectSymbols(true);
  }
  
  ES->setErrorReporter([env](Error e) {
    if(e)
      llvm::outs() << "JIT Error: " << e << "\n";
    AppendError(*env, env->errors, 0, ERR_RUNTIME_JIT_ERROR, "UNKNOWN JIT ERROR (check stdout)");
  });

  Whitelist = [env](const SymbolStringPtr& s) {
    if(!(env->flags & ENV_WHITELIST))
      return true;
    khiter_t iterexport = kh_get_modulepair(env->whitelist, (*s).data());
    return kh_exist2(env->whitelist, iterexport);
  };
}

JITContext::~JITContext()
{
  if(auto Err = ES->endSession())
    ES->reportError(std::move(Err));
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
    return AddGenerator<DynLibSearchGenerator>(
      DynLibSearchGenerator::Load(reinterpret_cast<const char*>(embed->data), DL.getGlobalPrefix(), Whitelist));
  case IN_TAG_STATIC:
    return AddGenerator<StaticLibraryDefinitionGenerator>(
      StaticLibraryDefinitionGenerator::Load(OLL, reinterpret_cast<const char*>(embed->data)));
  }

  return make_error<StringError>("Unknown tag type", llvm::errc::invalid_argument);
}

void innative::DumpJITState(Environment* env)
{
  auto fd = llvm::raw_fd_ostream(1, false); // TODO: try to emit this to env->log
  env->jit->GetSession().dump(fd);
  fd.flush();
}

llvm::Expected<llvm::JITEvaluatedSymbol> JITContext::Lookup(llvm::StringRef Name)
{
  auto str = Name.str();
  return ES->lookup(llvm::orc::JITDylibSearchOrder({ { &MainJD, llvm::orc::JITDylibLookupFlags::MatchAllSymbols } }),
                    Mangler(str));
} 

llvm::Error JITContext::CompileModule(std::unique_ptr<llvm::Module> m)
{
  promote(*m);
   
  for(auto& GV : m->global_values())
  {
    std::string view = GV.getName().str();
    if(GV.hasWeakAnyLinkage())
    {
      GV.setLinkage(GlobalValue::ExternalLinkage);
      GV.setVisibility(GlobalValue::HiddenVisibility);
    }
  }

  auto TSM = llvm::orc::ThreadSafeModule(std::move(m), Ctx);
  if(auto Err = TSM.withModuleDo([&](llvm::Module& M) -> llvm::Error {
       if(M.getDataLayout().isDefault())
         M.setDataLayout(DL);

       if(M.getDataLayout() != DL)
         return make_error<StringError>("Added modules have incompatible data layouts: " +
                                          M.getDataLayout().getStringRepresentation() + " (module) vs " +
                                          DL.getStringRepresentation() + " (jit)",
                                        inconvertibleErrorCode());

       return llvm::Error::success();
     }))
    return Err;
  return CL.add(MainJD.getDefaultResourceTracker(), std::move(TSM));
}

llvm::Expected<std::unique_ptr<JITContext>> JITContext::Create(std::unique_ptr<llvm::LLVMContext> ctx, Environment* env)
{
  auto jtmb = JITTargetMachineBuilder::detectHost();
  if(!jtmb)
    return jtmb.takeError();

  auto sepc = SelfExecutorProcessControl::Create();
  if(!sepc)
    return sepc.takeError();

  auto es = std::make_unique<ExecutionSession>(std::move(*sepc));

  auto mainlib = es->createJITDylib("<+innative_main>");
  if(!mainlib)
    return mainlib.takeError();

  auto dl = (*jtmb).getDefaultDataLayoutForTarget();
  if(!dl)
    return dl.takeError();

  auto tm = (*jtmb).createTargetMachine();
  if(!tm)
    return tm.takeError();

  return std::make_unique<JITContext>(std::move(es), *mainlib, *jtmb, std::move(*tm), std::move(*dl), std::move(ctx), env);
}

DynLibSearchGenerator::DynLibSearchGenerator(sys::DynamicLibrary Dylib, char GlobalPrefix, SymbolPredicate Allow) :
  Dylib(std::move(Dylib)), Allow(std::move(Allow)), GlobalPrefix(GlobalPrefix)
{}

Expected<std::unique_ptr<DynLibSearchGenerator>> DynLibSearchGenerator::Load(const char* FileName, char GlobalPrefix,
                                                                             SymbolPredicate Allow)
{
  std::string ErrMsg;
  auto Lib = sys::DynamicLibrary::getPermanentLibrary(FileName, &ErrMsg);
  if(!Lib.isValid())
    return make_error<StringError>(std::move(ErrMsg), inconvertibleErrorCode());
  return std::make_unique<DynLibSearchGenerator>(std::move(Lib), GlobalPrefix, std::move(Allow));
}

Error DynLibSearchGenerator::tryToGenerate(LookupState& LS, LookupKind K, JITDylib& JD, JITDylibLookupFlags JDLookupFlags,
                                           const SymbolLookupSet& Symbols)
{
  orc::SymbolMap NewSymbols;

  bool HasGlobalPrefix = (GlobalPrefix != '\0');

  for(auto& KV : Symbols)
  {
    auto& Name = KV.first;

    if((*Name).empty())
      continue;

    if(Allow && !Allow(Name))
      continue;

    if(HasGlobalPrefix && (*Name).front() != GlobalPrefix)
      continue;

    std::string tmp((*Name).data() + HasGlobalPrefix, (*Name).size() - HasGlobalPrefix);
    void* Addr = Dylib.getAddressOfSymbol(tmp.c_str());
    if(!Addr)
      tmp = "__imp_" + tmp;

    if(Addr = Dylib.getAddressOfSymbol(tmp.c_str()))
    {
      NewSymbols[Name] =
        JITEvaluatedSymbol(static_cast<JITTargetAddress>(reinterpret_cast<uintptr_t>(Addr)), JITSymbolFlags::Exported);
    }
  }

  if(NewSymbols.empty())
    return Error::success();

  return JD.define(absoluteSymbols(std::move(NewSymbols)));
}