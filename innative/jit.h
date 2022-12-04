// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__JIT_H
#define IN__JIT_H

#include "llvm.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "innative/schema.h"

namespace innative {
  class JITContext
  {
  public:
    JITContext(std::unique_ptr<llvm::orc::ExecutionSession> es, llvm::orc::JITDylib& dylib,
               llvm::orc::JITTargetMachineBuilder JTMB, std::unique_ptr<llvm::TargetMachine> tm, llvm::DataLayout dl,
               std::unique_ptr<llvm::LLVMContext> ctx, Environment* env);
    ~JITContext();

    inline llvm::LLVMContext& GetContext() { return *Ctx.getContext(); }

    llvm::Error CompileModule(std::unique_ptr<llvm::Module> m);
    llvm::Error CompileEmbedding(const Embedding* embed);
    llvm::Expected<llvm::JITEvaluatedSymbol> Lookup(llvm::StringRef Name);

    inline llvm::TargetMachine& GetTargetMachine() { return *TM; }
    inline llvm::orc::ExecutionSession& GetSession() { return *ES; }

    template<typename T> llvm::Error AddGenerator(llvm::Expected<std::unique_ptr<T>> gen)
    {
      if(!gen)
        return gen.takeError();

      MainJD.addGenerator(std::move(gen.get()));
      return llvm::Error::success();
    }

    static llvm::Expected<std::unique_ptr<JITContext>> Create(std::unique_ptr<llvm::LLVMContext> ctx, Environment* env);

  protected:
    llvm::orc::JITTargetMachineBuilder jtmb;
    llvm::orc::ThreadSafeContext Ctx;
    std::unique_ptr<llvm::orc::ExecutionSession> ES;
    llvm::orc::RTDyldObjectLinkingLayer OLL;
    llvm::DataLayout DL;
    llvm::orc::ObjectTransformLayer OTL; 
    std::unique_ptr<llvm::TargetMachine> TM;
    llvm::orc::IRCompileLayer CL;
    llvm::orc::IRTransformLayer TL;
    llvm::orc::MangleAndInterner Mangler;

    llvm::orc::JITDylib& MainJD;
    llvm::orc::SymbolLinkagePromoter promote;
    std::function<bool(const llvm::orc::SymbolStringPtr& s)> Whitelist;
  };
}

#endif