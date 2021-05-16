// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__JIT_H
#define IN__JIT_H

#include "llvm.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "innative/schema.h"

namespace innative {
  class JITContext
  {
  public:
    JITContext(llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL, std::unique_ptr<llvm::LLVMContext> ctx,
               Environment* env);

    inline llvm::LLVMContext& GetContext() { return *Ctx.getContext(); }

    inline llvm::Error CompileModule(std::unique_ptr<llvm::Module> m)
    {
      return CL.add(MainJD, llvm::orc::ThreadSafeModule(std::move(m), Ctx));
    }

    llvm::Error CompileEmbedding(const Embedding* embed);

    inline llvm::Expected<llvm::JITEvaluatedSymbol> Lookup(llvm::StringRef Name)
    {
      auto str = Name.str();
      return ES.lookup(llvm::orc::JITDylibSearchOrder({ { &MainJD, llvm::orc::JITDylibLookupFlags::MatchAllSymbols } }),
                       Mangler(str));
    }

    inline llvm::Expected<std::unique_ptr<llvm::TargetMachine>> GetTargetMachine() { return jtmb.createTargetMachine(); }
    inline llvm::orc::ExecutionSession& GetSession() { return ES; }

    template<typename T> llvm::Error AddGenerator(llvm::Expected<std::unique_ptr<T>> gen)
    {
      if(!gen)
        return gen.takeError();

      MainJD.addGenerator(std::move(gen.get()));
      return llvm::Error::success();
    }

  protected:
    llvm::orc::JITTargetMachineBuilder jtmb;
    llvm::orc::ExecutionSession ES;
    llvm::orc::RTDyldObjectLinkingLayer OL;
    llvm::orc::IRCompileLayer CL;
    llvm::orc::IRTransformLayer TL;

    llvm::DataLayout DL;
    llvm::orc::MangleAndInterner Mangler;
    llvm::orc::ThreadSafeContext Ctx;
    llvm::orc::JITDylib& MainJD;
    std::function<bool(const llvm::orc::SymbolStringPtr& s)> Whitelist;
  };
}

#endif