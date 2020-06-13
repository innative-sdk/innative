// Copyright (c)2020 Black Sphere Studios
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

namespace innative {
  class JITContext
  {
  public:
    JITContext(llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL, std::unique_ptr<llvm::LLVMContext> ctx,
               struct kh_modulepair_s* whitelist);

    inline llvm::LLVMContext& GetContext() { return *Ctx.getContext(); }

    inline llvm::Error CompileModule(std::unique_ptr<llvm::Module> m)
    {
      return CompileLayer.add(MainJD, llvm::orc::ThreadSafeModule(std::move(m), Ctx));
    }

    llvm::Error CompileObject(const char* file);

    inline llvm::Expected<llvm::JITEvaluatedSymbol> Lookup(llvm::StringRef Name)
    {
      return ES.lookup(llvm::orc::JITDylibSearchOrder({ { &MainJD, llvm::orc::JITDylibLookupFlags::MatchAllSymbols } }),
                       Mangle(Name.str()));
    }

    inline llvm::Expected<std::unique_ptr<llvm::TargetMachine>> GetTargetMachine() { return jtmb.createTargetMachine(); }
    inline llvm::orc::ExecutionSession& GetSession() { return ES; }

  protected:
    bool WhitelistPredicate(const llvm::orc::SymbolStringPtr& s);

    llvm::orc::JITTargetMachineBuilder jtmb;
    llvm::orc::ExecutionSession ES;
    llvm::orc::RTDyldObjectLinkingLayer ObjectLayer;
    llvm::orc::IRCompileLayer CompileLayer;
    llvm::orc::IRTransformLayer TransformLayer;

    llvm::DataLayout DL;
    llvm::orc::MangleAndInterner Mangle;
    llvm::orc::ThreadSafeContext Ctx;
    llvm::orc::JITDylib& MainJD;
    struct kh_modulepair_s* Whitelist;
  };
}

#endif