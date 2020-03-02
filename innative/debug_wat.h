// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_WAT_H
#define IN__DEBUG_WAT_H

#include "debug.h"
#include "stack.h"

namespace innative {
  class DebugWat : public Debugger
  {
  public:
    DebugWat(Compiler* compiler, llvm::Module& m, const char* name, const char* filepath);
    virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) override;
    virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body) override;
    virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
    virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
    virtual void DebugIns(llvm::Function* fn, Instruction& i) override;
    virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) override;
    virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) override;
    virtual void PopBlock() override;

    Stack<llvm::DILocalScope*> scopes;
  };
}

#endif