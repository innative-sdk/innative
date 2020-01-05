// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_WAT_H
#define IN__DEBUG_WAT_H

#include "debug.h"
#include "stack.h"

namespace innative {
  namespace code {
    class DebugWat : public Debugger
    {
    public:
      DebugWat(llvm::IntegerType* intptr, llvm::Module& m, const char* name, const Environment* env, const char* filepath);
      virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) override;
      virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body,
                            code::Context& context) override;
      virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) override;
      virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) override;
      virtual void DebugIns(llvm::Function* fn, Instruction& i, code::Context& context) override;
      virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) override;
      virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) override;
      virtual void PopBlock() override;
      
      Stack<llvm::DILocalScope*> scopes;
    };
  }
}

#endif