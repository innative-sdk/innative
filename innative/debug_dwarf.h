// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_DWARF_H
#define IN__DEBUG_DWARF_H

#include "debug_sourcemap.h"
#include "stack.h"

namespace innative {
  namespace code {
    class DebugDWARF : public DebugSourceMap
    {
    public:
      DebugDWARF(SourceMap* s, Context* context, llvm::Module& m, const char* name, const char* filepath);
      virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) override;
      virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body) override;
      virtual void PostFuncBody(llvm::Function* fn, FunctionBody& body) override;
      virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
      virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
      virtual void DebugIns(llvm::Function* fn, Instruction& i) override;
      virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) override;
      virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) override;
      virtual void DebugSetGlobal(int index) override;
      virtual llvm::DIType* GetDebugType(size_t index, llvm::DIType* parent = 0) override;
      virtual void UpdateVariables(llvm::Function* fn, SourceMapScope& scope) override;
      void UpdateLocation(Instruction& i);
      SourceMapFunction* GetSourceFunction(unsigned int column);
      llvm::DIFile* GetSourceFile(size_t i);
      llvm::DIType* StructOffsetType(llvm::DIType* ty, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                     uint64_t indice, unsigned int bitsize, unsigned int bytealign);

      llvm::AllocaInst* stacklocal; // Stores the current frame pointer
    };
  }
}

#endif