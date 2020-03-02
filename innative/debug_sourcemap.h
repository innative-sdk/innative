// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_SOURCE_MAP_H
#define IN__DEBUG_SOURCE_MAP_H

#include "debug.h"
#include "stack.h"

namespace innative {
  class DebugSourceMap : public Debugger
  {
  public:
    DebugSourceMap(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath, char target);
    virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) override;
    virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body) override;
    virtual void PostFuncBody(llvm::Function* fn, FunctionBody& body) override;
    virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
    virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) override;
    virtual void DebugIns(llvm::Function* fn, Instruction& i) override;
    virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) override;
    virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) override;
    virtual void DebugSetGlobal(int index) override;
    virtual llvm::DIType* GetDebugType(size_t index, llvm::DIType* parent = 0);
    virtual void UpdateVariables(llvm::Function* fn, SourceMapScope& scope);
    void UpdateLocation(Instruction& i);
    SourceMapFunction* GetSourceFunction(unsigned int column);
    llvm::DIFile* GetSourceFile(size_t i);
    static llvm::DINode::DIFlags GetFlags(unsigned short flags);

    SourceMap* sourcemap;
    std::vector<llvm::DIType*> types;
    std::vector<llvm::DIFile*> files;             // sourcemap files
    std::vector<llvm::DISubprogram*> subprograms; // Seperate subprograms for when functions are split across files
    std::vector<llvm::DILocalScope*> scopecache;
    Stack<size_t> scopes; // stack of scope indexes
    size_t cursegment;
    size_t curscopeindex;
  };
}

#endif