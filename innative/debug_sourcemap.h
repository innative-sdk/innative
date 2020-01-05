// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_SOURCE_MAP_H
#define IN__DEBUG_SOURCE_MAP_H

#include "debug.h"
#include "stack.h"

namespace innative {
  namespace code {
    class DebugSourceMap : public Debugger
    {
    public:
      DebugSourceMap(SourceMap* s, llvm::IntegerType* intptr, llvm::Module& m, const char* name, Environment* env,
                     const char* filepath);
      const SourceMapSegment* GetMappedSegment(unsigned int line, unsigned int column);
      llvm::DIFile* ResolveMappedSegment(unsigned int& line, unsigned int& column);
      llvm::DILocation* GetMappedLocation(unsigned int line, unsigned int column, llvm::DILocalScope* scope,
                                          llvm::LLVMContext& context);
      virtual void FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) override;
      virtual void FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body,
                            code::Context& context) override;
      virtual void FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) override;
      virtual void FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) override;
      virtual void DebugIns(llvm::Function* fn, Instruction& i, code::Context& context) override;
      virtual void DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) override;
      virtual void PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) override;

      llvm::DILocation* UpdateLocation(llvm::DILocation* loc, Instruction& i, code::Context& context);
      SourceMapFunction* GetSourceFunction(unsigned int column);
      llvm::DIFile* GetSourceFile(size_t i);
      llvm::DIType* SourceDebugType(size_t index);

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
}

#endif