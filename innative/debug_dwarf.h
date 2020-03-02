// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_DWARF_H
#define IN__DEBUG_DWARF_H

#include "debug_sourcemap.h"
#include "stack.h"

namespace innative {
  class DebugDWARF : public DebugSourceMap
  {
  public:
    DebugDWARF(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath);
    virtual void PostFuncBody(llvm::Function* fn, FunctionBody& body) override;
    virtual llvm::DIType* GetDebugType(size_t index, llvm::DIType* parent = 0) override;
    virtual void UpdateVariables(llvm::Function* fn, SourceMapScope& scope) override;
    llvm::DIType* StructOffsetType(llvm::DIType* ty, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                   uint64_t indice, unsigned int bitsize, unsigned int bytealign);
  };
}

#endif