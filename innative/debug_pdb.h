// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DEBUG_PDB_H
#define IN__DEBUG_PDB_H

#include "debug_sourcemap.h"
#include "stack.h"
#include <unordered_set>

namespace innative {
  KHASH_DECLARE(intmap, const char*, size_t);

  class DebugPDB : public DebugSourceMap
  {
  public:
    DebugPDB(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath);
    ~DebugPDB();
    virtual void PostFuncBody(llvm::Function* fn, FunctionBody& body) override;
    virtual llvm::DIType* GetDebugType(size_t index, llvm::DIType* parent = 0) override;
    virtual void UpdateVariables(llvm::Function* fn, SourceMapScope& scope) override;
    llvm::DIType* StructOffsetType(size_t index, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                   uint64_t indice, llvm::Function* fn);
    llvm::DIType* GetBaseType(size_t index, llvm::DIType* parent);
    virtual void Finalize() override;
    
  protected:
    void _finalize(const char* name);

    uint64_t _uid;
    kh_intmap_t* _deferred;
  };
}

#endif