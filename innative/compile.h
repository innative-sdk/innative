// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__COMPILE_H
#define IN__COMPILE_H

#include "innative/schema.h"
#include "constants.h"
#include "llvm.h"
#include "filesys.h"
#include "stack.h"
#include "debug.h"
#include <vector>
#include <string>

namespace innative {
  namespace code {
    struct Intrinsic;

    struct BlockResult
    {
      llvm::Value* v;
      llvm::BasicBlock* b;
      BlockResult* next;
    };

    struct Block
    {
      llvm::BasicBlock* block;  // Label
      llvm::BasicBlock* ifelse; // Label for else statement
      size_t limit;             // Limit of value stack
      varsint7 sig;             // Block signature
      uint8_t op;               // instruction that pushed this label
      BlockResult* results;     // Holds alternative branch results targeting this block
    };

    struct FunctionSet
    {
      llvm::Function* internal;
      llvm::Function* exported;
      llvm::Function* imported;
      Intrinsic* intrinsic;
      llvm::AllocaInst* memlocal;
    };

    KHASH_DECLARE(importhash, const char*, llvm::GlobalObject*);

    struct Context
    {
      const Environment& env;
      Module& m;
      llvm::LLVMContext& context;
      llvm::Module* llvm;
      llvm::IRBuilder<>& builder;
      llvm::TargetMachine* machine;
      kh_importhash_t* importhash;
      path objfile; // If this module has been compiled to a .obj file, stores the path so we can reliably delete it.
      llvm::IntegerType* intptrty;
      llvm::StructType* mempairty;
      std::unique_ptr<Debugger> debugger;
      Stack<llvm::Value*> values; // Tracks the current value stack
      Stack<Block> control;       // Control flow stack
      std::vector<llvm::AllocaInst*> locals;
      llvm::AllocaInst* memlocal;
      std::vector<llvm::GlobalVariable*> memories;
      std::vector<llvm::GlobalVariable*> tables;
      std::vector<llvm::GlobalVariable*> globals;
      llvm::GlobalVariable* exported_functions;
      std::vector<FunctionSet> functions;
      llvm::Function* init;
      llvm::Function* exit;
      llvm::Function* start;
      llvm::Function* memgrow;

      llvm::StructType* GetTableType(varsint7 element_type);
      llvm::StructType* GetPairType(llvm::Type* ty);
      llvm::Value* GetPairPtr(llvm::GlobalVariable* v, int index);
      llvm::Constant* GetPairNull(llvm::StructType* ty);
    };

    IN_ERROR InsertConditionalTrap(llvm::Value* cond, Context& context);
  }
}

#endif
