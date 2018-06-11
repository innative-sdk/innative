// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __OPTIMIZE_H__NW__
#define __OPTIMIZE_H__NW__

#include "native-wasm/schema.h"
#include "stack.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Target/TargetMachine.h"

struct NWBlockResult
{
  llvm::Value* v;
  llvm::BasicBlock* b;
  NWBlockResult* next;
};

struct NWBlock
{
  llvm::BasicBlock* block; // Label
  size_t limit; // Limit of value stack
  varsint7 sig; // Block signature
  varuint7 type; // instruction that pushed this label
  NWBlockResult* results; // Holds alternative branch results targeting this block
};

struct NWContext
{
  Environment& env;
  Module& m;
  llvm::LLVMContext& context;
  llvm::Module* llvm;
  llvm::IRBuilder<>& builder;
  llvm::TargetMachine* machine;
  Stack<llvm::Value*> values; // Tracks the current value stack
  Stack<NWBlock> control; // Control flow stack
  varuint32 n_locals;
  llvm::AllocaInst** locals;
  varuint32 n_memory;
  llvm::GlobalVariable** linearmemory;
  varuint32 n_tables;
  llvm::GlobalVariable** tables;
  varuint32 n_globals;
  llvm::GlobalVariable** globals;
  varuint32 n_functions;
  llvm::Function** functions;
  llvm::Function* init;
  llvm::Function* start;
  llvm::Function* memgrow;
  llvm::Function* memsize;
};

ERROR_CODE WrapFunctions(Environment* env, NWContext& context);
ERROR_CODE AnnotateFunctions(Environment* env, NWContext* contexts);

#endif
