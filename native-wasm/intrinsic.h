// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __INTRINSIC_H__NW__
#define __INTRINSIC_H__NW__

#include "native-wasm/schema.h"
#include "stack.h"
#pragma warning(push)
#pragma warning(disable:4146)
#define _SCL_SECURE_NO_WARNINGS
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
#pragma warning(pop)

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
  byte type; // instruction that pushed this label
  NWBlockResult* results; // Holds alternative branch results targeting this block
};

struct NWFunction
{
  llvm::Function* internal;
  llvm::Function* exported;
  llvm::Function* imported;
};

struct NWContext
{
  Environment& env;
  Module& m;
  llvm::LLVMContext& context;
  llvm::Module* llvm;
  llvm::IRBuilder<>& builder;
  llvm::TargetMachine* machine;
  llvm::IntegerType* intptrty;
  Stack<llvm::Value*> values; // Tracks the current value stack
  Stack<NWBlock> control; // Control flow stack
  varuint32 n_locals;
  llvm::AllocaInst** locals;
  varuint32 n_memory;
  llvm::GlobalVariable** linearmemory;
  varuint32 n_tables;
  llvm::GlobalVariable** tables;
  llvm::GlobalVariable** tabletypes;
  varuint32 n_globals;
  llvm::GlobalVariable** globals;
  varuint32 n_functions;
  NWFunction* functions;
  llvm::Function* init;
  llvm::Function* start;
  llvm::Function* memgrow;
};

llvm::Function* NW_Intrinsic_ToC(llvm::Function* f, struct NWContext& context);
llvm::Function* NW_Intrinsic_FromC(llvm::Function* f, struct NWContext& context);

struct NWIntrinsic
{
  const char* name;
  llvm::Function* (*gen)(llvm::Function* f, struct NWContext&);
  llvm::Function* fn;
};

static NWIntrinsic nw_intrinsics[] = {
  NWIntrinsic{ "_native_wasm_to_c", &NW_Intrinsic_ToC, nullptr },
  NWIntrinsic{ "_native_wasm_from_c", &NW_Intrinsic_FromC, nullptr } 
};

#endif