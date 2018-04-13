// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#pragma warning(push)
#pragma warning(disable:4146)
#define _SCL_SECURE_NO_WARNINGS
#include "util.h"
#include "stack.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#pragma warning(pop)

using llvm::Function;
using llvm::FunctionType;
using llvm::Type;
using llvm::APFloat;
using llvm::ConstantFP;
using llvm::APInt;
using llvm::ConstantInt;
using llvm::BasicBlock;

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

Type* GetType(varsint7 type, NWContext& context)
{
  switch(type)
  {
  case TE_i32: return Type::getInt32Ty(context.context);
  case TE_i64: return Type::getInt64Ty(context.context);
  case TE_f32: return Type::getFloatTy(context.context);
  case TE_f64: return Type::getDoubleTy(context.context);
  case TE_void: return Type::getVoidTy(context.context);
  case TE_anyfunc: return llvm::FunctionType::get(Type::getVoidTy(context.context), false); // placeholder (*void)() function pointer
  }

  assert(false);
  return 0;
}

Export* FindExport(varuint32 index, Module& m)
{
  for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
    if(m.exportsection.exports[i].kind == KIND_FUNCTION && m.exportsection.exports[i].index == index)
      return m.exportsection.exports + i;
  return 0;
}

Function* CompileFunction(FunctionSig& signature, varuint32 index, NWContext& context)
{
  Type* ret = Type::getVoidTy(context.context);
  
  if(signature.n_returns > 1)
    return 0;
  if(signature.n_returns > 0)
    ret = GetType(signature.returns[0], context);

  FunctionType* ft = FunctionType::get(ret, false);
  
  if(signature.n_params > 0)
  {
    std::vector<Type*> args;
    for(varsint32 i = 0; i < signature.n_params; ++i)
      args.push_back(GetType(signature.params[i], context));

    ft = FunctionType::get(ret, args, false);
  }

  Function* fn = Function::Create(ft, Function::ExternalWeakLinkage, "", context.llvm);
  fn->setCallingConv(llvm::CallingConv::Fast);
  return fn;
}

ERROR_CODE PushReturn(NWContext& context) { return ERR_SUCCESS; }

// Given a set of returns in the order given in the function/instruction signature, pushes them on to the stack in reverse order
template<typename Arg, typename... Args>
ERROR_CODE PushReturn(NWContext& context, Arg arg, Args... args)
{
  ERROR_CODE e = PushReturn(context, args...);
  context.values.Push(arg);
  return e;
}

bool CheckType(TYPE_ENCODING Ty, llvm::Value* v)
{
  llvm::Type* t = v->getType();
  switch(Ty)
  {
  case TE_i32:
    return t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 32;
  case TE_i64:
    return t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 64;
  case TE_f32:
    return t->isFloatTy();
  case TE_f64:
    return t->isDoubleTy();
  case TE_void:
    return t->isVoidTy();
  }

  return true;
}

// Given a function pointer to the appropriate builder function, pops two binary arguments off the stack and pushes the result
template<TYPE_ENCODING Ty1, TYPE_ENCODING Ty2, TYPE_ENCODING TyR, typename... Args>
ERROR_CODE CompileBinaryOp(NWContext& context, llvm::Value* (llvm::IRBuilder<>::*op)(llvm::Value*, llvm::Value*, Args...), Args... args)
{
  if(context.values.Size() < 2)
    return ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvm::Value* val2 = context.values.Pop();
  llvm::Value* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1) || !CheckType(Ty2, val2))
    return ERR_INVALID_TYPE;

  return PushReturn(context, (context.builder.*op)(val1, val2, args...));
}

// Given an intrinsic function ID, pops two binary arguments off the stack and pushes the result, converting it to a Value*
template<TYPE_ENCODING Ty1, TYPE_ENCODING Ty2, TYPE_ENCODING TyR>
ERROR_CODE CompileBinaryIntrinsic(NWContext& context, llvm::Intrinsic::ID id, const llvm::Twine& name)
{
  if(context.values.Size() < 2)
    return ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvm::Value* val2 = context.values.Pop();
  llvm::Value* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1) || !CheckType(Ty2, val2))
    return ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateBinaryIntrinsic(id, val1, val2, name));
}

// Given a function pointer to the appropriate builder function, pops one unary argument off the stack and pushes the result
template<TYPE_ENCODING Ty1, TYPE_ENCODING TyR, typename... Args>
ERROR_CODE CompileUnaryOp(NWContext& context, llvm::Value* (llvm::IRBuilder<>::*op)(llvm::Value*, Args...), Args... args)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;

  llvm::Value* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1))
    return ERR_INVALID_TYPE;

  return PushReturn(context, (context.builder.*op)(val1, args...));
}

// Given an intrinsic function ID, pops one unary argument off the stack and pushes the result, converting it to a Value*
template<TYPE_ENCODING Ty1, TYPE_ENCODING TyR>
ERROR_CODE CompileUnaryIntrinsic(NWContext& context, llvm::Intrinsic::ID id, const llvm::Twine& name)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;

  llvm::Value* val1 = context.values.Pop();
  Function *fn = llvm::Intrinsic::getDeclaration(context.llvm, id, { val1->getType() });

  if(!CheckType(Ty1, val1))
    return ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateCall(fn, { val1 }, name, nullptr));
}

ERROR_CODE CompileSelectOp(NWContext& context, const llvm::Twine& name, llvm::Instruction* from)
{
  if(context.values.Size() < 3)
    return ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvm::Value* cond = context.values.Pop();
  llvm::Value* valf = context.values.Pop();
  llvm::Value* valt = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateSelect(cond, valt, valf, name, from));
}

template<TYPE_ENCODING Ty, bool LEFT>
ERROR_CODE CompileRotationOp(NWContext& context, const char* name)
{
  if(context.values.Size() < 2)
    return ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvm::Value* count = context.values.Pop();
  llvm::Value* value = context.values.Pop();

  if(!CheckType(Ty, value) || !CheckType(Ty, count))
    return ERR_INVALID_TYPE;

  const int BITS = (Ty == TE_i32) ? 32 : 64;
  llvm::Value *l, *r;

  if(LEFT) // value<<count | value>>(BITS-count);
  {
    l = context.builder.CreateShl(value, count);
    r = context.builder.CreateLShr(value, context.builder.CreateSub(ConstantInt::get(context.context, APInt(BITS, BITS, true)), count));
  }
  else // value>>count | value<<(BITS-count);
  {
    l = context.builder.CreateLShr(value, count);
    r = context.builder.CreateShl(value, context.builder.CreateSub(ConstantInt::get(context.context, APInt(BITS, BITS, true)), count));
  }

  return PushReturn(context, context.builder.CreateOr(l, r, name));
}

BasicBlock* PushLabel(const char* name, varsint7 sig, varuint7 opcode, NWContext& context)
{
  BasicBlock* bb = BasicBlock::Create(context.context, name);
  context.control.Push({ bb, context.values.Limit(), sig, opcode });
  context.values.SetLimit(context.values.Size() + context.values.Limit()); // Set limit to current stack size to prevent a block from popping past this
  return bb;
}

void BindLabel(BasicBlock* block, NWContext& context)
{
  if(!block->getParent())
  {
    context.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(block);
    context.builder.SetInsertPoint(block);
  }
  assert(block->getParent() != 0);
}

void PushResult(NWBlockResult** root, llvm::Value* result, BasicBlock* block)
{
  NWBlockResult* next = *root;
  *root = new NWBlockResult{ result, block, next };
}

// Adds current value stack to target branch according to that branch's signature, but doesn't pop them.
ERROR_CODE AddBranch(NWBlock& target, NWContext& context)
{
  if(target.sig != TE_void)
  {
    if(context.values.Size() != 1 || !CheckType(TYPE_ENCODING(target.sig), context.values.Peek())) // Verify stack
      return ERR_INVALID_VALUE_STACK;
    PushResult(&target.results, context.values.Peek(), context.builder.GetInsertBlock()); // Push result
  } else if(context.values.Size() > 0)
    return ERR_INVALID_VALUE_STACK;

  return ERR_SUCCESS;
}

// Pops a label off the control stack, verifying that the value stack matches the signature and building PHI nodes as necessary
ERROR_CODE PopLabel(NWContext& context)
{
  varsint7 sig = context.control.Peek().sig;
  if(sig != TE_void)
  {
    if(context.values.Size() != 1 || !CheckType(TYPE_ENCODING(sig), context.values.Peek())) // Verify stack
      return ERR_INVALID_VALUE_STACK;
    if(context.control.Peek().results != nullptr) // If there are results from other branches, perform a PHI merge. Otherwise, leave the value stack alone
    {
      unsigned int count = 1; // Start with 1 for our current branch's values 
      for(auto i = context.control.Peek().results; i != nullptr; i = i->next)
        ++count; // Count number of additional results

      llvm::PHINode* phi = context.builder.CreatePHI(context.values.Peek()->getType(), count, "phi"); // TODO: Account for multiple return values once they are added
      phi->addIncoming(context.values.Pop(), context.builder.GetInsertBlock()); // Pop this branches values off value stack, add using proper insert block

      for(auto i = context.control.Peek().results; i != nullptr; i = i->next)
        phi->addIncoming(i->v, i->b);

      if(context.values.Size() > 0) // All values should have been popped off value stack by now
        return ERR_INVALID_VALUE_STACK;

      context.values.Push(phi); // Push phi nodes on to stack
    }
  }
  else if(context.values.Size() > 0 || context.control.Peek().results != nullptr)
    return ERR_INVALID_VALUE_STACK;

  context.values.SetLimit(context.control.Peek().limit);
  context.control.Pop();

  return ERR_SUCCESS;
}

ERROR_CODE CompileIfBlock(varsint7 sig, NWContext& context)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;

  llvm::Value* cond = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return ERR_INVALID_TYPE;

  llvm::Value* cmp = context.builder.CreateICmpNE(cond, ConstantInt::get(context.builder.getInt32Ty(), 0), "if_cond");

  Function* parent = context.builder.GetInsertBlock()->getParent();
  BasicBlock* tblock = BasicBlock::Create(context.context, "if_true", parent);
  BasicBlock* fblock = PushLabel("if_false", sig, OP_if, context);

  context.builder.CreateCondBr(cmp, tblock, fblock); // Insert branch in current block
  context.builder.SetInsertPoint(tblock); // Start inserting code into true block

  return ERR_SUCCESS;
}

ERROR_CODE CompileElseBlock(NWContext& context)
{
  if(context.control.Size() == 0 || context.control.Peek().type != OP_if)
    return ERR_IF_ELSE_MISMATCH;

  // Instead of popping and pushing a new control label, we just re-purpose the existing one. This preserves the value stack results.
  AddBranch(context.control.Peek(), context); // Add the true block's results to itself
  while(context.values.Size()) // Reset value stack
    context.values.Pop();

  BasicBlock* fblock = context.control.Peek().block; // Get stored false block
  context.control.Peek().block = BasicBlock::Create(context.context, "if_merge"); // Store the if_merge block for use later
  context.control.Peek().type = OP_else; // make this block an OP_else block
  
  context.builder.CreateBr(context.control.Peek().block); // Add a branch-to-merge instruction to our if_true block
  BindLabel(fblock, context); // Bind if_false block to current position

  return ERR_SUCCESS;
}

ERROR_CODE CompileReturn(NWContext& context)
{
  varsint7 sig = context.control[context.control.Size()].sig;

  if(sig == TE_void)
  {
    if(context.values.Size() > 0)
      return ERR_INVALID_VALUE_STACK;
    context.builder.CreateRetVoid();
  }
  else
  {
    if(context.values.Size() != 1)
      return ERR_INVALID_VALUE_STACK;
    if(!CheckType(TYPE_ENCODING(sig), context.values.Peek()))
      return ERR_INVALID_TYPE;
    context.builder.CreateRet(context.values.Peek());
  }

  return ERR_SUCCESS;
}

ERROR_CODE CompileEndBlock(NWContext& context)
{
  if(context.control.Size() == 0)
  {
    if(context.control.Limit() > 0)
    {
      context.control.SetLimit(0);
      if(context.control.Peek().type != OP_return)
        return ERR_END_MISMATCH;

      ERROR_CODE err = PopLabel(context);
      return (err < 0) ? err : CompileReturn(context);
    }
    return ERR_END_MISMATCH;
  }
  context.builder.CreateBr(context.control.Peek().block); // Branch into next block
  BindLabel(context.control.Peek().block, context);

  switch(context.control.Peek().type) // Verify source operation
  {
  case OP_if:
    if(context.control.Peek().sig != TE_void) // An if statement with no else statement cannot return a value
      return ERR_EXPECTED_ELSE_INSTRUCTION;
  case OP_else:
  case OP_block:
  case OP_loop:
    break;
  default:
    return ERR_END_MISMATCH;
  }

  return PopLabel(context);
}

ERROR_CODE CompileTrap(NWContext& context)
{
  return PushReturn(context, context.builder.CreateCall(llvm::Intrinsic::getDeclaration(context.llvm, llvm::Intrinsic::trap), { }, "trap", nullptr));
}

ERROR_CODE CompileBranch(varuint32 depth, NWContext& context)
{
  if(depth >= context.control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  NWBlock& target = context.control[depth];
  context.builder.CreateBr(target.block);
  return (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}

ERROR_CODE CompileIfBranch(varuint32 depth, NWContext& context)
{
  if(depth >= context.control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;
  llvm::Value* cond = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return ERR_INVALID_TYPE;

  llvm::Value* cmp = context.builder.CreateICmpNE(cond, ConstantInt::get(context.builder.getInt32Ty(), 0), "br_if_cond");

  // Because llvm requires explicit branches, we have to create a new block and append it to our current one
  BasicBlock* block = BasicBlock::Create(context.context, "br_if_cont", context.builder.GetInsertBlock()->getParent());

  NWBlock& target = context.control[depth];
  context.builder.CreateCondBr(cmp, target.block, block);
  context.builder.SetInsertPoint(block); // Start inserting code into continuation.
  return (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}
ERROR_CODE CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def, NWContext& context)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;
  llvm::Value* index = context.values.Pop();

  if(!CheckType(TE_i32, index))
    return ERR_INVALID_TYPE;

  if(def >= context.control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  llvm::SwitchInst* s = context.builder.CreateSwitch(index, context.control[def].block, n_table);
  ERROR_CODE err = (context.control[def].type != OP_loop) ? AddBranch(context.control[def], context) : ERR_SUCCESS;

  for(varuint32 i = 0; i < n_table && err == ERR_SUCCESS; ++i)
  {
    if(table[i] >= context.control.Size())
      return ERR_INVALID_BRANCH_DEPTH;

    NWBlock& target = context.control[table[i]];
    s->addCase(ConstantInt::get(context.context, APInt(32, i, true)), target.block);
    err = (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS;
  }
  
  return err; // TODO: determine if another block is needed
}

bool CompareTypes(llvm::Type* a, llvm::Type* b)
{
  if(a->isFloatTy())
    return b->isFloatTy();
  if(a->isDoubleTy())
    return b->isDoubleTy();
  if(a->isVoidTy())
    return b->isVoidTy();
  if(a->isIntegerTy())
    return b->isIntegerTy() && static_cast<llvm::IntegerType*>(a)->getBitWidth() == static_cast<llvm::IntegerType*>(b)->getBitWidth();
  return false;
}

ERROR_CODE CompileCall(varuint32 index, NWContext& context)
{
  if(index >= context.n_functions)
    return ERR_INVALID_FUNCTION_INDEX;
  
  llvm::Function* fn = context.functions[index];
  unsigned int num = fn->getNumOperands();
  if(num >= context.values.Size())
    return ERR_INVALID_VALUE_STACK;

  // Pop arguments in reverse order
  llvm::Value** ArgsV = tmalloc<llvm::Value*>(num);
  for(unsigned int i = num; i-- > 0;)
  {
    ArgsV[i] = context.values.Pop();
    if(!CompareTypes(ArgsV[i]->getType(), fn->getOperand(i)->getType()));
      return ERR_INVALID_ARGUMENT_TYPE;
  }

  llvm::CallInst* call = context.builder.CreateCall(fn, llvm::makeArrayRef(ArgsV, num), "call");
  if(context.env.flags & ENV_STRICT) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(llvm::CallInst::TCK_NoTail);
  call->setCallingConv(fn->getCallingConv());
  
  return PushReturn(context, call);
}

ERROR_CODE CompileIndirectCall(varuint32 sigindex, NWContext& context)
{
  if(sigindex >= context.m.type.n_functions)
    return ERR_INVALID_TYPE_INDEX;

  FunctionSig& sig = context.m.type.functions[sigindex];
  
  if(sig.n_params + 1 >= context.values.Size())
    return ERR_INVALID_VALUE_STACK;

  llvm::Value* callee = context.values.Pop();

  if(context.n_tables < 1)
    return ERR_INVALID_TABLE_INDEX;

  // Pop arguments in reverse order
  llvm::Value** ArgsV = tmalloc<llvm::Value*>(sig.n_params);
  for(unsigned int i = sig.n_params; i-- > 0;)
  {
    ArgsV[i] = context.values.Pop();
    if(!CompareTypes(ArgsV[i]->getType(), GetType(sig.params[i], context)));
      return ERR_INVALID_ARGUMENT_TYPE;
  }

  llvm::CallInst* call = context.builder.CreateCall(context.builder.CreateGEP(context.tables[0], callee), llvm::makeArrayRef(ArgsV, sig.n_params), "call");
  if(context.env.flags & ENV_STRICT) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(llvm::CallInst::TCK_NoTail);
  call->setCallingConv(llvm::CallingConv::Fast); // TODO: figure out import calling convention

  return PushReturn(context, call);
}

std::pair<ERROR_CODE, llvm::Constant*> CompileConstant(Instruction& ins, NWContext& context)
{
  switch(ins.opcode)
  {
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
    return { ERR_SUCCESS, ConstantInt::get(context.context, APInt(32, ins.immediates[0]._varuint32, true)) };
  case OP_i64_const:
    return { ERR_SUCCESS, ConstantInt::get(context.context, APInt(64, ins.immediates[0]._varuint64, true)) };
  case OP_f32_const:
    return { ERR_SUCCESS, ConstantFP::get(context.context, APFloat(ins.immediates[0]._float32)) };
  case OP_f64_const:
    return { ERR_SUCCESS, ConstantFP::get(context.context, APFloat(ins.immediates[0]._float64)) };
  case OP_get_global:
    if(ins.immediates[0]._varuint32 >= context.n_globals)
      return { ERR_INVALID_GLOBAL_INDEX, 0 };
    return { ERR_SUCCESS, context.globals[ins.immediates[0]._varuint32] };
  }

  return { ERR_INVALID_INITIALIZER, 0 };
}

llvm::Value* GetMemPointer(NWContext& context, llvm::Value* base, llvm::PointerType* ptr, varuint7 memory, varuint32 offset)
{
  // TODO: trap if invalid linear access
  llvm::Value* loc = context.builder.CreateAdd(base, ConstantInt::get(context.context, APInt(32, offset, true)), "", true, true);
  return context.builder.CreatePointerCast(context.builder.CreateGEP(context.linearmemory[memory], loc), ptr);
}

template<bool SIGNED>
ERROR_CODE CompileLoad(NWContext& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::Type* Ext, llvm::Type* Ty)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;
  if(context.n_memory < 1)
    return ERR_INVALID_MEMORY_INDEX;

  llvm::Value* base = context.values.Pop();

  if(!CheckType(TE_i32, base))
    return ERR_INVALID_TYPE;

  llvm::Value* result = context.builder.CreateAlignedLoad(GetMemPointer(context, base, Ty->getPointerTo(0), memory, offset), (1 << memflags), name);

  if(Ext != nullptr)
    result = SIGNED ? context.builder.CreateSExt(result, Ext) : context.builder.CreateZExt(result, Ext);

  return PushReturn(context, result);
}

template<TYPE_ENCODING Ty>
ERROR_CODE CompileStore(NWContext& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::IntegerType* Ext)
{
  if(context.values.Size() < 2)
    return ERR_INVALID_VALUE_STACK;
  if(context.n_memory < 1)
    return ERR_INVALID_MEMORY_INDEX;

  llvm::Value* value = context.values.Pop();
  llvm::Value* base = context.values.Pop();

  if(!CheckType(Ty, value))
    return ERR_INVALID_TYPE;
  if(!CheckType(TE_i32, base))
    return ERR_INVALID_TYPE;
  llvm::Type* PtrType = !Ext ? GetType(Ty, context) : Ext;

  // TODO: trap if invalid linear access
  llvm::Value* ptr = GetMemPointer(context, base, PtrType->getPointerTo(0), memory, offset);
  llvm::Value* result = context.builder.CreateAlignedStore(!Ext ? value : context.builder.CreateIntCast(value, Ext, false), ptr, (1 << memflags), name);

  return ERR_SUCCESS;
}

ERROR_CODE CompileMemGrow(NWContext& context, const char* name)
{
  if(context.values.Size() < 1)
    return ERR_INVALID_VALUE_STACK;
  if(context.n_memory < 1)
    return ERR_INVALID_MEMORY_INDEX;

  llvm::Value* delta = context.values.Pop();
  llvm::Value* old = context.builder.CreateLShr(context.builder.CreateIntCast(context.builder.CreateCall(context.memsize, { context.linearmemory[0] }), context.builder.getInt32Ty(), true), 16);

  if(!CheckType(TE_i32, delta))
    return ERR_INVALID_TYPE;

  llvm::CallInst* call = context.builder.CreateCall(context.memgrow, { context.linearmemory[0], context.builder.CreateShl(context.builder.CreateZExt(delta, context.builder.getInt64Ty()), 16) }, name);
  context.builder.CreateStore(call, context.linearmemory[0]);

  return PushReturn(context, old);
}

ERROR_CODE CompileInstruction(Instruction& ins, NWContext& context)
{
  switch(ins.opcode)
  {
  case OP_unreachable:
    context.builder.CreateUnreachable();
    return CompileTrap(context);
  case OP_nop:
    return ERR_SUCCESS;
  case OP_block:
    PushLabel("block", ins.immediates[0]._varsint7, OP_block, context);
    return ERR_SUCCESS;
  case OP_loop:
    PushLabel("loop", ins.immediates[0]._varsint7, OP_loop, context);
    context.builder.CreateBr(context.control.Peek().block); // Branch into next block
    BindLabel(context.control.Peek().block, context);
    return ERR_SUCCESS;
  case OP_if:
    return CompileIfBlock(ins.immediates[0]._varsint7, context);
  case OP_else:
    return CompileElseBlock(context);
  case OP_end:
    return CompileEndBlock(context);
  case OP_br:
    return CompileBranch(ins.immediates[0]._varuint32, context);
  case OP_br_if:
    return CompileIfBranch(ins.immediates[0]._varuint32, context);
  case OP_br_table:
    return CompileBranchTable(ins.immediates[0].n_table, ins.immediates[0].table, ins.immediates[1]._varuint32, context);
  case OP_return:
    return CompileReturn(context);

    // Call operators
  case OP_call:
    return CompileCall(ins.immediates[0]._varuint32, context);
  case OP_call_indirect:
    return CompileIndirectCall(ins.immediates[0]._varuint32, context);

    // Parametric operators
  case OP_drop:
    context.values.Pop(); // We do not delete the value because it could be referenced elsewhere (e.g. in a branch)
    return ERR_SUCCESS;
  case OP_select:
    return CompileSelectOp(context, OPNAMES[ins.opcode], nullptr);

    // Variable access
  case OP_get_local:
    if(ins.immediates[0]._varuint32 >= context.n_locals)
      return ERR_INVALID_LOCAL_INDEX;
    context.values.Push(context.builder.CreateLoad(context.locals[ins.immediates[0]._varuint32]));
    return ERR_SUCCESS;
  case OP_set_local:
  case OP_tee_local:
    if(ins.immediates[0]._varuint32 >= context.n_locals)
      return ERR_INVALID_LOCAL_INDEX;
    if(context.values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    context.builder.CreateStore(context.values.Peek(), context.locals[ins.immediates[0]._varuint32]);
    if(ins.opcode == OP_set_local) // tee_local is the same as set_local except the operand isn't popped
      context.values.Pop();
    return ERR_SUCCESS;
  case OP_get_global:
  {
    auto pair = CompileConstant(ins, context);
    if(pair.first >= 0)
      context.values.Push(pair.second);
    return pair.first;
  }
  case OP_set_global:
    if(ins.immediates[0]._varuint32 >= context.n_globals)
      return ERR_INVALID_GLOBAL_INDEX;
    if(context.values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    context.builder.CreateStore(context.values.Pop(), context.globals[ins.immediates[0]._varuint32]);
    return ERR_SUCCESS;

    // Memory-related operators
  case OP_i32_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr, context.builder.getInt32Ty());
  case OP_i64_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr, context.builder.getInt64Ty());
  case OP_f32_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr, context.builder.getFloatTy());
  case OP_f64_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr, context.builder.getDoubleTy());
  case OP_i32_load8_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt8Ty());
  case OP_i32_load8_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt8Ty());
  case OP_i32_load16_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt16Ty());
  case OP_i32_load16_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt16Ty());
  case OP_i64_load8_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt8Ty());
  case OP_i64_load8_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt8Ty());
  case OP_i64_load16_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt16Ty());
  case OP_i64_load16_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt16Ty());
  case OP_i64_load32_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt32Ty());
  case OP_i64_load32_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt32Ty());
  case OP_i32_store:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr);
  case OP_i64_store:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr);
  case OP_f32_store:
    return CompileStore<TE_f32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr);
  case OP_f64_store:
    return CompileStore<TE_f64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], nullptr);
  case OP_i32_store8:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt8Ty());
  case OP_i32_store16:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt16Ty());
  case OP_i64_store8:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt8Ty());
  case OP_i64_store16:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt16Ty());
  case OP_i64_store32:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._memflags, OPNAMES[ins.opcode], context.builder.getInt32Ty());
  case OP_current_memory:
    return PushReturn(context, context.builder.CreateLShr(context.builder.CreateIntCast(context.builder.CreateCall(context.memsize, { context.linearmemory[0] }, OPNAMES[ins.opcode]), context.builder.getInt32Ty(), true), 16));
  case OP_grow_memory:
    return CompileMemGrow(context, OPNAMES[ins.opcode]);

    // Constants
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
  case OP_i64_const:
  case OP_f32_const:
  case OP_f64_const:
  {
    auto pair = CompileConstant(ins, context);
    if(pair.first >= 0)
      context.values.Push(pair.second);
    return pair.first;
  }

    // Comparison operators
  case OP_i32_eqz:
    context.values.Push(ConstantInt::get(context.context, APInt(32, 0, true))); // Fallthrough to OP_i32_eq
  case OP_i32_eq:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpEQ, OPNAMES[ins.opcode]);
  case OP_i32_ne:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpNE, OPNAMES[ins.opcode]);
  case OP_i32_lt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLT, OPNAMES[ins.opcode]);
  case OP_i32_lt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpULT, OPNAMES[ins.opcode]);
  case OP_i32_gt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGT, OPNAMES[ins.opcode]);
  case OP_i32_gt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGT, OPNAMES[ins.opcode]);
  case OP_i32_le_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLE, OPNAMES[ins.opcode]);
  case OP_i32_le_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpULE, OPNAMES[ins.opcode]);
  case OP_i32_ge_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGE, OPNAMES[ins.opcode]);
  case OP_i32_ge_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGE, OPNAMES[ins.opcode]);
  case OP_i64_eqz:
    context.values.Push(ConstantInt::get(context.context, APInt(64, 0, true))); // Fallthrough to OP_i64_eq
  case OP_i64_eq:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpEQ, OPNAMES[ins.opcode]);
  case OP_i64_ne:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpNE, OPNAMES[ins.opcode]);
  case OP_i64_lt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLT, OPNAMES[ins.opcode]);
  case OP_i64_lt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpULT, OPNAMES[ins.opcode]);
  case OP_i64_gt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGT, OPNAMES[ins.opcode]);
  case OP_i64_gt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGT, OPNAMES[ins.opcode]);
  case OP_i64_le_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLE, OPNAMES[ins.opcode]);
  case OP_i64_le_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpULE, OPNAMES[ins.opcode]);
  case OP_i64_ge_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGE, OPNAMES[ins.opcode]);
  case OP_i64_ge_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGE, OPNAMES[ins.opcode]);
  case OP_f32_eq:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOEQ, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ne:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpUNE, OPNAMES[ins.opcode], nullptr);
  case OP_f32_lt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLT, OPNAMES[ins.opcode], nullptr);
  case OP_f32_gt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGT, OPNAMES[ins.opcode], nullptr);
  case OP_f32_le:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLE, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ge:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_eq:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOEQ, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ne:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpUNE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_lt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLT, OPNAMES[ins.opcode], nullptr);
  case OP_f64_gt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGT, OPNAMES[ins.opcode], nullptr);
  case OP_f64_le:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ge:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGE, OPNAMES[ins.opcode], nullptr);

    // Numeric operators
  case OP_i32_clz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::ctlz, OPNAMES[ins.opcode]);
  case OP_i32_ctz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::cttz, OPNAMES[ins.opcode]);
  case OP_i32_popcnt:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::ctpop, OPNAMES[ins.opcode]);
  case OP_i32_add:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateAdd, OPNAMES[ins.opcode], false, false);
  case OP_i32_sub:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateSub, OPNAMES[ins.opcode], false, false);
  case OP_i32_mul:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateMul, OPNAMES[ins.opcode], false, false);
  case OP_i32_div_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateSDiv, OPNAMES[ins.opcode], false);
  case OP_i32_div_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateUDiv, OPNAMES[ins.opcode], false);
  case OP_i32_rem_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSRem, OPNAMES[ins.opcode]);
  case OP_i32_rem_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i32_and:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i32_or:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i32_xor:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i32_shl:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i32_shr_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i32_shr_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
  case OP_i32_rotl:
    return CompileRotationOp<TE_i32, true>(context, OPNAMES[ins.opcode]);
  case OP_i32_rotr:
    return CompileRotationOp<TE_i32, false>(context, OPNAMES[ins.opcode]);
  case OP_i64_clz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(context, llvm::Intrinsic::ctlz, OPNAMES[ins.opcode]);
  case OP_i64_ctz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(context, llvm::Intrinsic::cttz, OPNAMES[ins.opcode]);
  case OP_i64_popcnt:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(context, llvm::Intrinsic::ctpop, OPNAMES[ins.opcode]);
  case OP_i64_add:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateAdd, OPNAMES[ins.opcode], false, false);
  case OP_i64_sub:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateSub, OPNAMES[ins.opcode], false, false);
  case OP_i64_mul:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateMul, OPNAMES[ins.opcode], false, false);
  case OP_i64_div_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateSDiv, OPNAMES[ins.opcode], false);
  case OP_i64_div_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateUDiv, OPNAMES[ins.opcode], false);
  case OP_i64_rem_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSRem, OPNAMES[ins.opcode]);
  case OP_i64_rem_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i64_and:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i64_or:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i64_xor:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i64_shl:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i64_shr_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i64_shr_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
  case OP_i64_rotl:
    return CompileRotationOp<TE_i64, true>(context, OPNAMES[ins.opcode]);
  case OP_i64_rotr:
    return CompileRotationOp<TE_i64, false>(context, OPNAMES[ins.opcode]);
  case OP_f32_abs:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f32_neg:
    return CompileUnaryOp<TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFNeg, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ceil:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f32_floor:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f32_trunc:
    return CompileUnaryOp<TE_f32, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_nearest: // We must round floats using IEEE 754-2008 roundToIntegralTowardZero, which rint gaurantees
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::rint, OPNAMES[ins.opcode]);
  case OP_f32_sqrt:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f32_add:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], false);
  case OP_f32_sub:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], false);
  case OP_f32_mul:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], false);
  case OP_f32_div:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], false);
  case OP_f32_min:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f32_max:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f32_copysign:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);
  case OP_f64_abs:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f64_neg:
    return CompileUnaryOp<TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFNeg, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ceil:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f64_floor:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f64_trunc:
    return CompileUnaryOp<TE_f64, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_nearest:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::rint, OPNAMES[ins.opcode]);
  case OP_f64_sqrt:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f64_add:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], false);
  case OP_f64_sub:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], false);
  case OP_f64_mul:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], false);
  case OP_f64_div:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], false);
  case OP_f64_min:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f64_max:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f64_copysign:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);

    // Conversions
  case OP_i32_wrap_i64:
    return CompileUnaryOp<TE_i64, TE_i32, llvm::Type*, bool, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt32Ty(), true, OPNAMES[ins.opcode]);
  case OP_i32_trunc_s_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_u_f32:
    return CompileUnaryOp<TE_f32, TE_i32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_s_f64:
    return CompileUnaryOp<TE_f64, TE_i32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_u_f64:
    return CompileUnaryOp<TE_f64, TE_i32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_extend_s_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvm::Type*, bool, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), true, OPNAMES[ins.opcode]);
  case OP_i64_extend_u_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvm::Type*, bool, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), false, OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f64:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f64, TE_i64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f64:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f64, TE_i64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_convert_s_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_u_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_s_i64:
    return CompileUnaryOp<TE_i64, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_u_i64:
    return CompileUnaryOp<TE_i64, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_demote_f64:
    return CompileUnaryOp<TE_f64, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_s_i32:
    return CompileUnaryOp<TE_i32, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_u_i32:
    return CompileUnaryOp<TE_i32, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_s_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_u_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_promote_f32:
    return CompileUnaryOp<TE_f32, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateFPExt, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);

    // Reinterpretations
  case OP_i32_reinterpret_f32:
    return CompileUnaryOp<TE_f32, TE_i32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_reinterpret_f64:
    return CompileUnaryOp<TE_f64, TE_i64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_reinterpret_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_reinterpret_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvm::Type*, const llvm::Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  default:
    return ERR_FATAL_UNKNOWN_INSTRUCTION;
  }

  assert(false); // ERROR NOT IMPLEMENTED
  return ERR_SUCCESS;
}

ERROR_CODE CompileFunctionBody(Function* fn, FunctionSig& sig, FunctionBody& body, NWContext& context)
{
  // Get return value
  varsint7 ret = TE_void;
  if(sig.n_returns > 0)
    ret = sig.returns[0];

  // Setup initial basic block.
  BindLabel(PushLabel("entry", ret, OP_return, context), context);
  context.control.SetLimit(1); // Don't allow breaking to the function entry
  context.n_locals = sig.n_params;

  // Count and allocate all parameters and local variables
  for(varuint32 i = 0; i < body.n_locals; ++i)
    context.n_locals += body.locals[i].count;

  context.locals = tmalloc<llvm::AllocaInst*>(context.n_locals);
  varuint32 index = 0;

  // We allocate parameters first, followed by local variables
  for(varuint32 i = 0; i < sig.n_params; ++i)
    context.locals[index++] = context.builder.CreateAlloca(GetType(sig.params[i], context));

  for(varuint32 i = 0; i < body.n_locals; ++i)
    for(varuint32 j = 0; j < body.locals[i].count; ++j)
      context.locals[index++] = context.builder.CreateAlloca(GetType(body.locals[i].type, context));

  // Begin iterating through the instructions until there aren't any left
  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    ERROR_CODE err = CompileInstruction(body.body[i], context);
    if(err != ERR_SUCCESS)
      return err;
  }

  if(body.body[body.n_body - 1].opcode != OP_end)
    return ERR_FATAL_EXPECTED_END_INSTRUCTION;
  if(context.control.Size() > 0 || context.control.Limit() > 0)
    return ERR_INVALID_VALUE_STACK;
  return ERR_SUCCESS;
}

const char* GlobalName(varuint32 i)
{
  static char buf[30] = { 0 };
  _itoa_s(i, buf, 10); // TODO: make cross platform
  return buf;
}

llvm::GlobalVariable* CreateGlobal(NWContext& context, llvm::Type* ty, bool isconst, bool external, llvm::Constant* init = 0)
{
  return new llvm::GlobalVariable(
    *context.llvm,
    ty,
    isconst,
    external ? llvm::GlobalValue::LinkageTypes::ExternalLinkage : llvm::GlobalValue::LinkageTypes::InternalLinkage,
    init,
    "",
    nullptr,
    llvm::GlobalValue::NotThreadLocal,
    0,
    external);
}

llvm::Constant* CompileInitConstant(Instruction& ins, NWContext& context)
{
  auto pair = CompileConstant(ins, context);
  return pair.first < 0 ? nullptr : pair.second;
}
uint64_t GetTotalSize(llvm::Type* t)
{
  return t->getArrayNumElements() * (t->getScalarSizeInBits() / 8);
}

ERROR_CODE CompileModule(NWContext& context)
{
  context.llvm = new llvm::Module((const char*)context.m.name.bytes, context.context);

  // Define a unique init function for performing module initialization
  context.init = Function::Create(FunctionType::get(context.builder.getVoidTy(), false), Function::ExternalLinkage, "_native_wasm_internal_init", context.llvm);
  BasicBlock* initblock = BasicBlock::Create(context.context, "entry", context.init);
  context.builder.SetInsertPoint(initblock);
  
  // Declare C runtime function prototypes that we assume exist on the system
  context.memsize = Function::Create(FunctionType::get(context.builder.getInt64Ty(), { context.builder.getInt8PtrTy(0) }, false), Function::ExternalLinkage, "_native_wasm_internal_env_memory_size", context.llvm);
  context.memgrow = Function::Create(FunctionType::get(context.builder.getInt8PtrTy(0), { context.builder.getInt8PtrTy(0), context.builder.getInt64Ty() }, false), Function::ExternalLinkage, "_native_wasm_internal_env_grow_memory", context.llvm);
  Function* fn_memcpy = Function::Create(FunctionType::get(context.builder.getInt8PtrTy(0), { context.builder.getInt8PtrTy(0), context.builder.getInt8PtrTy(0), context.builder.getInt64Ty() }, false), Function::ExternalLinkage, "_native_wasm_internal_env_memcpy", context.llvm);

  context.functions = tmalloc<llvm::Function*>(context.m.importsection.functions + context.m.function.n_funcdecl);
  context.tables = tmalloc<llvm::GlobalVariable*>(context.m.importsection.tables - context.m.importsection.functions + context.m.table.n_tables);
  context.linearmemory = tmalloc<llvm::GlobalVariable*>(context.m.importsection.memory - context.m.importsection.tables + context.m.memory.n_memory);
  context.globals = tmalloc<llvm::GlobalVariable*>(context.m.importsection.globals - context.m.importsection.memory + context.m.global.n_globals);

  // Import function prototypes
  for(varuint32 i = 0; i < context.m.importsection.functions; ++i)
    context.functions[context.n_functions++] = CompileFunction(context.m.type.functions[context.m.importsection.imports[i].sig_index], i, context);

  // Import tables
  for(varuint32 i = context.m.importsection.functions; i < context.m.importsection.tables; ++i)
    context.tables[context.n_tables++] = CreateGlobal(context, GetType(context.m.importsection.imports[i].table_desc.element_type, context), false, true);

  // Import memory
  for(varuint32 i = context.m.importsection.tables; i < context.m.importsection.memory; ++i)
    context.linearmemory[context.n_memory++] = CreateGlobal(context, context.builder.getInt8PtrTy(0), false, true);

  // Import global variables
  for(varuint32 i = context.m.importsection.memory; i < context.m.importsection.globals; ++i)
  {
    context.globals[context.n_globals] = CreateGlobal(
      context, 
      GetType(context.m.importsection.imports[i].global_desc.type, context), 
      !context.m.importsection.imports[i].global_desc.mutability, 
      true);
  }

  // Cache internal function start index
  if(context.m.function.n_funcdecl != context.m.code.n_funcbody)
    return ERR_INVALID_FUNCTION_BODY;
  varuint32 code_index = context.n_functions;

  // Declare function prototypes
  for(varuint32 i = 0; i < context.m.function.n_funcdecl; ++i)
    context.functions[context.n_functions++] = CompileFunction(context.m.type.functions[context.m.function.funcdecl[i]], i, context);

  // Declare tables and allocate in init function
  for(varuint32 i = 0; i < context.m.table.n_tables; ++i)
  {
    context.tables[context.n_tables] = CreateGlobal(context, GetType(context.m.table.tables[i].element_type, context), false, true);
    unsigned int bytewidth = context.tables[context.n_tables]->getType()->getPrimitiveSizeInBits() / 8;
    if(!bytewidth)
      return ERR_INVALID_TABLE_TYPE;

    llvm::CallInst* call = context.builder.CreateCall(context.memgrow, { llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy(0)), ConstantInt::get(context.context, APInt(64, context.m.table.tables[i].resizable.minimum * bytewidth, true)) });
    context.builder.CreateStore(call, context.tables[context.n_tables]);
    ++context.n_tables;
  }

  // Declare linear memory spaces and allocate in init function
  for(varuint32 i = 0; i < context.m.memory.n_memory; ++i)
  {
    context.linearmemory[context.n_memory] = CreateGlobal(context, context.builder.getInt8PtrTy(0), false, false);
    llvm::CallInst* call = context.builder.CreateCall(context.memgrow, { llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy(0)), ConstantInt::get(context.context, APInt(64, context.m.memory.memory[i].limits.minimum, true)) });
    context.builder.CreateStore(call, context.linearmemory[context.n_memory]);
    ++context.n_memory;
  }

  // Declare global variables
  for(varuint32 i = 0; i < context.m.global.n_globals; ++i)
  {
    context.globals[i + context.m.importsection.globals] = CreateGlobal(
      context,
      GetType(context.m.global.globals[i].desc.type, context),
      !context.m.global.globals[i].desc.mutability,
      false,
      CompileInitConstant(context.m.global.globals[i].init, context));
  }

  // Process exports by modifying global variables or function definitions as needed
  for(varuint32 i = 0; i < context.m.exportsection.n_exports; ++i)
  {
    Export& e = context.m.exportsection.exports[i];
    switch(e.kind)
    {
    case KIND_FUNCTION:
      context.functions[e.index]->setLinkage(Function::ExternalLinkage);
      context.functions[e.index]->setName((const char*)e.name.bytes);
      context.functions[e.index]->setCallingConv(llvm::CallingConv::C);
      break;
    case KIND_TABLE:
      context.tables[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.tables[e.index]->setName((const char*)e.name.bytes);
      break;
    case KIND_MEMORY:
      context.linearmemory[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.linearmemory[e.index]->setName((const char*)e.name.bytes);
      break;
    case KIND_GLOBAL:
      context.globals[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.globals[e.index]->setName((const char*)e.name.bytes);
      break;
    }
  }

  // Process data section by appending to the init function
  for(varuint32 i = 0; i < context.m.data.n_data; ++i)
  {
    DataInit& d = context.m.data.data[i]; // First we declare a constant array that stores the data in the EXE
    llvm::Constant* val = llvm::ConstantDataArray::get(context.context, llvm::makeArrayRef<uint8_t>(d.data.bytes, d.data.bytes)); 
    llvm::Constant* offset = CompileInitConstant(d.offset, context);

    // Then we create a memcpy call that copies this data to the appropriate location in the init function
    context.builder.CreateCall(fn_memcpy, { context.builder.CreateGEP(context.linearmemory[d.index], offset), val, ConstantInt::get(context.context, APInt(64, GetTotalSize(val->getType()), false)) });
  }

  // Process element section by appending to the init function
  for(varuint32 i = 0; i < context.m.element.n_elements; ++i)
  {
    TableInit& e = context.m.element.elements[i]; // First we declare a constant array that stores the data in the EXE
    TableDesc* t = ModuleTable(context.m, e.index);
    if(!t)
      return ERR_INVALID_TABLE_INDEX;

    if(t->element_type == TE_anyfunc)
    {
      std::vector<llvm::Constant*> fnarray;

      // Go through and resolve all indices to function pointers
      for(varuint32 j = 0; j < e.n_elems; ++j)
      {
        if(e.elems[j] >= context.n_functions)
          return ERR_INVALID_FUNCTION_INDEX;
        fnarray.push_back(context.functions[e.elems[j]]);
      }

      llvm::Constant* val = llvm::ConstantArray::get(llvm::ArrayType::get(GetType(t->element_type, context), fnarray.size()), fnarray);
      llvm::Constant* offset = CompileInitConstant(e.offset, context);
      
      // Then we create a memcpy call that copies this data to the appropriate location in the init function
      context.builder.CreateCall(fn_memcpy, { context.builder.CreateGEP(context.tables[e.index], offset), val, ConstantInt::get(context.context, llvm::APInt(64, GetTotalSize(val->getType()), false)) });
    }
  }

  // Generate code for each function body
  for(varuint32 i = 0; i < context.m.code.n_funcbody; ++i)
  {
    Function* fn = context.functions[code_index++];
    ERROR_CODE err;

    if(fn)
    {
      if((err = CompileFunctionBody(fn, context.m.type.functions[context.m.function.funcdecl[i]], context.m.code.funcbody[i], context)) < 0)
        return err;
    }
  }

  // If the start section exists, lift the start function to the context so our environment knows about it.
  if(context.m.knownsections & SECTION_START)
  {
    if(context.m.start >= context.n_functions)
      return ERR_INVALID_START_FUNCTION;
    context.start = context.functions[context.m.start];
  }

  // Verify module
  llvm::verifyModule(*context.llvm);

  // Do optimization passes

  return ERR_SUCCESS;
}

ERROR_CODE CompileEnvironment(Environment* env)
{
  llvm::LLVMContext context;
  llvm::IRBuilder<> builder(context);
  Function* main = nullptr;
  Function* start = nullptr;
  ERROR_CODE err = ERR_SUCCESS;
  Function** init = tmalloc<Function*>(env->n_modules);

  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    NWContext nw = NWContext{ *env, env->modules[i], context, 0, builder };
    if((err = CompileModule(nw)) < 0)
      return err;
    
    // If module has a start function, create a main entry point function
    if(nw.start != nullptr)
    {
      if(main != nullptr)
        return ERR_MULTIPLE_ENTRY_POINTS;
      main = Function::Create(FunctionType::get(builder.getVoidTy(), false), Function::ExternalLinkage, "main", nw.llvm);
      start = nw.start;
      init[i] = nw.init;
    }
  }

  // Initialize all modules and call start function
  if(main != nullptr)
  {
    BasicBlock* initblock = BasicBlock::Create(context, "entry", main);
    builder.SetInsertPoint(initblock);
    for(varuint32 i = 0; i < env->n_modules; ++i)
      builder.CreateCall(init[i], {});
    builder.CreateCall(start, {});
  }

  return ERR_SUCCESS;
}