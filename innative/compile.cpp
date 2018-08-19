// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#pragma warning(push)
#pragma warning(disable:4146)
#define _SCL_SECURE_NO_WARNINGS
#include "util.h"
#include "optimize.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "lld/Common/Driver.h"
#include <iostream>
#pragma warning(pop)

using namespace innative;
using namespace utility;

using Func = llvm::Function;
using FuncTy = llvm::FunctionType;
using llvmTy = llvm::Type;
using llvmVal = llvm::Value;
using llvm::APFloat;
using llvm::ConstantFP;
using llvm::APInt;
using llvm::Twine;
using llvm::CallInst;
using CInt = llvm::ConstantInt;
using BB = llvm::BasicBlock;
using std::vector;
using std::string;

Func* fn_print;

llvmTy* GetType(varsint7 type, code::Context& context)
{
  switch(type)
  {
  case TE_i32: return llvmTy::getInt32Ty(context.context);
  case TE_i64: return llvmTy::getInt64Ty(context.context);
  case TE_f32: return llvmTy::getFloatTy(context.context);
  case TE_f64: return llvmTy::getDoubleTy(context.context);
  case TE_void: return llvmTy::getVoidTy(context.context);
  case TE_anyfunc: return FuncTy::get(llvmTy::getVoidTy(context.context), false)->getPointerTo(0); // placeholder (*void)() function pointer
  }

  assert(false);
  return 0;
}

Export* FindExport(varuint32 index, Module& m)
{
  for(varuint32 i = 0; i < m.exportsection.n_exports; ++i)
    if(m.exportsection.exports[i].kind == WASM_KIND_FUNCTION && m.exportsection.exports[i].index == index)
      return m.exportsection.exports + i;
  return 0;
}

FuncTy* GetFunctionType(FunctionSig& signature, code::Context& context)
{
  if(signature.n_returns > 1)
    return 0;
  llvmTy* ret = (signature.n_returns > 0) ? GetType(signature.returns[0], context) : llvmTy::getVoidTy(context.context);

  if(signature.n_params > 0)
  {
    vector<llvmTy*> args;
    for(varuint32 i = 0; i < signature.n_params; ++i)
      args.push_back(GetType(signature.params[i], context));

    return FuncTy::get(ret, args, false);
  }
  return FuncTy::get(ret, false);
}

Func* CompileFunction(FunctionSig& signature, const Twine& name, code::Context& context)
{
  Func* fn = Func::Create(GetFunctionType(signature, context), Func::InternalLinkage, name, context.llvm);
  fn->setCallingConv(llvm::CallingConv::Fast);
  return fn;
}

Func* HomogenizeFunction(Func* fn, const Twine& name, code::Context& context)
{
  vector<llvmTy*> types; // Replace the entire function with just i64
  for(auto& arg : fn->args())
    types.push_back(context.builder.getInt64Ty());

  Func* wrap = Func::Create(FuncTy::get(context.builder.getInt64Ty(), types, false), Func::ExternalLinkage, name, context.llvm);

  auto prev = context.builder.GetInsertBlock();

  BB* bb = BB::Create(context.context, "homogenize_block", wrap);
  context.builder.SetInsertPoint(bb);
  vector<llvmVal*> values;
  int i = 0;
  for(auto& arg : wrap->args())
  {
    llvmVal* v = nullptr;
    auto ty = fn->getFunctionType()->params()[i++];
    if(ty->isIntegerTy()) // Directly convert all ints from i64
      v = context.builder.CreateIntCast(&arg, ty, true);
    else if(ty->isFloatingPointTy()) // Bitcast from i64 to a double, then shrink to the appropriate size
      v = context.builder.CreateFPCast(context.builder.CreateBitCast(&arg, context.builder.getDoubleTy()), ty);
    else if(ty->isPointerTy())
      v = context.builder.CreateIntToPtr(&arg, ty);
    else
      assert(false);

    values.push_back(v);
  }
  llvmVal* val = context.builder.CreateCall(fn, values);

  if(!fn->getReturnType()->isVoidTy())
  {
    if(fn->getReturnType()->isIntegerTy()) // Directly convert all ints to i64
      val = context.builder.CreateIntCast(val, context.builder.getInt64Ty(), true);
    else if(fn->getReturnType()->isFloatingPointTy()) // expand to double, then bitcast to i64
      val = context.builder.CreateBitCast(context.builder.CreateFPCast(val, context.builder.getDoubleTy()), context.builder.getInt64Ty());
    else if(fn->getReturnType()->isPointerTy())
      val = context.builder.CreatePtrToInt(val, context.builder.getInt64Ty());
    else
      assert(false);
    context.builder.CreateRet(val);
  }
  else
    context.builder.CreateRet(context.builder.getInt64(0));

  context.builder.SetInsertPoint(prev);
  return wrap;
}

Func* WrapFunction(Func* fn, const Twine& name, code::Context& context)
{
  Func* wrap = Func::Create(fn->getFunctionType(), Func::InternalLinkage, name, context.llvm);
  wrap->setCallingConv(llvm::CallingConv::Fast);

  auto prev = context.builder.GetInsertBlock();

  BB* bb = BB::Create(context.context, "wrapper_block", wrap);
  context.builder.SetInsertPoint(bb);
  vector<llvmVal*> values;
  for(auto& arg : wrap->args())
    values.push_back(&arg);
  auto val = context.builder.CreateCall(fn, values);
  val->setCallingConv(fn->getCallingConv());

  if(!wrap->getReturnType()->isVoidTy())
    context.builder.CreateRet(val);
  else
    context.builder.CreateRetVoid();

  context.builder.SetInsertPoint(prev);
  return wrap;
}

IR_ERROR PushReturn(code::Context& context) { return ERR_SUCCESS; }

// Given a set of returns in the order given in the function/instruction signature, pushes them on to the stack in reverse order
template<typename Arg, typename... Args>
IR_ERROR PushReturn(code::Context& context, Arg arg, Args... args)
{
  IR_ERROR e = PushReturn(context, args...);
  context.values.Push(arg);
  return e;
}

bool CheckType(WASM_TYPE_ENCODING ty, llvmVal* v)
{
  llvmTy* t = v->getType();
  switch(ty)
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
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileBinaryOp(code::Context& context, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  if(context.values.Size() < 2)
    return assert(false), ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvmVal* val2 = context.values.Pop();
  llvmVal* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1) || !CheckType(Ty2, val2))
    return assert(false), ERR_INVALID_TYPE;

  return PushReturn(context, (context.builder.*op)(val1, val2, args...));
}

// Given an intrinsic function ID, pops two binary arguments off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IR_ERROR CompileBinaryIntrinsic(code::Context& context, llvm::Intrinsic::ID id, const Twine& name)
{
  if(context.values.Size() < 2)
    return assert(false), ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvmVal* val2 = context.values.Pop();
  llvmVal* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1) || !CheckType(Ty2, val2))
    return assert(false), ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateBinaryIntrinsic(id, val1, val2, name));
}

// Given a function pointer to the appropriate builder function, pops one unary argument off the stack and pushes the result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileUnaryOp(code::Context& context, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, Args...), Args... args)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;

  llvmVal* val1 = context.values.Pop();

  if(!CheckType(Ty1, val1))
    return assert(false), ERR_INVALID_TYPE;

  return PushReturn(context, (context.builder.*op)(val1, args...));
}

// Given an intrinsic function ID, pops one unary argument off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR>
IR_ERROR CompileUnaryIntrinsic(code::Context& context, llvm::Intrinsic::ID id, const Twine& name)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;

  llvmVal* val1 = context.values.Pop();
  Func *fn = llvm::Intrinsic::getDeclaration(context.llvm, id, { val1->getType() });

  if(!CheckType(Ty1, val1))
    return assert(false), ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateCall(fn, { val1 }, name, nullptr));
}

IR_ERROR CompileSelectOp(code::Context& context, const Twine& name, llvm::Instruction* from)
{
  if(context.values.Size() < 3)
    return assert(false), ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvmVal* cond = context.values.Pop();
  llvmVal* valf = context.values.Pop();
  llvmVal* valt = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return assert(false), ERR_INVALID_TYPE;

  return PushReturn(context, context.builder.CreateSelect(cond, valt, valf, name, from));
}

template<WASM_TYPE_ENCODING TYPE, bool LEFT>
IR_ERROR CompileRotationOp(code::Context& context, const char* name)
{
  if(context.values.Size() < 2)
    return assert(false), ERR_INVALID_VALUE_STACK;

  // Pop in reverse order
  llvmVal* count = context.values.Pop();
  llvmVal* value = context.values.Pop();

  if(!CheckType(TYPE, value) || !CheckType(TYPE, count))
    return assert(false), ERR_INVALID_TYPE;

  const int BITS = (TYPE == TE_i32) ? 32 : 64;
  llvmVal *l, *r;

  if(LEFT) // value<<count | value>>(BITS-count);
  {
    l = context.builder.CreateShl(value, count);
    r = context.builder.CreateLShr(value, context.builder.CreateSub(CInt::get(context.context, APInt(BITS, BITS, true)), count));
  }
  else // value>>count | value<<(BITS-count);
  {
    l = context.builder.CreateLShr(value, count);
    r = context.builder.CreateShl(value, context.builder.CreateSub(CInt::get(context.context, APInt(BITS, BITS, true)), count));
  }

  return PushReturn(context, context.builder.CreateOr(l, r, name));
}

BB* PushLabel(const char* name, varsint7 sig, uint8_t opcode, Func* fnptr, code::Context& context)
{
  BB* bb = BB::Create(context.context, name, fnptr);
  context.control.Push(code::Block{ bb, context.values.Limit(), sig, opcode });
  context.values.SetLimit(context.values.Size() + context.values.Limit()); // Set limit to current stack size to prevent a block from popping past this
  return bb;
}

void BindLabel(BB* block, code::Context& context)
{
  if(!block->getParent())
  {
    context.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(block);
    context.builder.SetInsertPoint(block);
  }
  assert(block->getParent() != 0);
}

void PushResult(code::BlockResult** root, llvmVal* result, BB* block)
{
  code::BlockResult* next = *root;
  *root = tmalloc<code::BlockResult>(1);
  new(*root) code::BlockResult{ result, block, next };
}

// Adds current value stack to target branch according to that branch's signature, but doesn't pop them.
IR_ERROR AddBranch(code::Block& target, code::Context& context)
{
  if(context.values.Size() < 1 || !CheckType(WASM_TYPE_ENCODING(target.sig), context.values.Peek())) // We only verify the type, not the entire stack
    return assert(false), ERR_INVALID_VALUE_STACK;
  PushResult(&target.results, context.values.Peek(), context.builder.GetInsertBlock()); // Push result

  return ERR_SUCCESS;
}

// Pops a label off the control stack, verifying that the value stack matches the signature and building PHI nodes as necessary
IR_ERROR PopLabel(code::Context& context)
{
  varsint7 sig = context.control.Peek().sig;
  if(sig != TE_void)
  {
    if(context.values.Size() != 1 || !CheckType(WASM_TYPE_ENCODING(sig), context.values.Peek())) // Verify stack
      return assert(false), ERR_INVALID_VALUE_STACK;
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
        return assert(false), ERR_INVALID_VALUE_STACK;

      context.values.Push(phi); // Push phi nodes on to stack
    }
  }
  else if(context.values.Size() > 0 || context.control.Peek().results != nullptr)
    return assert(false), ERR_INVALID_VALUE_STACK;

  context.values.SetLimit(context.control.Peek().limit);
  context.control.Pop();

  return ERR_SUCCESS;
}

IR_ERROR CompileIfBlock(varsint7 sig, code::Context& context)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;

  if(!context.values.Peek()) // If we're in an unreachable segment, just push another placeholder on to the stack
  {
    context.values.Push(nullptr);
    return ERR_SUCCESS;
  }

  llvmVal* cond = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return assert(false), ERR_INVALID_TYPE;

  llvmVal* cmp = context.builder.CreateICmpNE(cond, context.builder.getInt32(0), "if_cond");

  Func* parent = context.builder.GetInsertBlock()->getParent();
  BB* tblock = BB::Create(context.context, "if_true", parent);
  BB* fblock = PushLabel("if_false", sig, OP_if, nullptr, context);

  context.builder.CreateCondBr(cmp, tblock, fblock); // Insert branch in current block
  context.builder.SetInsertPoint(tblock); // Start inserting code into true block

  return ERR_SUCCESS;
}

IR_ERROR CompileElseBlock(code::Context& context)
{
  if(context.control.Size() == 0 || context.control.Peek().type != OP_if)
    return assert(false), ERR_IF_ELSE_MISMATCH;

  // Instead of popping and pushing a new control label, we just re-purpose the existing one. This preserves the value stack results.
  if(context.control.Peek().sig != TE_void)
  {
    if(context.values.Size() != 1 || !CheckType(WASM_TYPE_ENCODING(context.control.Peek().sig), context.values.Peek())) // Verify stack
      return assert(false), ERR_INVALID_VALUE_STACK;
    PushResult(&context.control.Peek().results, context.values.Peek(), context.builder.GetInsertBlock()); // Push result
  }
  else if(context.values.Size() > 0)
    return assert(false), ERR_INVALID_VALUE_STACK;

  while(context.values.Size()) // Reset value stack
    context.values.Pop();

  BB* fblock = context.control.Peek().block; // Get stored false block
  context.control.Peek().block = BB::Create(context.context, "if_merge"); // Store the if_merge block for use later
  context.control.Peek().type = OP_else; // make this block an OP_else block

  context.builder.CreateBr(context.control.Peek().block); // Add a branch-to-merge instruction to our if_true block
  BindLabel(fblock, context); // Bind if_false block to current position

  return ERR_SUCCESS;
}

void ClearValueStack(code::Context& context)
{
  while(context.values.Size() > 0)
    context.values.Pop();
}

IR_ERROR CompileReturn(code::Context& context, varsint7 sig)
{
  if(sig == TE_void)
    context.builder.CreateRetVoid();
  else
  {
    if(context.values.Size() < 1)
      return assert(false), ERR_INVALID_VALUE_STACK;
    if(!CheckType(WASM_TYPE_ENCODING(sig), context.values.Peek()))
      return assert(false), ERR_INVALID_TYPE;
    context.builder.CreateRet(context.values.Peek());
  }

  ClearValueStack(context);
  context.values.Push(nullptr); // Push polymorphic placeholder
  return ERR_SUCCESS;
}

IR_ERROR CompileEndBlock(code::Context& context)
{
  if(context.control.Size() == 0)
  {
    if(context.control.Limit() > 0)
    {
      varsint7 sig = context.control[context.control.Size()].sig;
      context.control.SetLimit(0);
      if(context.control.Peek().type != OP_return)
        return assert(false), ERR_END_MISMATCH;

      IR_ERROR err = PopLabel(context);
      if(context.values.Size() > 0 && !context.values.Peek())
      {
        context.values.Pop();
        return err;  // In this case, the end of the function itself is unreachable code, so we can't generate a return statement
      }
      if(err >= 0)
        err = CompileReturn(context, sig);
      return err;
    }
    return assert(false), ERR_END_MISMATCH;
  }
  if(context.values.Size() > 0 && !context.values.Peek()) // Pop off one polymorphic placeholder
    context.values.Pop();
  if(!context.values.Size() || context.values.Peek() != nullptr) // If we're still in unreachable code, just bail out
    return ERR_SUCCESS;
  context.builder.CreateBr(context.control.Peek().block); // Branch into next block
  BindLabel(context.control.Peek().block, context);

  switch(context.control.Peek().type) // Verify source operation
  {
  case OP_if:
    if(context.control.Peek().sig != TE_void) // An if statement with no else statement cannot return a value
      return assert(false), ERR_EXPECTED_ELSE_INSTRUCTION;
  case OP_else:
  case OP_block:
  case OP_loop:
    break;
  default:
    return assert(false), ERR_END_MISMATCH;
  }

  return PopLabel(context);
}

IR_ERROR CompileTrap(code::Context& context)
{
  auto call = context.builder.CreateCall(llvm::Intrinsic::getDeclaration(context.llvm, llvm::Intrinsic::trap), { });
  call->setDoesNotReturn();
  context.builder.CreateUnreachable();
  return ERR_SUCCESS;
}

IR_ERROR InsertConditionalTrap(llvmVal* cond, code::Context& context)
{
  // Define a failure block that all errors jump to via a conditional branch which simply traps
  auto trapblock = BB::Create(context.context, "trap_block", context.builder.GetInsertBlock()->getParent());
  auto contblock = BB::Create(context.context, "trap_continue", context.builder.GetInsertBlock()->getParent());

  context.builder.CreateCondBr(cond, trapblock, contblock);
  context.builder.SetInsertPoint(trapblock);
  CompileTrap(context);

  context.builder.SetInsertPoint(contblock);
  return ERR_SUCCESS;
}

IR_ERROR CompileBranch(varuint32 depth, code::Context& context)
{
  if(depth >= context.control.Size())
    return assert(false), ERR_INVALID_BRANCH_DEPTH;

  code::Block& target = context.control[depth];
  context.builder.CreateBr(target.block);
  ClearValueStack(context);
  return (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}

IR_ERROR CompileIfBranch(varuint32 depth, code::Context& context)
{
  if(depth >= context.control.Size())
    return assert(false), ERR_INVALID_BRANCH_DEPTH;

  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;
  llvmVal* cond = context.values.Pop();

  if(!CheckType(TE_i32, cond))
    return assert(false), ERR_INVALID_TYPE;

  llvmVal* cmp = context.builder.CreateICmpNE(cond, context.builder.getInt32(0), "br_if_cond");

  // Because llvm requires explicit branches, we have to create a new block and append it to our current one
  BB* block = BB::Create(context.context, "br_if_cont", context.builder.GetInsertBlock()->getParent());

  code::Block& target = context.control[depth];
  context.builder.CreateCondBr(cmp, target.block, block);
  context.builder.SetInsertPoint(block); // Start inserting code into continuation.
  return (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}
IR_ERROR CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def, code::Context& context)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;
  llvmVal* index = context.values.Pop();
  ClearValueStack(context);

  if(!CheckType(TE_i32, index))
    return assert(false), ERR_INVALID_TYPE;

  if(def >= context.control.Size())
    return assert(false), ERR_INVALID_BRANCH_DEPTH;

  llvm::SwitchInst* s = context.builder.CreateSwitch(index, context.control[def].block, n_table);
  IR_ERROR err = (context.control[def].type != OP_loop) ? AddBranch(context.control[def], context) : ERR_SUCCESS;

  for(varuint32 i = 0; i < n_table && err == ERR_SUCCESS; ++i)
  {
    if(table[i] >= context.control.Size())
      return assert(false), ERR_INVALID_BRANCH_DEPTH;

    code::Block& target = context.control[table[i]];
    s->addCase(CInt::get(context.context, APInt(32, i, true)), target.block);
    err = (target.type != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS;
  }

  return err; // TODO: determine if another block is needed
}

bool CompareTypes(llvmTy* a, llvmTy* b)
{
  if(a->isFloatTy())
    return b->isFloatTy();
  if(a->isDoubleTy())
    return b->isDoubleTy();
  if(a->isVoidTy())
    return b->isVoidTy();
  if(a->isIntegerTy())
    return b->isIntegerTy() && (static_cast<llvm::IntegerType*>(a)->getBitWidth() == static_cast<llvm::IntegerType*>(b)->getBitWidth());
  return false;
}

IR_ERROR CompileCall(varuint32 index, code::Context& context)
{
  if(index >= context.functions.size())
    return assert(false), ERR_INVALID_FUNCTION_INDEX;

  // Because this is a static function call, we can call the imported C function directly with the appropriate calling convention
Func* fn = !context.functions[index].imported ? context.functions[index].internal: context.functions[index].imported;
unsigned int num = fn->getFunctionType()->getNumParams();
if(num > context.values.Size())
return assert(false), ERR_INVALID_VALUE_STACK;

// Pop arguments in reverse order
llvmVal** ArgsV = tmalloc<llvmVal*>(num);
for(unsigned int i = num; i-- > 0;)
{
  ArgsV[i] = context.values.Pop();
  if(!CompareTypes(ArgsV[i]->getType(), fn->getFunctionType()->getParamType(i)))
    return assert(false), ERR_INVALID_ARGUMENT_TYPE;
}

CallInst* call = context.builder.CreateCall(fn, llvm::makeArrayRef(ArgsV, num));
if(context.env.flags & ENV_STRICT) // In strict mode, tail call optimization is not allowed
call->setTailCallKind(CallInst::TCK_NoTail);
call->setCallingConv(fn->getCallingConv());
call->setAttributes(fn->getAttributes());

if(!fn->getReturnType()->isVoidTy()) // Only push a value if there is one to push
return PushReturn(context, call);
return ERR_SUCCESS;
}

llvmVal* GetMemSize(llvm::GlobalVariable* target, code::Context& context)
{
  return context.builder.CreateLoad(
    context.builder.CreateGEP(
      context.builder.CreatePointerCast(
        context.builder.CreateLoad(target),
        context.builder.getInt64Ty()->getPointerTo()),
      context.builder.getInt32(-1)));
}

IR_ERROR CompileIndirectCall(varuint32 sigindex, code::Context& context)
{
  if(sigindex >= context.m.type.n_functions)
    return assert(false), ERR_INVALID_TYPE_INDEX;

  FunctionSig& sig = context.m.type.functions[sigindex];

  if(sig.n_params + 1 > context.values.Size())
    return assert(false), ERR_INVALID_VALUE_STACK;

  llvmVal* callee = context.values.Pop();

  if(context.tables.size() < 1)
    return assert(false), ERR_INVALID_TABLE_INDEX;

  // Pop arguments in reverse order
  llvmVal** ArgsV = tmalloc<llvmVal*>(sig.n_params);
  for(unsigned int i = sig.n_params; i-- > 0;)
  {
    ArgsV[i] = context.values.Pop();
    if(!CompareTypes(ArgsV[i]->getType(), GetType(sig.params[i], context)))
      return assert(false), ERR_INVALID_ARGUMENT_TYPE;
  }

  if(context.env.flags & ENV_STRICT) // In strict mode, trap if index is out of bounds
    InsertConditionalTrap(context.builder.CreateICmpUGE(callee, context.builder.CreateIntCast(context.builder.CreateLShr(GetMemSize(context.tables[0], context), 3), context.builder.getInt32Ty(), false), "indirect_call_oob_check"), context);

  // Deference global variable to get the actual array of function pointers, index into them, then dereference that array index to get the actual function pointer
  llvmVal* funcptr = context.builder.CreateLoad(context.builder.CreateGEP(context.builder.CreateLoad(context.tables[0]), callee), "indirect_call_load_func_ptr");

  if(context.env.flags & ENV_STRICT) // In strict mode, trap if function pointer is NULL
    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(funcptr, context.intptrty), CInt::get(context.intptrty, 0), "indirect_call_null_check"), context);

  // Now that we have the function pointer we have to actually cast back to the function signature that we expect, instead of void()
  FuncTy* ty = GetFunctionType(sig, context);
  funcptr = context.builder.CreatePointerCast(funcptr, ty->getPointerTo(0));

  if(context.env.flags & ENV_STRICT) // In strict mode, trap if the expected type does not match the actual type of the function
  {
    auto funcsig = context.builder.CreateLoad(context.builder.CreateGEP(context.builder.CreateLoad(context.tabletypes[0]), callee));
    InsertConditionalTrap(context.builder.CreateICmpNE(funcsig, context.builder.getInt32(sigindex), "indirect_call_sig_check"), context);
  }

  // CreateCall will then do the final dereference of the function pointer to make the indirect call
  CallInst* call = context.builder.CreateCall(funcptr, llvm::makeArrayRef(ArgsV, sig.n_params), "indirect_call");

  if(context.env.flags & ENV_STRICT) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(CallInst::TCK_NoTail);
  call->setCallingConv(llvm::CallingConv::Fast); // Always pick the fast convention, because the table is always set to the internal wrapping function

  if(!ty->getReturnType()->isVoidTy()) // Only push a value if there is one to push
    return PushReturn(context, call);
  return ERR_SUCCESS;
}

std::pair<IR_ERROR, llvm::Constant*> CompileConstant(Instruction& ins, code::Context& context)
{
  switch(ins.opcode)
  {
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
    return { ERR_SUCCESS, CInt::get(context.context, APInt(32, ins.immediates[0]._varuint32, true)) };
  case OP_i64_const:
    return { ERR_SUCCESS, CInt::get(context.context, APInt(64, ins.immediates[0]._varuint64, true)) };
  case OP_f32_const:
    return { ERR_SUCCESS, ConstantFP::get(context.context, APFloat(ins.immediates[0]._float32)) };
  case OP_f64_const:
    return { ERR_SUCCESS, ConstantFP::get(context.context, APFloat(ins.immediates[0]._float64)) };
  case OP_get_global:
    if(ins.immediates[0]._varuint32 >= context.globals.size())
      return { ERR_INVALID_GLOBAL_INDEX, 0 };
    return { ERR_SUCCESS, context.globals[ins.immediates[0]._varuint32] };
  }

  return { ERR_INVALID_INITIALIZER, 0 };
}

llvmVal* GetMemPointer(code::Context& context, llvmVal* base, llvm::PointerType* ptr, varuint7 memory, varuint32 offset)
{
  llvmVal* loc = context.builder.CreateAdd(base, CInt::get(context.context, APInt(32, offset, true)), "", true, true);

  if(context.env.flags&ENV_STRICT) // In strict mode, generate a check that traps if this is an invalid memory access
    InsertConditionalTrap(context.builder.CreateICmpULE(loc, context.builder.getInt32(0), "invalid_mem_access_cond"), context);

  return context.builder.CreatePointerCast(context.builder.CreateGEP(context.builder.CreateLoad(context.memories[memory]), loc), ptr);
}

template<bool SIGNED>
IR_ERROR CompileLoad(code::Context& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvmTy* ext, llvmTy* ty)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  llvmVal* base = context.values.Pop();

  if(!CheckType(TE_i32, base))
    return assert(false), ERR_INVALID_TYPE;

  llvmVal* result = context.builder.CreateAlignedLoad(GetMemPointer(context, base, ty->getPointerTo(0), memory, offset), (1 << memflags), name);

  if(ext != nullptr)
    result = SIGNED ? context.builder.CreateSExt(result, ext) : context.builder.CreateZExt(result, ext);

  return PushReturn(context, result);
}

template<WASM_TYPE_ENCODING TY>
IR_ERROR CompileStore(code::Context& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::IntegerType* ext)
{
  if(context.values.Size() < 2)
    return assert(false), ERR_INVALID_VALUE_STACK;
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  llvmVal* value = context.values.Pop();
  llvmVal* base = context.values.Pop();

  if(!CheckType(TY, value))
    return assert(false), ERR_INVALID_TYPE;
  if(!CheckType(TE_i32, base))
    return assert(false), ERR_INVALID_TYPE;
  llvmTy* PtrType = !ext ? GetType(TY, context) : ext;

  llvmVal* ptr = GetMemPointer(context, base, PtrType->getPointerTo(0), memory, offset);
  llvmVal* result = context.builder.CreateAlignedStore(!ext ? value : context.builder.CreateIntCast(value, ext, false), ptr, (1 << memflags), name);

  return ERR_SUCCESS;
}

// Gets memory size in pages, not bytes
llvmVal* CompileMemSize(llvm::GlobalVariable* target, code::Context& context)
{
  return context.builder.CreateIntCast(context.builder.CreateLShr(GetMemSize(target, context), 16), context.builder.getInt32Ty(), true);
}

IR_ERROR CompileMemGrow(code::Context& context, const char* name)
{
  if(context.values.Size() < 1)
    return assert(false), ERR_INVALID_VALUE_STACK;
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  llvmVal* delta = context.values.Pop();
  llvmVal* old = CompileMemSize(context.memories[0], context);

  if(!CheckType(TE_i32, delta))
    return assert(false), ERR_INVALID_TYPE;
  auto max = CInt::get(context.context, APInt(64, (context.m.memory.memory[0].limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? context.m.memory.memory[0].limits.maximum : 0, true));
  CallInst* call = context.builder.CreateCall(context.memgrow, { context.memories[0], context.builder.CreateShl(context.builder.CreateZExt(delta, context.builder.getInt64Ty()), 16), max }, name);

  auto result = context.builder.CreateSelect(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context.builder.getInt32(-1), old);
  return PushReturn(context, result);
}

IR_ERROR CompileInstruction(Instruction& ins, code::Context& context)
{
  if(context.values.Peek() == nullptr) // If we have a polymorphic type on top we're in unreachable code so just skip all non-control operators
  {
    switch(ins.opcode)
    {
    case OP_block:
    case OP_loop:
    case OP_if:
    case OP_end:
      break;
    default:
      return ERR_SUCCESS;
    }
  }
  switch(ins.opcode)
  {
  case OP_unreachable:
    context.builder.CreateUnreachable();
    ClearValueStack(context);
    return CompileTrap(context);
  case OP_nop:
    return ERR_SUCCESS;
  case OP_block:
    PushLabel("block", ins.immediates[0]._varsint7, OP_block, nullptr, context);
    if(context.values.Size() > 0 && !context.values.Peek())
      context.values.Push(nullptr);
    return ERR_SUCCESS;
  case OP_loop:
    PushLabel("loop", ins.immediates[0]._varsint7, OP_loop, nullptr, context);
    context.builder.CreateBr(context.control.Peek().block); // Branch into next block
    BindLabel(context.control.Peek().block, context);
    if(context.values.Size() > 0 && !context.values.Peek())
      context.values.Push(nullptr);
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
    return CompileReturn(context, context.control[context.control.Size()].sig);

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
    if(ins.immediates[0]._varuint32 >= context.locals.size())
      return assert(false), ERR_INVALID_LOCAL_INDEX;
    context.values.Push(context.builder.CreateLoad(context.locals[ins.immediates[0]._varuint32]));
    return ERR_SUCCESS;
  case OP_set_local:
  case OP_tee_local:
    if(ins.immediates[0]._varuint32 >= context.locals.size())
      return assert(false), ERR_INVALID_LOCAL_INDEX;
    if(context.values.Size() < 1)
      return assert(false), ERR_INVALID_VALUE_STACK;
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
    if(ins.immediates[0]._varuint32 >= context.globals.size())
      return assert(false), ERR_INVALID_GLOBAL_INDEX;
    if(context.values.Size() < 1)
      return assert(false), ERR_INVALID_VALUE_STACK;
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
  case OP_memory_size:
    return PushReturn(context, CompileMemSize(context.memories[0], context));
  case OP_memory_grow:
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
    context.values.Push(CInt::get(context.context, APInt(32, 0, true))); // Fallthrough to OP_i32_eq
  case OP_i32_eq:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpEQ, OPNAMES[ins.opcode]);
  case OP_i32_ne:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpNE, OPNAMES[ins.opcode]);
  case OP_i32_lt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLT, OPNAMES[ins.opcode]);
  case OP_i32_lt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpULT, OPNAMES[ins.opcode]);
  case OP_i32_gt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGT, OPNAMES[ins.opcode]);
  case OP_i32_gt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGT, OPNAMES[ins.opcode]);
  case OP_i32_le_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLE, OPNAMES[ins.opcode]);
  case OP_i32_le_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpULE, OPNAMES[ins.opcode]);
  case OP_i32_ge_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGE, OPNAMES[ins.opcode]);
  case OP_i32_ge_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGE, OPNAMES[ins.opcode]);
  case OP_i64_eqz:
    context.values.Push(CInt::get(context.context, APInt(64, 0, true))); // Fallthrough to OP_i64_eq
  case OP_i64_eq:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpEQ, OPNAMES[ins.opcode]);
  case OP_i64_ne:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpNE, OPNAMES[ins.opcode]);
  case OP_i64_lt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLT, OPNAMES[ins.opcode]);
  case OP_i64_lt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpULT, OPNAMES[ins.opcode]);
  case OP_i64_gt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGT, OPNAMES[ins.opcode]);
  case OP_i64_gt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGT, OPNAMES[ins.opcode]);
  case OP_i64_le_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSLE, OPNAMES[ins.opcode]);
  case OP_i64_le_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpULE, OPNAMES[ins.opcode]);
  case OP_i64_ge_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpSGE, OPNAMES[ins.opcode]);
  case OP_i64_ge_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateICmpUGE, OPNAMES[ins.opcode]);
  case OP_f32_eq:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOEQ, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ne:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpUNE, OPNAMES[ins.opcode], nullptr);
  case OP_f32_lt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLT, OPNAMES[ins.opcode], nullptr);
  case OP_f32_gt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGT, OPNAMES[ins.opcode], nullptr);
  case OP_f32_le:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLE, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ge:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_eq:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOEQ, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ne:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpUNE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_lt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLT, OPNAMES[ins.opcode], nullptr);
  case OP_f64_gt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGT, OPNAMES[ins.opcode], nullptr);
  case OP_f64_le:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOLE, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ge:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFCmpOGE, OPNAMES[ins.opcode], nullptr);

    // Numeric operators
  case OP_i32_clz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::ctlz, OPNAMES[ins.opcode]);
  case OP_i32_ctz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::cttz, OPNAMES[ins.opcode]);
  case OP_i32_popcnt:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::ctpop, OPNAMES[ins.opcode]);
  case OP_i32_add:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateAdd, OPNAMES[ins.opcode], false, false);
  case OP_i32_sub:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateSub, OPNAMES[ins.opcode], false, false);
  case OP_i32_mul:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateMul, OPNAMES[ins.opcode], false, false);
  case OP_i32_div_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateSDiv, OPNAMES[ins.opcode], false);
  case OP_i32_div_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateUDiv, OPNAMES[ins.opcode], false);
  case OP_i32_rem_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateSRem, OPNAMES[ins.opcode]);
  case OP_i32_rem_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i32_and:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i32_or:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i32_xor:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i32_shl:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i32_shr_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i32_shr_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
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
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateAdd, OPNAMES[ins.opcode], false, false);
  case OP_i64_sub:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateSub, OPNAMES[ins.opcode], false, false);
  case OP_i64_mul:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateMul, OPNAMES[ins.opcode], false, false);
  case OP_i64_div_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateSDiv, OPNAMES[ins.opcode], false);
  case OP_i64_div_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateUDiv, OPNAMES[ins.opcode], false);
  case OP_i64_rem_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateSRem, OPNAMES[ins.opcode]);
  case OP_i64_rem_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i64_and:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i64_or:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i64_xor:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i64_shl:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i64_shr_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i64_shr_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
  case OP_i64_rotl:
    return CompileRotationOp<TE_i64, true>(context, OPNAMES[ins.opcode]);
  case OP_i64_rotr:
    return CompileRotationOp<TE_i64, false>(context, OPNAMES[ins.opcode]);
  case OP_f32_abs:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f32_neg:
    return CompileUnaryOp<TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFNeg, OPNAMES[ins.opcode], nullptr);
  case OP_f32_ceil:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f32_floor:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f32_trunc:
    return CompileUnaryOp<TE_f32, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_nearest: // We must round floats using IEEE 754-2008 roundToIntegralTowardZero, which rint gaurantees
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::rint, OPNAMES[ins.opcode]);
  case OP_f32_sqrt:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f32_add:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], false);
  case OP_f32_sub:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], false);
  case OP_f32_mul:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], false);
  case OP_f32_div:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], false);
  case OP_f32_min:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f32_max:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f32_copysign:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);
  case OP_f64_abs:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f64_neg:
    return CompileUnaryOp<TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFNeg, OPNAMES[ins.opcode], nullptr);
  case OP_f64_ceil:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f64_floor:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f64_trunc:
    return CompileUnaryOp<TE_f64, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_nearest:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::rint, OPNAMES[ins.opcode]);
  case OP_f64_sqrt:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f64_add:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], false);
  case OP_f64_sub:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], false);
  case OP_f64_mul:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], false);
  case OP_f64_div:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], false);
  case OP_f64_min:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f64_max:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f64_copysign:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);

    // Conversions
  case OP_i32_wrap_i64:
    return CompileUnaryOp<TE_i64, TE_i32, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt32Ty(), true, OPNAMES[ins.opcode]);
  case OP_i32_trunc_s_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_u_f32:
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_s_f64:
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
    // TODO struct: trap if value too large to fit in integer
  case OP_i32_trunc_u_f64:
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_extend_s_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), true, OPNAMES[ins.opcode]);
  case OP_i64_extend_u_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), false, OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f32:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f64:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f64:
    // TODO struct: trap if value too large to fit in integer
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_convert_s_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_u_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_s_i64:
    return CompileUnaryOp<TE_i64, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_u_i64:
    return CompileUnaryOp<TE_i64, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_demote_f64:
    return CompileUnaryOp<TE_f64, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_s_i32:
    return CompileUnaryOp<TE_i32, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_u_i32:
    return CompileUnaryOp<TE_i32, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_s_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateSIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_u_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateUIToFP, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_promote_f32:
    return CompileUnaryOp<TE_f32, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPExt, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);

    // Reinterpretations
  case OP_i32_reinterpret_f32:
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_reinterpret_f64:
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_reinterpret_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_reinterpret_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateBitCast, context.builder.getDoubleTy(), OPNAMES[ins.opcode]);
  default:
    return assert(false), ERR_FATAL_UNKNOWN_INSTRUCTION;
  }

  assert(false); // ERROR NOT IMPLEMENTED
  return ERR_SUCCESS;
}

IR_ERROR CompileFunctionBody(Func* fn, FunctionSig& sig, FunctionBody& body, code::Context& context)
{
  // Ensure context is reset
  assert(!context.control.Size() && !context.control.Limit());
  assert(!context.values.Size() && !context.values.Limit());

  // Get return value
  varsint7 ret = TE_void;
  if(sig.n_returns > 0)
    ret = sig.returns[0];

  // Setup initial basic block.
  context.builder.SetInsertPoint(PushLabel("entry", ret, OP_return, fn, context));
  context.control.SetLimit(1); // Don't allow breaking to the function entry
  context.locals.resize(0);
  context.locals.reserve(sig.n_params + body.n_locals);
  varuint32 index = 0;

  // We allocate parameters first, followed by local variables
  for(auto& arg : fn->args())
  {
    assert(index < sig.n_params);
    auto ty = GetType(sig.params[index], context);
    assert(ty == arg.getType());
    context.locals.push_back(context.builder.CreateAlloca(ty, nullptr, (body.param_names && body.param_names[index]) ? body.param_names[index] : ""));
    context.builder.CreateStore(&arg, context.locals.back()); // Store parameter (we can't use the parameter directly because wasm lets you store to parameters)
    ++index;
  }

  for(uint64_t i = 0; i < body.n_locals; ++i)
    context.locals.push_back(context.builder.CreateAlloca(GetType(body.locals[i], context), nullptr, (body.local_names && body.local_names[i]) ? body.local_names[i] : ""));

  // Begin iterating through the instructions until there aren't any left
  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    IR_ERROR err = CompileInstruction(body.body[i], context);
    if(err != ERR_SUCCESS)
      return err;
  }

  if(body.body[body.n_body - 1].opcode != OP_end)
    return assert(false), ERR_FATAL_EXPECTED_END_INSTRUCTION;
  if(context.control.Size() > 0 || context.control.Limit() > 0)
    return assert(false), ERR_END_MISMATCH;
  if(context.values.Size() > 0 || context.values.Limit() > 0)
    return assert(false), ERR_INVALID_VALUE_STACK;
  return ERR_SUCCESS;
}

llvm::GlobalVariable* CreateGlobal(code::Context& context, llvmTy* ty, bool isconst, bool external, const Twine& name, llvm::Constant* init = 0)
{
  return new llvm::GlobalVariable(
    *context.llvm,
    ty,
    isconst,
    external ? llvm::GlobalValue::LinkageTypes::ExternalLinkage : llvm::GlobalValue::LinkageTypes::InternalLinkage,
    init,
    name,
    nullptr,
    llvm::GlobalValue::NotThreadLocal,
    0,
    !init);
}

llvm::Constant* CompileInitConstant(Instruction& ins, code::Context& context)
{
  auto pair = CompileConstant(ins, context);
  return pair.first < 0 ? nullptr : pair.second;
}
uint64_t GetTotalSize(llvmTy* t)
{
  return t->getArrayNumElements() * (t->getArrayElementType()->getPrimitiveSizeInBits() / 8);
}

int GetCallingConvention(Import& imp)
{
  const char* str = !imp.module_name.str() ? nullptr : strrchr(imp.module_name.str(), '!');
  if(!str)
    return llvm::CallingConv::C;
  ++str;
  if(!STRICMP(str, "C"))
    return llvm::CallingConv::C;
  if(!STRICMP(str, "STD"))
    return llvm::CallingConv::X86_StdCall;
  if(!STRICMP(str, "JS"))
    return llvm::CallingConv::WebKit_JS;
  if(!STRICMP(str, "GHC"))
    return llvm::CallingConv::GHC;
  if(!STRICMP(str, "SWIFT"))
    return llvm::CallingConv::Swift;
  if(!STRICMP(str, "HiPE"))
    return llvm::CallingConv::HiPE;
  return llvm::CallingConv::C;
}

llvm::Function* GetIntrinsic(Import& imp, code::Context& context)
{
  if(!imp.module_name.size())
  {
    for(auto& v : code::intrinsics)
    {
      if(!strcmp(v.name, imp.export_name.str()))
        return v.fn = (*v.gen)(0, context);
    }
  }
  return nullptr;
}

IR_ERROR CompileModule(Environment* env, code::Context& context)
{
  context.llvm = new llvm::Module(context.m.name.str(), context.context);
  context.llvm->setTargetTriple(context.machine->getTargetTriple().getTriple());
  context.llvm->setDataLayout(context.machine->createDataLayout());
  context.intptrty = context.builder.getIntPtrTy(context.llvm->getDataLayout(), 0);

  // Define a unique init function for performing module initialization
  context.init = Func::Create(
    FuncTy::get(context.builder.getVoidTy(), false),
    Func::ExternalLinkage,
    MergeName(context.m.name.str(), "innative_internal_init"),
    context.llvm);

  BB* initblock = BB::Create(
    context.context,
    "init_entry",
    context.init);

  context.builder.SetInsertPoint(initblock);

  // Declare C runtime function prototypes that we assume exist on the system
  context.memgrow = Func::Create(
    FuncTy::get(context.builder.getInt8PtrTy(0), { context.builder.getInt8PtrTy(0), context.builder.getInt64Ty(), context.builder.getInt64Ty() }, false),
    Func::ExternalLinkage,
    "_innative_internal_env_grow_memory",
    context.llvm);
  context.memgrow->setReturnDoesNotAlias(); // This is a system memory allocation function, so the return value does not alias

  Func* fn_memcpy = Func::Create(
    FuncTy::get(context.builder.getVoidTy(), { context.builder.getInt8PtrTy(0), context.builder.getInt8PtrTy(0), context.builder.getInt64Ty() }, false),
    Func::ExternalLinkage,
    "_innative_internal_env_memcpy",
    context.llvm);

  fn_print = Func::Create(FuncTy::get(context.builder.getVoidTy(), { context.builder.getInt64Ty() }, false), Func::ExternalLinkage, "_innative_internal_env_print_compiler", context.llvm);

  context.functions.reserve(context.m.importsection.functions + context.m.function.n_funcdecl);
  context.tables.reserve(context.m.importsection.tables - context.m.importsection.functions + context.m.table.n_tables);
  if(env->flags&ENV_STRICT)
    context.tabletypes.reserve(context.tables.size());

  context.memories.reserve(context.m.importsection.memory - context.m.importsection.tables + context.m.memory.n_memory);
  context.globals.reserve(context.m.importsection.globals - context.m.importsection.memory + context.m.global.n_globals);

  // Import function prototypes
  for(varuint32 i = 0; i < context.m.importsection.functions; ++i)
  {
    context.functions.emplace_back();
    context.functions.back().internal = GetIntrinsic(context.m.importsection.imports[i], context);
    if(context.functions.back().internal == nullptr)
    {
      auto index = context.m.importsection.imports[i].func_desc.sig_index;
      if(index >= context.m.type.n_functions)
        return assert(false), ERR_INVALID_TYPE_INDEX;
      context.functions.back().internal = CompileFunction(
        context.m.type.functions[index],
        CanonImportName(context.m.importsection.imports[i]),
        context);

      auto e = ResolveExport(*env, context.m.importsection.imports[i]);
      if(!e.second)
      {
        context.functions.back().imported = context.functions.back().internal;
        context.functions.back().imported->setLinkage(Func::ExternalLinkage);
        context.functions.back().imported->setCallingConv(GetCallingConvention(context.m.importsection.imports[i]));
        context.functions.back().internal = WrapFunction(
          context.functions.back().imported,
          "@" + context.functions.back().imported->getName() + ":internal",
          context);

        if(context.m.importsection.imports[i].func_desc.debug_name.get())
          context.functions.back().internal->setName(context.m.importsection.imports[i].func_desc.debug_name.str() + (":" + std::to_string(i)));
      }
    }
  }

  // Import tables
  for(varuint32 i = context.m.importsection.functions; i < context.m.importsection.tables; ++i)
    context.tables.push_back(CreateGlobal(
      context,
      GetType(context.m.importsection.imports[i].table_desc.element_type, context)->getPointerTo(0),
      false,
      true,
      CanonImportName(context.m.importsection.imports[i])));

  // Import memory
  for(varuint32 i = context.m.importsection.tables; i < context.m.importsection.memory; ++i)
    context.memories.push_back(CreateGlobal(
      context,
      context.builder.getInt8PtrTy(0),
      false,
      true,
      CanonImportName(context.m.importsection.imports[i])));

  // Import global variables
  for(varuint32 i = context.m.importsection.memory; i < context.m.importsection.globals; ++i)
  {
    context.globals.push_back(CreateGlobal(
      context,
      GetType(context.m.importsection.imports[i].global_desc.type, context),
      !context.m.importsection.imports[i].global_desc.mutability,
      true,
      CanonImportName(context.m.importsection.imports[i])));
  }

  // Cache internal function start index
  if(context.m.function.n_funcdecl != context.m.code.n_funcbody)
    return assert(false), ERR_INVALID_FUNCTION_BODY;
  varuint32 code_index = context.functions.size();

  // Declare function prototypes
  for(varuint32 i = 0; i < context.m.function.n_funcdecl; ++i)
  {
    auto index = context.m.function.funcdecl[i];
    if(index >= context.m.type.n_functions)
      return assert(false), ERR_INVALID_TYPE_INDEX;

    context.functions.emplace_back();
    context.functions.back().internal = CompileFunction(
      context.m.type.functions[index],
      "func:" + std::to_string(context.functions.size()),
      context);
  }

  // Declare tables and allocate in init function
  for(varuint32 i = 0; i < context.m.table.n_tables; ++i)
  {
    auto type = GetType(context.m.table.tables[i].element_type, context)->getPointerTo(0);
    context.tables.push_back(CreateGlobal(
      context,
      type,
      false,
      false,
      MergeName(context.m.name.str(), "table#", i),
      llvm::ConstantPointerNull::get(type)));

    uint64_t bytewidth = context.llvm->getDataLayout().getTypeAllocSize(context.tables.back()->getType());
    if(!bytewidth)
      return assert(false), ERR_INVALID_TABLE_TYPE;

    CallInst* call = context.builder.CreateCall(
      context.memgrow,
      {
        llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy(0)),
        CInt::get(context.context, APInt(64, context.m.table.tables[i].resizable.minimum * bytewidth, true)),
        CInt::get(context.context, APInt(64, (context.m.table.tables[i].resizable.flags & WASM_LIMIT_HAS_MAXIMUM) ? context.m.table.tables[i].resizable.maximum * bytewidth : 0, true))
      });

    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context);
    context.builder.CreateStore(context.builder.CreatePointerCast(call, type), context.tables.back());

    if(env->flags&ENV_STRICT)
    {
      type = llvmTy::getInt32PtrTy(context.context);
      context.tabletypes.push_back(CreateGlobal(
        context,
        type,
        false,
        false,
        MergeName(context.m.name.str(), "tabletype#", i),
        llvm::ConstantPointerNull::get(type)));

      call = context.builder.CreateCall(
        context.memgrow,
        {
          llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy(0)),
          CInt::get(context.context, APInt(64, context.m.table.tables[i].resizable.minimum * sizeof(int32_t), true)),
          CInt::get(context.context, APInt(64, (context.m.table.tables[i].resizable.flags & WASM_LIMIT_HAS_MAXIMUM) ? context.m.table.tables[i].resizable.maximum * sizeof(int32_t) : 0, true))
        });

      InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context);
      context.builder.CreateStore(context.builder.CreatePointerCast(call, type), context.tabletypes.back());
    }
  }

  // Declare linear memory spaces and allocate in init function
  for(varuint32 i = 0; i < context.m.memory.n_memory; ++i)
  {
    auto type = context.builder.getInt8PtrTy(0);
    auto sz = CInt::get(context.context, APInt(64, ((uint64_t)context.m.memory.memory[i].limits.minimum) << 16, true));
    auto max = CInt::get(context.context, APInt(64, (context.m.memory.memory[i].limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? (((uint64_t)context.m.memory.memory[i].limits.maximum) << 16) : 0ULL, true));
    context.memories.push_back(CreateGlobal(context, type, false, false, MergeName(context.m.name.str(), "linearmemory#", i), llvm::ConstantPointerNull::get(type)));

    CallInst* call = context.builder.CreateCall(context.memgrow, { llvm::ConstantPointerNull::get(type), sz, max });
    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context);
    context.builder.CreateStore(call, context.memories.back());
  }

  // Declare global variables
  for(varuint32 i = 0; i < context.m.global.n_globals; ++i)
  {
    context.globals[i + context.m.importsection.globals] = CreateGlobal(
      context,
      GetType(context.m.global.globals[i].desc.type, context),
      !context.m.global.globals[i].desc.mutability,
      false,
      MergeName(context.m.name.str(), "globalvariable#", i),
      CompileInitConstant(context.m.global.globals[i].init, context));
  }

  // Set ENV_HOMOGENIZE_FUNCTIONS flag appropriately.
  auto wrapperfn = (env->flags & ENV_HOMOGENIZE_FUNCTIONS) ? &HomogenizeFunction : &WrapFunction;

  // Process exports by modifying global variables or function definitions as needed
  for(varuint32 i = 0; i < context.m.exportsection.n_exports; ++i)
  {
    Export& e = context.m.exportsection.exports[i];
    switch(e.kind)
    {
    case WASM_KIND_FUNCTION:
      
      context.functions[e.index].exported = (*wrapperfn)(context.functions[e.index].imported ? context.functions[e.index].imported : context.functions[e.index].internal, MergeName(context.m.name.str(), e.name.str()), context);
      context.functions[e.index].exported->setLinkage(Func::ExternalLinkage);
      context.functions[e.index].exported->setCallingConv(llvm::CallingConv::C);
      break;
    case WASM_KIND_TABLE:
      assert(context.tables[e.index]->getLinkage() != Func::ExternalLinkage); // We can't export other imports right now because the names blow up
      context.tables[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.tables[e.index]->setName(MergeName(context.m.name.str(), e.name.str()));
      break;
    case WASM_KIND_MEMORY:
      assert(context.memories[e.index]->getLinkage() != Func::ExternalLinkage); // We can't export other imports right now because the names blow up
      context.memories[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.memories[e.index]->setName(MergeName(context.m.name.str(), e.name.str()));
      break;
    case WASM_KIND_GLOBAL:
      assert(context.globals[e.index]->getLinkage() != Func::ExternalLinkage); // We can't export other imports right now because the names blow up
      context.globals[e.index]->setLinkage(llvm::GlobalValue::ExternalLinkage);
      context.globals[e.index]->setName(MergeName(context.m.name.str(), e.name.str()));
      break;
    }
  }

  // Process data section by appending to the init function
  for(varuint32 i = 0; i < context.m.data.n_data; ++i)
  {
    DataInit& d = context.m.data.data[i]; // First we declare a constant array that stores the data in the EXE
    auto data = llvm::ConstantDataArray::get(context.context, llvm::makeArrayRef<uint8_t>(d.data.get(), d.data.get() + d.data.size()));
    auto val = new llvm::GlobalVariable(*context.llvm, data->getType(), true, llvm::GlobalValue::LinkageTypes::PrivateLinkage, data);
    llvm::Constant* offset = CompileInitConstant(d.offset, context);

    // Then we create a memcpy call that copies this data to the appropriate location in the init function
    context.builder.CreateCall(fn_memcpy,
      {
        context.builder.CreateInBoundsGEP(context.builder.getInt8Ty(), context.builder.CreateLoad(context.memories[d.index]), offset),
        context.builder.CreateInBoundsGEP(data->getType(), val, {context.builder.getInt32(0), context.builder.getInt32(0)}),
        context.builder.getInt64(GetTotalSize(data->getType()))
      });
  }

  // Process element section by appending to the init function
  for(varuint32 i = 0; i < context.m.element.n_elements; ++i)
  {
    TableInit& e = context.m.element.elements[i]; // First we declare a constant array that stores the data in the EXE
    TableDesc* t = ModuleTable(context.m, e.index);
    if(!t)
      return assert(false), ERR_INVALID_TABLE_INDEX;

    if(t->element_type == TE_anyfunc)
    {
      llvmTy* target = GetType(TE_anyfunc, context);
      llvm::Constant* offset = CompileInitConstant(e.offset, context);

      // Go through and resolve all indices to function pointers
      for(varuint32 j = 0; j < e.n_elems; ++j)
      {
        if(e.elems[j] >= context.functions.size())
          return assert(false), ERR_INVALID_FUNCTION_INDEX;

        // Store function pointer in correct table memory location
        auto ptr = context.builder.CreateGEP(context.builder.CreateLoad(context.tables[e.index]), context.builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)));
        context.builder.CreateAlignedStore(context.builder.CreatePointerCast(context.functions[e.elems[j]].internal, target), ptr, context.llvm->getDataLayout().getPointerSize());

        if(env->flags&ENV_STRICT)
        {
          ptr = context.builder.CreateGEP(context.builder.CreateLoad(context.tabletypes[e.index]), context.builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)));
          varuint32 index = ModuleFunctionType(context.m, e.elems[j]);
          if(index == (varuint32)~0)
            return assert(false), ERR_INVALID_FUNCTION_INDEX;

          context.builder.CreateAlignedStore(context.builder.getInt32(index), ptr, 4);
        }
      }
    }
  }

  // Terminate init function
  context.builder.CreateRetVoid();

  // Generate code for each function body
  for(varuint32 i = 0; i < context.m.code.n_funcbody; ++i)
  {
    assert(!context.functions[code_index].imported);
    Func* fn = context.functions[code_index++].internal;
    IR_ERROR err;

    if(fn)
    {
      if(context.m.code.funcbody[i].debug_name.str()) // If there is a debug name, use it, but always number the function because names are not required to be unique
        fn->setName(Twine(context.m.code.funcbody[i].debug_name.str()) + ":" + std::to_string(i + context.m.importsection.functions));
      if(context.m.function.funcdecl[i] >= context.m.type.n_functions)
        return assert(false), ERR_INVALID_TYPE_INDEX;
      if((err = CompileFunctionBody(fn, context.m.type.functions[context.m.function.funcdecl[i]], context.m.code.funcbody[i], context)) < 0)
        return err;
    }
  }

  // If the start section exists, lift the start function to the context so our environment knows about it.
  if(context.m.knownsections & (1 << WASM_SECTION_START))
  {
    if(context.m.start >= context.functions.size())
      return assert(false), ERR_INVALID_START_FUNCTION;
    assert(!context.functions[context.m.start].imported);
    context.start = context.functions[context.m.start].internal;
  }

  // Generate intrinsic function bodies for this module
  for(auto& v : code::intrinsics)
  {
    if(v.fn)
    {
      (*v.gen)(v.fn, context);
      v.fn = 0;
    }
  }

  {
    std::error_code EC;
    llvm::raw_fd_ostream dest(string(context.llvm->getName()) + ".llvm", EC, llvm::sys::fs::F_None);
    context.llvm->print(dest, nullptr);
  }

  // Verify module
  std::error_code EC;
  llvm::raw_fd_ostream dest(1, false, true);
  if(llvm::verifyModule(*context.llvm, &dest))
    return ERR_FATAL_INVALID_MODULE;

  return ERR_SUCCESS;
}

IR_ERROR OutputObjectFile(code::Context& ctx, const char* out)
{
  std::error_code EC;
  llvm::raw_fd_ostream dest(out, EC, llvm::sys::fs::F_None);

  if(EC)
  {
    llvm::errs() << "Could not open file: " << EC.message();
    return assert(false), ERR_FATAL_FILE_ERROR;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::TargetMachine::CGFT_ObjectFile;

  if(ctx.machine->addPassesToEmitFile(pass, dest, FileType))
  {
    llvm::errs() << "TheTargetMachine can't emit a file of this type";
    return assert(false), ERR_FATAL_FILE_ERROR;
  }

  pass.run(*ctx.llvm);
  dest.flush();
  return ERR_SUCCESS;
}

namespace innative {
  IR_ERROR CompileEnvironment(Environment* env, const char* filepath)
  {
    Path file(filepath);
    Path workdir = GetWorkingDir();
    Path programpath = GetProgramPath();
    SetWorkingDir(programpath.BaseDir().Get().c_str());
    utility::DeferLambda<std::function<void()>> defer([&]() { SetWorkingDir(workdir.Get().c_str()); });

    if(!file.IsAbsolute())
      file = workdir + file;

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder(context);
    code::Context* start = nullptr;
    IR_ERROR err = ERR_SUCCESS;
    code::Context* ctx = tmalloc<code::Context>(env->n_modules);
    string triple = llvm::sys::getDefaultTargetTriple();

    // Set up our target architecture, necessary up here so our code generation knows how big a pointer is
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    string Error;
    auto arch = llvm::TargetRegistry::lookupTarget(triple, Error);

    if(!arch)
    {
      llvm::errs() << Error;
      return assert(false), ERR_FATAL_UNKNOWN_TARGET;
    }

    auto CPU = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto machine = arch->createTargetMachine(triple, CPU, Features, opt, RM);

    for(varuint32 i = 0; i < env->n_modules; ++i)
    {
      new(ctx + i) code::Context{ *env, env->modules[i], context, 0, builder, machine };
      if((err = CompileModule(env, ctx[i])) < 0)
        return err;

      // If module has a start function, create a main entry point function
      if(ctx[i].start != nullptr)
      {
        if(start != nullptr)
          return assert(false), ERR_MULTIPLE_ENTRY_POINTS;
        start = ctx + i;
      }
    }

    uint64_t eflags = env->flags;

    if(!env->n_modules)
      return ERR_FATAL_INVALID_MODULE;

    // Initialize all modules and call start function (if it exists)
    if(!start) // We always create an initialization function, even for DLLs, to do proper initialization of modules
      start = &ctx[0];
    if(start->start == nullptr)
      eflags |= ENV_DLL; // We can't compile an EXE without an entry point

    FuncTy* mainTy = FuncTy::get(builder.getVoidTy(), false);
#ifdef IR_PLATFORM_WIN32
    if(eflags&ENV_DLL)
      mainTy = FuncTy::get(builder.getInt32Ty(), { builder.getInt8PtrTy(), start->intptrty, builder.getInt8PtrTy() }, false);
#endif

    Func* main = Func::Create(mainTy, Func::ExternalLinkage, IR_INIT_FUNCTION);
#ifdef IR_PLATFORM_WIN32
    if(eflags&ENV_DLL)
      main->setCallingConv(llvm::CallingConv::X86_StdCall);
#endif

    BB* initblock = BB::Create(context, "start_entry", main);
    builder.SetInsertPoint(initblock);
    for(size_t i = 0; i < env->n_modules; ++i)
    {
      if(ctx + i == start)
        builder.CreateCall(ctx[i].init, {});
      else
      {
        Func* stub = Func::Create(ctx[i].init->getFunctionType(),
          ctx[i].init->getLinkage(),
          ctx[i].init->getName(),
          start->llvm); // Create function prototype in main module
        builder.CreateCall(stub, {});
      }
    }
    if(start->start != nullptr)
      builder.CreateCall(start->start, {});

#ifdef IR_PLATFORM_WIN32
    if(eflags&ENV_DLL)
      builder.CreateRet(builder.getInt32(1));
    else
#endif
      builder.CreateRetVoid();

    start->llvm->getFunctionList().push_back(main);

    // Reverify module
    std::error_code EC;
    llvm::raw_fd_ostream dest(1, false, true);
    if(llvm::verifyModule(*start->llvm, &dest))
      return assert(false), ERR_FATAL_INVALID_MODULE;
    llvm::raw_fd_ostream dest2(string(start->llvm->getName()) + ".llvm", EC, llvm::sys::fs::F_None);
    start->llvm->print(dest2, nullptr);

    // Annotate functions
    AnnotateFunctions(env, ctx);

    // Optimize all modules
    for(size_t i = 0; i < env->n_modules; ++i)
    {
    }

    {
      // Write all in-memory environments to cache files
      vector<string> cache;
      vector<const char*> linkargs = { "innative", "/ERRORREPORT:QUEUE", "/INCREMENTAL:NO", "/NOLOGO", "/nodefaultlib", "/MANIFEST", "/MANIFEST:embed", "/SUBSYSTEM:CONSOLE", "/VERBOSE", "/LARGEADDRESSAWARE", "/OPT:REF", "/OPT:ICF", "/STACK:10000000", "/DYNAMICBASE", "/NXCOMPAT", "/MACHINE:X64", "/machine:x64" };

#ifdef IR_PLATFORM_WIN32
      linkargs.push_back("/ENTRY:" IR_INIT_FUNCTION);
#else
      linkargs.push_back("-init " IR_INIT_FUNCTION); // https://stackoverflow.com/questions/9759880/automatically-executed-functions-when-loading-shared-libraries
#endif

      if(eflags&ENV_DLL)
        linkargs.push_back("/DLL");
      if(eflags&ENV_DEBUG)
        linkargs.push_back("/DEBUG");

      for(size_t i = 0; i < env->n_modules; ++i)
      {
        for(auto& f : ctx[i].functions)
          if(f.exported != nullptr)
            cache.push_back(("/EXPORT:" + f.exported->getName()).str().c_str());

        for(auto& t : ctx[i].tables)
          if(t->getLinkage() == llvm::GlobalValue::ExternalLinkage)
            cache.push_back(("/EXPORT:" + t->getName() + ",DATA").str().c_str());

        for(auto& m : ctx[i].memories)
          if(m->getLinkage() == llvm::GlobalValue::ExternalLinkage)
            cache.push_back(("/EXPORT:" + m->getName() + ",DATA").str().c_str());

        for(auto& g : ctx[i].globals)
          if(g->getLinkage() == llvm::GlobalValue::ExternalLinkage)
            cache.push_back(("/EXPORT:" + g->getName() + ",DATA").str().c_str());
      }

      for(Embedding* cur = env->embeddings; cur != nullptr; cur = cur->next)
      {
        if(cur->size > 0) // If the size is greater than 0, this is an in-memory embedding
        {
          cache.push_back(std::to_string(reinterpret_cast<size_t>(cur)) + IR_ENV_EXTENSION + ".lib");
          FILE* f;
          FOPEN(f, cache.back().c_str(), "wb");
          if(!f)
            return assert(false), ERR_FATAL_FILE_ERROR;

          fwrite(cur->data, 1, cur->size, f);
          fclose(f);
        }
        else
          linkargs.push_back((const char*)cur->data);
      }
      for(auto& v : cache)
        linkargs.push_back(v.c_str());

      vector<string> targets = { string("/OUT:") + file.Get(), "/LIBPATH:" + programpath.BaseDir().Get(), "/LIBPATH:" + workdir.Get() };

      // Generate object code
      for(size_t i = 0; i < env->n_modules; ++i)
      {
        assert(ctx[i].m.name.get() != 0);
        targets.emplace_back(MergeStrings(ctx[i].m.name.str(), ".o"));
        OutputObjectFile(ctx[i], targets.back().c_str());
      }

      for(auto& v : targets)
        linkargs.push_back(v.c_str());

      // Link object code
      llvm::raw_fd_ostream dest(1, false, true);
      if(!lld::coff::link(linkargs, false, dest))
      {
        for(auto& v : cache)
          std::remove(v.c_str());
        return assert(false), ERR_FATAL_LINK_ERROR;
      }
      // Delete cache files
      for(auto& v : cache)
        std::remove(v.c_str());
    }

    return ERR_SUCCESS;
  }
}