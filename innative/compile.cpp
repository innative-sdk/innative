// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#pragma warning(push)
#pragma warning(disable : 4146 4267 4141 4244 4624)
#define _SCL_SECURE_NO_WARNINGS
#include "util.h"
#include "validate.h"
#include "optimize.h"
#include "innative/export.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/MC/SubtargetFeature.h"
#include "lld/Common/Driver.h"
#include <iostream>
#include <sstream>
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
Func* fn_memdump;

llvmTy* GetLLVMType(varsint7 type, code::Context& context)
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
  return nullptr;
}

WASM_TYPE_ENCODING GetTypeEncoding(llvmTy* t)
{
  if(t->isFloatTy())
    return TE_f32;
  if(t->isDoubleTy())
    return TE_f64;
  if(t->isVoidTy())
    return TE_void;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 32)
    return TE_i32;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 64)
    return TE_i64;
  return TE_NONE;
}

FuncTy* GetFunctionType(FunctionType& signature, code::Context& context)
{
  if(signature.n_returns > 1)
    return nullptr;
  llvmTy* ret = (signature.n_returns > 0) ? GetLLVMType(signature.returns[0], context) : llvmTy::getVoidTy(context.context);

  if(signature.n_params > 0)
  {
    vector<llvmTy*> args;
    for(varuint32 i = 0; i < signature.n_params; ++i)
      args.push_back(GetLLVMType(signature.params[i], context));

    return FuncTy::get(ret, args, false);
  }
  return FuncTy::get(ret, false);
}

llvm::DIType* CreateDebugType(llvmTy* t, code::Context& context)
{
  if(t->isPointerTy())
    return context.dbuilder->createPointerType(CreateDebugType(t->getPointerElementType(), context), context.intptrty->getBitWidth());
  if(t->isFloatTy())
    return context.diF32;
  if(t->isDoubleTy())
    return context.diF64;
  if(t->isVoidTy())
    return context.diVoid;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 1)
    return context.diI1;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 32)
    return context.diI32;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 64)
    return context.diVoid;

  return nullptr;
}

void FunctionDebugInfo(llvm::Function* fn, code::Context& context, bool definition, size_t line)
{
  llvm::SmallVector<llvm::Metadata*, 8> dwarfTys = { CreateDebugType(fn->getReturnType(), context) };
  for(auto& arg : fn->args())
    dwarfTys.push_back(CreateDebugType(arg.getType(), context));

  auto subtype = context.dbuilder->createSubroutineType(
    context.dbuilder->getOrCreateTypeArray(dwarfTys),
    llvm::DINode::FlagZero,
    (fn->getCallingConv() == llvm::CallingConv::C) ? llvm::dwarf::DW_CC_normal : llvm::dwarf::DW_CC_nocall);

  fn->setSubprogram(context.dbuilder->createFunction(
    context.dunit,
    fn->getName(),
    "",
    context.dunit,
    line,
    subtype,
    !fn->hasValidDeclarationLinkage(),
    definition,
    line,
    definition ? llvm::DINode::FlagZero : llvm::DINode::FlagFwdDecl,
    context.env.optimize != 0));
}

Func* CompileFunction(FunctionType& signature, const Twine& name, code::Context& context)
{
  Func* fn = Func::Create(GetFunctionType(signature, context), ((context.env.flags & ENV_DEBUG) ? Func::ExternalLinkage : Func::InternalLinkage), name, context.llvm);
  fn->setCallingConv(llvm::CallingConv::Fast);
  return fn;
}

Func* HomogenizeFunction(Func* fn, const Twine& name, code::Context& context, llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv = llvm::CallingConv::C)
{
  vector<llvmTy*> types; // Replace the entire function with just i64
  for(auto& arg : fn->args())
    types.push_back(context.builder.getInt64Ty());

  Func* wrap = Func::Create(FuncTy::get(context.builder.getInt64Ty(), types, false), linkage, name, context.llvm);
  wrap->setCallingConv(callconv);
  if(context.dbuilder && fn->getSubprogram())
    FunctionDebugInfo(wrap, context, true, fn->getSubprogram()->getLine());

  auto prev = context.builder.GetInsertBlock();

  BB* bb = BB::Create(context.context, "homogenize_block", wrap);
  context.builder.SetInsertPoint(bb);

  if(context.dbuilder && wrap->getSubprogram())
    context.builder.SetCurrentDebugLocation(llvm::DILocation::get(context.context, wrap->getSubprogram()->getLine(), 0, wrap->getSubprogram()));

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

Func* WrapFunction(Func* fn, const Twine& name, code::Context& context, llvm::GlobalValue::LinkageTypes linkage = Func::ExternalLinkage, llvm::CallingConv::ID callconv = llvm::CallingConv::Fast)
{
  Func* wrap = Func::Create(fn->getFunctionType(), linkage, name, context.llvm);
  wrap->setCallingConv(callconv);
  if(context.dbuilder && fn->getSubprogram())
    FunctionDebugInfo(wrap, context, true, fn->getSubprogram()->getLine());

  auto prev = context.builder.GetInsertBlock();

  BB* bb = BB::Create(context.context, "wrapper_block", wrap);
  context.builder.SetInsertPoint(bb);

  if(context.dbuilder && wrap->getSubprogram())
    context.builder.SetCurrentDebugLocation(llvm::DILocation::get(context.context, wrap->getSubprogram()->getLine(), 0, wrap->getSubprogram()));

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
  auto ty = static_cast<llvmVal*>(arg)->getType();
  if(ty->isIntegerTy() && ty->getIntegerBitWidth() != 32 && ty->getIntegerBitWidth() != 64 && ty->getIntegerBitWidth() != 1)
    assert(false);
  context.values.Push((ty->isIntegerTy() && ty->getIntegerBitWidth() == 1) ? context.builder.CreateIntCast(arg, context.builder.getInt32Ty(), false) : arg);
  return e;
}

bool CheckType(varsint7 ty, llvmVal* v)
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

bool CheckSig(varsint7 sig, const Stack<llvmVal*>& values)
{
  if(sig == TE_void)
    return !values.Size() || !values.Peek() || values.Peek()->getType()->isVoidTy();
  if(!values.Size())
    return false;
  if(!values.Peek())
    return true;
  return CheckType(sig, values.Peek());
}

IR_ERROR PopType(varsint7 ty, code::Context& context, llvmVal*& v, bool peek = false)
{
  if(ty == TE_void)
    v = 0;
  else if(!context.values.Size())
    return assert(false), ERR_INVALID_VALUE_STACK;
  else if(!context.values.Peek()) // polymorphic value
  {
    switch(ty)
    {
    case TE_i32: v = context.builder.getInt32(0); break;
    case TE_i64: v = context.builder.getInt64(0); break;
    case TE_f32: v = ConstantFP::get(context.builder.getFloatTy(), 0.0f); break;
    case TE_f64: v = ConstantFP::get(context.builder.getDoubleTy(), 0.0f); break;
    default:
      return ERR_INVALID_TYPE;
    }
  }
  else if(!CheckType(ty, context.values.Peek()))
    return ERR_INVALID_TYPE;
  else if(peek)
    v = context.values.Peek();
  else
    v = context.values.Pop();

  return ERR_SUCCESS;
}

// Given a function pointer to the appropriate builder function, pops two binary arguments off the stack and pushes the result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileBinaryOp(code::Context& context, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, context, val2))
    return assert(false), err;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  return PushReturn(context, (context.builder.*op)(val1, val2, args...));
}

// Given an intrinsic function ID, pops two binary arguments off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IR_ERROR CompileBinaryIntrinsic(code::Context& context, llvm::Intrinsic::ID id, const Twine& name)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, context, val2))
    return assert(false), err;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  return PushReturn(context, context.builder.CreateBinaryIntrinsic(id, val1, val2, nullptr, name));
}

// Given a function pointer to the appropriate builder function, pops one unary argument off the stack and pushes the result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileUnaryOp(code::Context& context, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, Args...), Args... args)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val1;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  return PushReturn(context, (context.builder.*op)(val1, args...));
}

// Given an intrinsic function ID, pops one unary argument off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileUnaryIntrinsic(code::Context& context, llvm::Intrinsic::ID id, const Twine& name, Args... args)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val1;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  llvmVal* values[sizeof...(Args) + 1] = { val1, args... };

  Func *fn = llvm::Intrinsic::getDeclaration(context.llvm, id, { val1->getType() });

  return PushReturn(context, context.builder.CreateCall(fn, values, name));
}

IR_ERROR CompileSelectOp(code::Context& context, const Twine& name, llvm::Instruction* from)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *cond, *valf, *valt;
  if(err = PopType(TE_i32, context, cond))
    return assert(false), err;

  if(!context.values.Size())
    return ERR_INVALID_VALUE_STACK;
  if(!context.values.Peek()) // If a polymorphic type is on the stack, the result is a polymorphic type, so just do nothing
    return ERR_SUCCESS;

  valf = context.values.Pop();

  if(err = PopType(GetTypeEncoding(valf->getType()), context, valt))
    return assert(false), err;

  return PushReturn(context, context.builder.CreateSelect(context.builder.CreateICmpNE(cond, context.builder.getInt32(0)), valt, valf, name, from));
}

llvmVal* MaskShiftBits(code::Context& context, llvmVal* value)
{
  // WASM requires that a shift count greater than the bit width of the type is wrapped, which matches x86 behavior but is undefined in LLVM, so we make this explicit. 
  return context.builder.CreateAnd(value, CInt::get(value->getType(), value->getType()->getIntegerBitWidth() - 1));
}

template<WASM_TYPE_ENCODING TYPE, bool LEFT>
IR_ERROR CompileRotationOp(code::Context& context, const char* name)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *count, *value;
  if(err = PopType(TYPE, context, count))
    return assert(false), err;
  if(err = PopType(TYPE, context, value))
    return assert(false), err;

  const int BITS = (TYPE == TE_i32) ? 32 : 64;
  llvmVal *l, *r;
  count = MaskShiftBits(context, count);
  llvmVal* sub = context.builder.CreateSub(CInt::get(context.context, APInt(BITS, BITS, true)), count);

  if(LEFT) // value<<count | value>>(BITS-count);
  {
    l = context.builder.CreateShl(value, count);
    r = context.builder.CreateLShr(value, sub);
  }
  else // value>>count | value<<(BITS-count);
  {
    l = context.builder.CreateLShr(value, count);
    r = context.builder.CreateShl(value, sub);
  }

  return PushReturn(context, context.builder.CreateOr(l, r, name));
}

// BinaryOp for shift functions which has to apply a shift mask to one operand
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IR_ERROR CompileBinaryShiftOp(code::Context& context, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, context, val2))
    return assert(false), err;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  return PushReturn(context, (context.builder.*op)(val1, MaskShiftBits(context, val2), args...));
}

BB* PushLabel(const char* name, varsint7 sig, uint8_t opcode, Func* fnptr, code::Context& context, llvm::DIScope* scope)
{
  BB* bb = BB::Create(context.context, name, fnptr);
  context.control.Push(code::Block{ bb, 0, context.values.Limit(), sig, opcode, scope }); // TODO: create proper nested lexical contexts
  context.values.SetLimit(context.values.Size() + context.values.Limit()); // Set limit to current stack size to prevent a block from popping past this

  if(context.values.Size() > 0 && !context.values.Peek()) // If we're in an unreachable segment, just push another placeholder on to the stack
    context.values.Push(nullptr);

  return bb;
}

BB* BindLabel(BB* block, code::Context& context)
{
  if(block->getParent() != nullptr) // Because this always happens after a branch, even if we have nothing to bind to, we must create a new block for LLVM
    block = BB::Create(context.context, "bind_block", nullptr);

  context.builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(block);
  context.builder.SetInsertPoint(block);
  assert(block->getParent() != nullptr);
  return block;
}

void PushResult(code::BlockResult** root, llvmVal* result, BB* block, const Environment& env)
{
  code::BlockResult* next = *root;
  *root = tmalloc<code::BlockResult>(env, 1);
  new(*root) code::BlockResult{ result, block, next };
}

// Adds current value stack to target branch according to that branch's signature.
IR_ERROR AddBranch(code::Block& target, code::Context& context)
{
  IR_ERROR err = ERR_SUCCESS;
  if(target.sig != TE_void)
  {
    llvmVal* value;
    err = PopType(target.sig, context, value, true);
    if(!err)
      PushResult(&target.results, value, context.builder.GetInsertBlock(), context.env); // Push result
  }
  return err;
}

// Pops a label off the control stack, verifying that the value stack matches the signature and building PHI nodes as necessary
IR_ERROR PopLabel(code::Context& context, BB* block)
{
  varsint7 sig = context.control.Peek().sig;
  llvmVal* push = nullptr;
  if(sig != TE_void)
  {
    IR_ERROR err;
    if(err = PopType(sig, context, push))
      return assert(false), err;
    if(context.control.Peek().results != nullptr) // If there are results from other branches, perform a PHI merge. Otherwise, leave the value stack alone
    {
      unsigned int count = 1; // Start with 1 for our current branch's values 
      for(auto i = context.control.Peek().results; i != nullptr; i = i->next)
        ++count; // Count number of additional results

      llvm::PHINode* phi = context.builder.CreatePHI(push->getType(), count, "phi"); // TODO: Account for multiple return values once they are added
      phi->addIncoming(push, block); // Pop this branches values off value stack, add using proper insert block

      for(auto i = context.control.Peek().results; i != nullptr; i = i->next)
        phi->addIncoming(i->v, i->b);

      push = phi; // Push phi nodes on to stack
    }
  }
  else if(context.control.Peek().results != nullptr)
    return assert(false), ERR_INVALID_VALUE_STACK;

  if(context.values.Size() > 0 && !context.values.Peek()) // Pop at most 1 polymorphic type off the stack.
    context.values.Pop();
  if(context.values.Size() > 0) // value stack should be completely empty now
    return assert(false), ERR_INVALID_VALUE_STACK;

  if(push)
    PushReturn(context, push);

  context.values.SetLimit(context.control.Peek().limit);
  context.control.Pop();

  return ERR_SUCCESS;
}

IR_ERROR CompileIfBlock(varsint7 sig, code::Context& context)
{
  IR_ERROR err;
  llvmVal* cond;

  if(err = PopType(TE_i32, context, cond))
    return assert(false), err;

  llvmVal* cmp = context.builder.CreateICmpNE(cond, context.builder.getInt32(0), "if_cond");

  Func* parent = context.builder.GetInsertBlock()->getParent();
  BB* tblock = BB::Create(context.context, "if_true", parent);
  BB* fblock = BB::Create(context.context, "if_else", parent); // Create else stub
  BB* endblock = PushLabel("if_end", sig, OP_if, nullptr, context, context.control.Peek().scope);
  context.control.Peek().ifelse = fblock;

  context.builder.CreateCondBr(cmp, tblock, fblock); // Insert branch in current block
  context.builder.SetInsertPoint(fblock); // Point else stub at end block
  context.builder.CreateBr(endblock);

  context.builder.SetInsertPoint(tblock); // Start inserting code into true block
  return ERR_SUCCESS;
}

void PolymorphicStack(code::Context& context)
{
  while(context.values.Size() > 0)
    context.values.Pop();
  context.values.Push(nullptr);
  BB* graveyard = BB::Create(context.context, "graveyard", context.builder.GetInsertBlock()->getParent());
  context.builder.SetInsertPoint(graveyard);
}

IR_ERROR CompileElseBlock(code::Context& context)
{
  if(context.control.Size() == 0 || context.control.Peek().op != OP_if)
    return assert(false), ERR_IF_ELSE_MISMATCH;

  context.builder.CreateBr(context.control.Peek().block); // Add a branch-to-merge instruction to our if_true block

  // Instead of popping and pushing a new control label, we just re-purpose the existing one. This preserves the value stack results.
  if(context.control.Peek().sig != TE_void)
  {
    IR_ERROR err;
    llvmVal* value;
    if(err = PopType(context.control.Peek().sig, context, value))
      return assert(false), err;
    PushResult(&context.control.Peek().results, value, context.builder.GetInsertBlock(), context.env); // Push result
  }

  // Reset value stack, but ensure that we preserve a polymorphic value if we had pushed one before
  while(context.values.Size() > 1)
    context.values.Pop();
  if(context.values.Size() > 1 && context.values.Peek() != nullptr)
    context.values.Pop();

  context.control.Peek().op = OP_else; // make this block an OP_else block
  BB* fblock = context.control.Peek().ifelse; // Get stored else block
  fblock->begin()->eraseFromParent(); // Erase stub instruction
  fblock->removeFromParent(); // Required for correct label binding behavior
  BindLabel(fblock, context); // Bind if_false block to current position

  return ERR_SUCCESS;
}

IR_ERROR CompileReturn(code::Context& context, varsint7 sig)
{
  if(sig == TE_void)
    context.builder.CreateRetVoid();
  else
  {
    llvmVal* val;
    IR_ERROR err = PopType(sig, context, val);
    if(err)
      return assert(false), err;

    context.builder.CreateRet(val);
  }

  return ERR_SUCCESS;
}

IR_ERROR CompileEndBlock(code::Context& context)
{
  BB* cur = context.builder.GetInsertBlock();
  if(context.control.Peek().block->getParent() != nullptr) // If the label wasn't bound, branch to the new one we create
  {
    BB* block = BindLabel(context.control.Peek().block, context);
    context.builder.SetInsertPoint(cur);
    context.builder.CreateBr(block);
    context.builder.SetInsertPoint(block);
  }
  else
  {
    context.builder.CreateBr(context.control.Peek().block);
    BindLabel(context.control.Peek().block, context);
  }

  auto cache = context.control.Peek();
  switch(cache.op) // Verify source operation
  {
  case OP_if:
    if(context.control.Peek().sig != TE_void) // An if statement with no else statement cannot return a value
      return assert(false), ERR_EXPECTED_ELSE_INSTRUCTION;
  case OP_else:
  case OP_block:
  case OP_loop:
  case OP_return:
    break;
  default:
    return assert(false), ERR_END_MISMATCH;
  }

  IR_ERROR err = PopLabel(context, cur); // Pop the label to assemble the phi node before pushing it.
  if(cache.op == OP_return)
    CompileReturn(context, cache.sig);

  return err;
}

void CompileTrap(code::Context& context)
{
  auto call = context.builder.CreateCall(llvm::Intrinsic::getDeclaration(context.llvm, llvm::Intrinsic::trap), { });
  call->setDoesNotReturn();
  context.builder.CreateUnreachable();
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
  IR_ERROR err = (target.op != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
  PolymorphicStack(context);
  return err;
}

IR_ERROR CompileIfBranch(varuint32 depth, code::Context& context)
{
  if(depth >= context.control.Size())
    return assert(false), ERR_INVALID_BRANCH_DEPTH;

  IR_ERROR err;
  llvmVal* cond;
  if(err = PopType(TE_i32, context, cond))
    return assert(false), err;

  llvmVal* cmp = context.builder.CreateICmpNE(cond, context.builder.getInt32(0), "br_if_cond");

  // Because llvm requires explicit branches, we have to create a new block and append it to our current one
  BB* block = BB::Create(context.context, "br_if_cont", context.builder.GetInsertBlock()->getParent());

  code::Block& target = context.control[depth];
  context.builder.CreateCondBr(cmp, target.block, block);
  if(target.op != OP_loop)
  {
    if(err = AddBranch(target, context))
      return err;
  }
  context.builder.SetInsertPoint(block); // Start inserting code into continuation AFTER we add the branch, so the branch goes to the right place
  return ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}
IR_ERROR CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def, code::Context& context)
{
  IR_ERROR err;
  llvmVal* index;
  if(err = PopType(TE_i32, context, index))
    return assert(false), err;

  if(def >= context.control.Size())
    return assert(false), ERR_INVALID_BRANCH_DEPTH;

  llvm::SwitchInst* s = context.builder.CreateSwitch(index, context.control[def].block, n_table);
  err = (context.control[def].op != OP_loop) ? AddBranch(context.control[def], context) : ERR_SUCCESS;

  for(varuint32 i = 0; i < n_table && err == ERR_SUCCESS; ++i)
  {
    if(table[i] >= context.control.Size())
      return assert(false), ERR_INVALID_BRANCH_DEPTH;

    code::Block& target = context.control[table[i]];
    s->addCase(context.builder.getInt32(i), target.block);
    err = (target.op != OP_loop) ? AddBranch(target, context) : ERR_SUCCESS;
  }

  PolymorphicStack(context);
  return err;
}

IR_ERROR CompileCall(varuint32 index, code::Context& context)
{
  if(index >= context.functions.size())
    return assert(false), ERR_INVALID_FUNCTION_INDEX;

  // Because this is a static function call, we can call the imported C function directly with the appropriate calling convention.
  Func* fn = (!context.functions[index].imported) ? (context.functions[index].internal) : context.functions[index].imported;
  unsigned int num = fn->getFunctionType()->getNumParams();

  // Pop arguments in reverse order
  IR_ERROR err;
  llvmVal** ArgsV = tmalloc<llvmVal*>(context.env, num);
  for(unsigned int i = num; i-- > 0;)
  {
    if(err = PopType(GetTypeEncoding(fn->getFunctionType()->getParamType(i)), context, ArgsV[i]))
      return assert(false), err;
  }

  CallInst* call = context.builder.CreateCall(fn, llvm::makeArrayRef(ArgsV, num));
  if(context.env.flags & ENV_DISABLE_TAIL_CALL) // In strict mode, tail call optimization is not allowed
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
      { CInt::get(context.builder.getInt32Ty(), -1, true) }));
}

// Gets the first type index that matches the given type, used as a stable type hash
varuint32 GetFirstType(varuint32 type, code::Context& context)
{
  if(type < context.m.type.n_functions)
  {
    for(varuint32 i = 0; i < type; ++i)
      if(MatchFunctionType(context.m.type.functions[i], context.m.type.functions[type]))
        return i;
  }

  return type;
}

IR_ERROR CompileIndirectCall(varuint32 index, code::Context& context)
{
  index = GetFirstType(index, context);
  if(index >= context.m.type.n_functions)
    return assert(false), ERR_INVALID_TYPE_INDEX;

  IR_ERROR err;
  FunctionType& ftype = context.m.type.functions[index];
  llvmVal* callee;
  if(err = PopType(TE_i32, context, callee))
    return assert(false), err;

  if(context.tables.size() < 1)
    return assert(false), ERR_INVALID_TABLE_INDEX;

  // Pop arguments in reverse order
  llvmVal** ArgsV = tmalloc<llvmVal*>(context.env, ftype.n_params);
  for(unsigned int i = ftype.n_params; i-- > 0;)
  {
    if(err = PopType(ftype.params[i], context, ArgsV[i]))
      return assert(false), err;
  }

  uint64_t bytewidth = context.llvm->getDataLayout().getTypeAllocSize(context.tables[0]->getType()->getElementType()->getPointerElementType());

  if(context.env.flags & ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if index is out of bounds
  {
    InsertConditionalTrap(
      context.builder.CreateICmpUGE(context.builder.CreateIntCast(callee, context.builder.getInt64Ty(), false),
        context.builder.CreateUDiv(GetMemSize(context.tables[0], context), context.builder.getInt64(bytewidth)),
        "indirect_call_oob_check"),
      context);
  }

  // Deference global variable to get the actual array of function pointers, index into them, then dereference that array index to get the actual function pointer
  llvmVal* funcptr = context.builder.CreateLoad(context.builder.CreateGEP(context.builder.CreateLoad(context.tables[0]), { callee, context.builder.getInt32(0) }), "indirect_call_load_func_ptr");

  if(context.env.flags & ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if function pointer is NULL
    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(funcptr, context.intptrty), CInt::get(context.intptrty, 0), "indirect_call_null_check"), context);

  // Now that we have the function pointer we have to actually cast back to the function signature that we expect, instead of void()
  FuncTy* ty = GetFunctionType(ftype, context);
  funcptr = context.builder.CreatePointerCast(funcptr, ty->getPointerTo(0));

  if(context.env.flags & ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if the expected type does not match the actual type of the function
  {
    auto sig = context.builder.CreateLoad(context.builder.CreateGEP(context.builder.CreateLoad(context.tables[0]), { callee, context.builder.getInt32(1) }));
    InsertConditionalTrap(context.builder.CreateICmpNE(sig, context.builder.getInt32(index), "indirect_call_sig_check"), context);
  }

  // CreateCall will then do the final dereference of the function pointer to make the indirect call
  CallInst* call = context.builder.CreateCall(funcptr, llvm::makeArrayRef(ArgsV, ftype.n_params));

  if(context.env.flags & ENV_DISABLE_TAIL_CALL) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(CallInst::TCK_NoTail);
  call->setCallingConv(llvm::CallingConv::Fast); // Always pick the fast convention, because the table is always set to the internal wrapping function

  if(!ty->getReturnType()->isVoidTy()) // Only push a value if there is one to push
    return PushReturn(context, call);
  return ERR_SUCCESS;
}

IR_ERROR CompileConstant(Instruction& instruction, code::Context& context, llvm::Constant*& constant)
{
  switch(instruction.opcode)
  {
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
    constant = CInt::get(context.context, APInt(32, instruction.immediates[0]._varuint32, true));
    break;
  case OP_i64_const:
    constant = CInt::get(context.context, APInt(64, instruction.immediates[0]._varuint64, true));
    break;
  case OP_f32_const:
    constant = ConstantFP::get(context.context, APFloat(instruction.immediates[0]._float32));
    break;
  case OP_f64_const:
    constant = ConstantFP::get(context.context, APFloat(instruction.immediates[0]._float64));
    break;
  default:
    return ERR_INVALID_INITIALIZER;
  }

  return ERR_SUCCESS;
}

llvmVal* GetMemPointer(code::Context& context, llvmVal* base, llvm::PointerType* pointer_type, varuint7 memory, varuint32 offset)
{
  llvmVal* loc = context.builder.CreateAdd(context.builder.CreateIntCast(base, context.builder.getInt64Ty(), false), context.builder.getInt64(offset), "", true, true);

  if(context.env.flags&ENV_CHECK_MEMORY_ACCESS) // In strict mode, generate a check that traps if this is an invalid memory access
    InsertConditionalTrap(
      context.builder.CreateICmpUGT(
        context.builder.CreateAdd(loc, context.builder.getInt64(pointer_type->getPointerElementType()->getPrimitiveSizeInBits() / 8)),
        GetMemSize(context.memories[memory], context),
        "invalid_mem_access_cond"),
      context);

  return context.builder.CreatePointerCast(context.builder.CreateGEP(context.builder.CreateLoad(context.memories[memory]), loc), pointer_type);
}

template<bool SIGNED>
IR_ERROR CompileLoad(code::Context& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvmTy* ext, llvmTy* ty)
{
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  llvmVal* base;
  IR_ERROR err;
  if(err = PopType(TE_i32, context, base))
    return assert(false), err;

  llvmVal* result = context.builder.CreateAlignedLoad(GetMemPointer(context, base, ty->getPointerTo(0), memory, offset), (1 << memflags), name);

  if(ext != nullptr)
    result = SIGNED ? context.builder.CreateSExt(result, ext) : context.builder.CreateZExt(result, ext);

  return PushReturn(context, result);
}

template<WASM_TYPE_ENCODING TY>
IR_ERROR CompileStore(code::Context& context, varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvm::IntegerType* ext)
{
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  IR_ERROR err;
  llvmVal *value, *base;
  if(err = PopType(TY, context, value))
    return assert(false), err;
  if(err = PopType(TE_i32, context, base))
    return assert(false), err;

  llvmTy* PtrType = !ext ? GetLLVMType(TY, context) : ext;

  llvmVal* ptr = GetMemPointer(context, base, PtrType->getPointerTo(0), memory, offset);
  llvmVal* result = context.builder.CreateAlignedStore(!ext ? value : context.builder.CreateIntCast(value, ext, false), ptr, (1 << memflags), name);

  //context.builder.CreateCall(fn_memdump, { context.builder.CreateLoad(context.memories[0]), GetMemSize(context.memories[0], context) });

  return ERR_SUCCESS;
}

// Gets memory size in pages, not bytes
llvmVal* CompileMemSize(llvm::GlobalVariable* target, code::Context& context)
{
  return context.builder.CreateIntCast(context.builder.CreateLShr(GetMemSize(target, context), 16), context.builder.getInt32Ty(), true);
}

IR_ERROR CompileMemGrow(code::Context& context, const char* name)
{
  if(context.memories.size() < 1)
    return assert(false), ERR_INVALID_MEMORY_INDEX;

  IR_ERROR err;
  llvmVal *delta;
  if(err = PopType(TE_i32, context, delta))
    return assert(false), err;

  llvmVal* old = CompileMemSize(context.memories[0], context);

  auto max = context.builder.getInt64(uint64_t((context.m.memory.memories[0].limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? context.m.memory.memories[0].limits.maximum : 0x10000) << 16);
  CallInst* call = context.builder.CreateCall(context.memgrow, { context.builder.CreateLoad(context.memories[0]), context.builder.CreateShl(context.builder.CreateZExt(delta, context.builder.getInt64Ty()), 16), max }, name);

  llvmVal* success = context.builder.CreateICmpNE(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0));

  auto oldblock = context.builder.GetInsertBlock();
  auto successblock = BB::Create(context.context, "grow_success", context.builder.GetInsertBlock()->getParent());
  auto contblock = BB::Create(context.context, "grow_fail", context.builder.GetInsertBlock()->getParent());

  context.builder.CreateCondBr(success, successblock, contblock);
  context.builder.SetInsertPoint(successblock); // Only set new memory if call succeeded
  context.builder.CreateAlignedStore(call, context.memories[0], context.builder.getInt64Ty()->getPrimitiveSizeInBits() / 8);
  context.builder.CreateBr(contblock);

  context.builder.SetInsertPoint(contblock);

  auto phi = context.builder.CreatePHI(context.builder.getInt32Ty(), 2);
  phi->addIncoming(old, successblock);
  phi->addIncoming(CInt::get(context.builder.getInt32Ty(), -1, true), oldblock);

  return PushReturn(context, phi);
}

template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IR_ERROR CompileSRem(code::Context& context, const Twine& name)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, context, val2))
    return assert(false), err;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;
  
  // The specific case of INT_MIN % -1 is undefined behavior in LLVM and crashes on x86, but WASM requires that it return 0, so we branch on that specific case.
  llvm::Value* cond = context.builder.CreateAnd(
    context.builder.CreateICmpEQ(val1, (val1->getType()->getIntegerBitWidth() == 32) ? context.builder.getInt32(0x80000000) : context.builder.getInt64(0x8000000000000000)),
    context.builder.CreateICmpEQ(val2, CInt::get(val2->getType(), ~0ULL, true)));
  return PushReturn(context, context.builder.CreateSRem(context.builder.CreateSelect(cond, CInt::get(val1->getType(), 0, true), val1), val2, name));
}

IR_ERROR InsertTruncTrap(code::Context& context, double max, double min, llvm::Type* ty)
{
  if((context.env.flags & ENV_CHECK_FLOAT_TRUNC) && context.values.Size() > 0 && context.values.Peek() != nullptr)
  {
    return InsertConditionalTrap(context.builder.CreateOr(
      (ty->isFloatTy()
        ? context.builder.CreateICmpEQ(context.builder.CreateAnd(context.builder.CreateBitCast(context.values.Peek(), context.builder.getInt32Ty()), context.builder.getInt32(0x7F800000)), context.builder.getInt32(0x7F800000))
        : context.builder.CreateICmpEQ(context.builder.CreateAnd(context.builder.CreateBitCast(context.values.Peek(), context.builder.getInt64Ty()), context.builder.getInt64(0x7FF0000000000000)), context.builder.getInt64(0x7FF0000000000000))),
      context.builder.CreateOr(
        context.builder.CreateFCmpOGT(context.values.Peek(), ConstantFP::get(ty, max)),
        context.builder.CreateFCmpOLT(context.values.Peek(), ConstantFP::get(ty, min)))),
      context);
  }
  return ERR_SUCCESS;
};

void DumpContextState(code::Context& context)
{
  std::cout << "values: [";

  size_t total = context.values.Size() + context.values.Limit();
  for(size_t i = 0; i < total; ++i)
  {
    if(i == context.values.Limit())
      std::cout << " |";
    if(!context.values[i])
      std::cout << " Poly";
    else
      switch(GetTypeEncoding(context.values[i]->getType()))
      {
      case TE_i32: std::cout << " i32"; break;
      case TE_i64: std::cout << " i64"; break;
      case TE_f32: std::cout << " f32"; break;
      case TE_f64: std::cout << " f64"; break;
      }
  }

  std::cout << " ]" << std::endl;
  std::cout << "control: [";

  for(size_t i = 0; i < context.control.Size(); ++i)
  {
    switch(context.control[i].sig)
    {
    case TE_i32: std::cout << " i32"; break;
    case TE_i64: std::cout << " i64"; break;
    case TE_f32: std::cout << " f32"; break;
    case TE_f64: std::cout << " f64"; break;
    }

    std::cout << ":" << (int)context.control[i].op;
  }

  std::cout << " ]\n" << std::endl;
}

template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IR_ERROR CompileFloatCmp(code::Context& context, llvm::Intrinsic::ID id, const Twine& name)
{
  IR_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, context, val2))
    return assert(false), err;
  if(err = PopType(Ty1, context, val1))
    return assert(false), err;

  // WASM requires we return an NaN if either operand is NaN
  auto nancheck = context.builder.CreateFCmpUNO(val1, val2);
  auto compare = context.builder.CreateBinaryIntrinsic(id, val1, val2, nullptr, name);
  return PushReturn(context, context.builder.CreateSelect(nancheck, llvm::ConstantFP::getNaN(val1->getType()), compare));
}

IR_ERROR CompileInstruction(Instruction& ins, code::Context& context)
{
  //std::cout << OPNAMES[ins.opcode] << std::endl;
  //DumpContextState(context);

  switch(ins.opcode)
  {
  case OP_unreachable:
    CompileTrap(context); // Automatically terminates block as unreachable
    PolymorphicStack(context);
    return ERR_SUCCESS;
  case OP_nop:
    return ERR_SUCCESS;
  case OP_block:
    PushLabel("block", ins.immediates[0]._varsint7, OP_block, nullptr, context, context.control.Peek().scope);
    return ERR_SUCCESS;
  case OP_loop:
    PushLabel("loop", ins.immediates[0]._varsint7, OP_loop, nullptr, context, context.control.Peek().scope);
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
  {
    IR_ERROR err = CompileReturn(context, context.control[context.control.Size() - 1].sig);
    PolymorphicStack(context);
    return err;
  }

  // Call operators
  case OP_call:
    return CompileCall(ins.immediates[0]._varuint32, context);
  case OP_call_indirect:
    return CompileIndirectCall(ins.immediates[0]._varuint32, context);

    // Parametric operators
  case OP_drop:
    if(context.values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    if(context.values.Peek() != nullptr)
      context.values.Pop(); // We do not delete the value because it could be referenced elsewhere (e.g. in a branch)
    return ERR_SUCCESS;
  case OP_select:
    return CompileSelectOp(context, OPNAMES[ins.opcode], nullptr);

    // Variable access
  case OP_get_local:
    if(ins.immediates[0]._varuint32 >= context.locals.size())
      return assert(false), ERR_INVALID_LOCAL_INDEX;
    PushReturn(context, context.builder.CreateLoad(context.locals[ins.immediates[0]._varuint32]));
    return ERR_SUCCESS;
  case OP_set_local:
  case OP_tee_local:
  {
    if(ins.immediates[0]._varuint32 >= context.locals.size())
      return assert(false), ERR_INVALID_LOCAL_INDEX;
    if(context.values.Size() < 1)
      return assert(false), ERR_INVALID_VALUE_STACK;
    context.builder.CreateStore(!context.values.Peek() ? llvm::Constant::getAllOnesValue(context.locals[ins.immediates[0]._varuint32]->getType()->getElementType()) : context.values.Peek(), context.locals[ins.immediates[0]._varuint32]);
    if(context.values.Peek() != nullptr && ins.opcode == OP_set_local) // tee_local is the same as set_local except the operand isn't popped
      context.values.Pop();
    return ERR_SUCCESS;
  }
  case OP_set_global:
    if(ins.immediates[0]._varuint32 >= context.globals.size())
      return assert(false), ERR_INVALID_GLOBAL_INDEX;
    if(context.values.Size() < 1)
      return assert(false), ERR_INVALID_VALUE_STACK;
    context.builder.CreateStore(!context.values.Peek() ? llvm::Constant::getAllOnesValue(context.globals[ins.immediates[0]._varuint32]->getType()->getElementType()) : context.values.Pop(), context.globals[ins.immediates[0]._varuint32]);
    return ERR_SUCCESS;
  case OP_get_global:
    if(ins.immediates[0]._varuint32 >= context.globals.size())
      return ERR_INVALID_GLOBAL_INDEX;
    PushReturn(context, context.builder.CreateLoad(context.globals[ins.immediates[0]._varuint32]));
    return ERR_SUCCESS;

    // Memory-related operators
  case OP_i32_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr, context.builder.getInt32Ty());
  case OP_i64_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr, context.builder.getInt64Ty());
  case OP_f32_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr, context.builder.getFloatTy());
  case OP_f64_load:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr, context.builder.getDoubleTy());
  case OP_i32_load8_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt8Ty());
  case OP_i32_load8_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt8Ty());
  case OP_i32_load16_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt16Ty());
  case OP_i32_load16_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt32Ty(), context.builder.getInt16Ty());
  case OP_i64_load8_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt8Ty());
  case OP_i64_load8_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt8Ty());
  case OP_i64_load16_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt16Ty());
  case OP_i64_load16_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt16Ty());
  case OP_i64_load32_s:
    return CompileLoad<true>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt32Ty());
  case OP_i64_load32_u:
    return CompileLoad<false>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt64Ty(), context.builder.getInt32Ty());
  case OP_i32_store:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr);
  case OP_i64_store:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr);
  case OP_f32_store:
    return CompileStore<TE_f32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr);
  case OP_f64_store:
    return CompileStore<TE_f64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr);
  case OP_i32_store8:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt8Ty());
  case OP_i32_store16:
    return CompileStore<TE_i32>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt16Ty());
  case OP_i64_store8:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt8Ty());
  case OP_i64_store16:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt16Ty());
  case OP_i64_store32:
    return CompileStore<TE_i64>(context, 0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], context.builder.getInt32Ty());
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
    llvm::Constant* constant;
    IR_ERROR err = CompileConstant(ins, context, constant);
    if(!err)
      PushReturn(context, constant);
    return err;
  }

  // Comparison operators
  case OP_i32_eqz:
    context.values.Push(context.builder.getInt32(0)); // Fallthrough to OP_i32_eq
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
    context.values.Push(context.builder.getInt64(0)); // Fallthrough to OP_i64_eq
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
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::ctlz, OPNAMES[ins.opcode], context.builder.getInt1(false));
  case OP_i32_ctz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(context, llvm::Intrinsic::cttz, OPNAMES[ins.opcode], context.builder.getInt1(false));
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
    return CompileSRem<TE_i32, TE_i32, TE_i32>(context, OPNAMES[ins.opcode]);
  case OP_i32_rem_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i32_and:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i32_or:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i32_xor:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i32_shl:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i32_shr_s:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i32_shr_u:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
  case OP_i32_rotl:
    return CompileRotationOp<TE_i32, true>(context, OPNAMES[ins.opcode]);
  case OP_i32_rotr:
    return CompileRotationOp<TE_i32, false>(context, OPNAMES[ins.opcode]);
  case OP_i64_clz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(context, llvm::Intrinsic::ctlz, OPNAMES[ins.opcode], context.builder.getInt1(false));
  case OP_i64_ctz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(context, llvm::Intrinsic::cttz, OPNAMES[ins.opcode], context.builder.getInt1(false));
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
    return CompileSRem<TE_i64, TE_i64, TE_i64>(context, OPNAMES[ins.opcode]);
  case OP_i64_rem_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateURem, OPNAMES[ins.opcode]);
  case OP_i64_and:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i64_or:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i64_xor:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const Twine&>(context, &llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i64_shl:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const Twine&, bool, bool>(context, &llvm::IRBuilder<>::CreateShl, OPNAMES[ins.opcode], false, false);
  case OP_i64_shr_s:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateAShr, OPNAMES[ins.opcode], false);
  case OP_i64_shr_u:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const Twine&, bool>(context, &llvm::IRBuilder<>::CreateLShr, OPNAMES[ins.opcode], false);
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
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::trunc, OPNAMES[ins.opcode]);
    return CompileUnaryOp<TE_f32, TE_f32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPTrunc, context.builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_nearest: // We must round floats using IEEE 754-2008 roundToIntegralTowardZero, which rint gaurantees
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::nearbyint, OPNAMES[ins.opcode]);
  case OP_f32_sqrt:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f32_add:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], nullptr);
  case OP_f32_sub:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], nullptr);
  case OP_f32_mul:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], nullptr);
  case OP_f32_div:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], nullptr);
  case OP_f32_min:
    return CompileFloatCmp<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f32_max:
    return CompileFloatCmp<TE_f32, TE_f32, TE_f32>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
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
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::trunc, OPNAMES[ins.opcode]);
  case OP_f64_nearest:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::nearbyint, OPNAMES[ins.opcode]);
  case OP_f64_sqrt:
    return CompileUnaryIntrinsic<TE_f64, TE_f64>(context, llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f64_add:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFAdd, OPNAMES[ins.opcode], nullptr);
  case OP_f64_sub:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFSub, OPNAMES[ins.opcode], nullptr);
  case OP_f64_mul:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFMul, OPNAMES[ins.opcode], nullptr);
  case OP_f64_div:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const Twine&, llvm::MDNode*>(context, &llvm::IRBuilder<>::CreateFDiv, OPNAMES[ins.opcode], nullptr);
  case OP_f64_min:
    return CompileFloatCmp<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f64_max:
    return CompileFloatCmp<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f64_copysign:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(context, llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);

    // Conversions
  case OP_i32_wrap_i64:
    return CompileUnaryOp<TE_i64, TE_i32, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt32Ty(), true, OPNAMES[ins.opcode]);
  case OP_i32_trunc_s_f32: // These truncation values are specifically picked to be the largest representable 32-bit floating point value that can be safely converted.
    InsertTruncTrap(context, 2147483520.0, -2147483650.0, context.builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_u_f32: // This is truncation, so values of up to -0.999999... are actually valid unsigned integers because they are truncated to 0
    InsertTruncTrap(context, 4294967040.0, -0.999999940, context.builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_s_f64: // Doubles can exactly represent 32-bit integers, so the real values are used
    InsertTruncTrap(context, 2147483647.0, -2147483648.0, context.builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_u_f64:
    InsertTruncTrap(context, 4294967295.0, -0.99999999999999989, context.builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_extend_s_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), true, OPNAMES[ins.opcode]);
  case OP_i64_extend_u_i32:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const Twine&>(context, &llvm::IRBuilder<>::CreateIntCast, context.builder.getInt64Ty(), false, OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f32:
    InsertTruncTrap(context, 9223371490000000000.0, -9223372040000000000.0, context.builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f32:
    InsertTruncTrap(context, 18446743000000000000.0, -0.999999940, context.builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToUI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_s_f64: // Largest representable 64-bit floating point values that can be safely converted
    InsertTruncTrap(context, 9223372036854774800.0, -9223372036854775800.0, context.builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const Twine&>(context, &llvm::IRBuilder<>::CreateFPToSI, context.builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_u_f64:
    InsertTruncTrap(context, 18446744073709550000.0, -0.99999999999999989, context.builder.getDoubleTy());
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

IR_ERROR CompileFunctionBody(Func* fn, FunctionType& sig, FunctionBody& body, code::Context& context)
{
  // Ensure context is reset
  assert(!context.control.Size() && !context.control.Limit());
  assert(!context.values.Size() && !context.values.Limit());

  // Get return value
  varsint7 ret = TE_void;
  if(sig.n_returns > 0)
    ret = sig.returns[0];

  PushLabel("exit", ret, OP_return, nullptr, context, fn->getSubprogram()); // Setup the function exit block that wraps everything
  context.builder.SetInsertPoint(BB::Create(context.context, "entry", fn)); // Setup initial basic block.
  context.locals.resize(0);
  context.locals.reserve(sig.n_params + body.n_locals);
  varuint32 index = 0;

  if(context.dbuilder)
    context.builder.SetCurrentDebugLocation(llvm::DILocation::get(context.context, body.debug.line, body.debug.column, fn->getSubprogram()));

  // We allocate parameters first, followed by local variables
  for(auto& arg : fn->args())
  {
    assert(index < sig.n_params);
    auto ty = GetLLVMType(sig.params[index], context);
    assert(ty == arg.getType());
    context.locals.push_back(context.builder.CreateAlloca(ty, nullptr, (body.param_names && body.param_names[index].name.size()) ? body.param_names[index].name.str() : ""));
    context.builder.CreateStore(&arg, context.locals.back()); // Store parameter (we can't use the parameter directly because wasm lets you store to parameters)

    if(context.dbuilder)
    {
      llvm::DILocalVariable *dparam = context.dbuilder->createParameterVariable(
        fn->getSubprogram(),
        context.locals.back()->getName(),
        index + 1, // the arg index starts at 1
        context.dunit,
        body.debug.line,
        CreateDebugType(context.locals.back()->getType(), context),
        true);

      context.dbuilder->insertDeclare(context.locals.back(), dparam, context.dbuilder->createExpression(),
        llvm::DebugLoc::get(body.debug.line, body.debug.column, fn->getSubprogram()),
        context.builder.GetInsertBlock());
    }

    ++index;
  }

  for(uint64_t i = 0; i < body.n_locals; ++i)
  {
    context.locals.push_back(context.builder.CreateAlloca(GetLLVMType(body.locals[i], context), nullptr, (body.local_names && body.local_names[i].name.size()) ? body.local_names[i].name.str() : ""));

    if(context.dbuilder)
    {
      llvm::DILocalVariable *dparam = context.dbuilder->createAutoVariable(
        fn->getSubprogram(),
        context.locals.back()->getName(),
        context.dunit,
        body.debug.line,
        CreateDebugType(context.locals.back()->getType(), context),
        true);

      context.dbuilder->insertDeclare(context.locals.back(), dparam, context.dbuilder->createExpression(),
        llvm::DebugLoc::get(body.debug.line, body.debug.column, fn->getSubprogram()),
        context.builder.GetInsertBlock());
    }
  }

  unsigned int stacksize = 0;
  for(auto local : context.locals)
    stacksize += (local->getType()->getElementType()->getPrimitiveSizeInBits() / 8);

  // If we allocate more than 2048 bytes of stack space, make a stack probe so we can't blow past the gaurd page.
  if(stacksize > 2048)
    fn->addFnAttr("probe-stack");

  // Begin iterating through the instructions until there aren't any left
  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    if(context.dbuilder)
      context.builder.SetCurrentDebugLocation(llvm::DILocation::get(context.context, body.body[i].line, body.body[i].column, context.control.Peek().scope));
    IR_ERROR err = CompileInstruction(body.body[i], context);
    if(err < 0)
      return err;
  }

  if(context.values.Size() > 0 && !context.values.Peek()) // Pop at most 1 polymorphic type off the stack. Any additional ones are an error.
    context.values.Pop();
  if(body.body[body.n_body - 1].opcode != OP_end)
    return assert(false), ERR_FATAL_EXPECTED_END_INSTRUCTION;
  if(context.control.Size() > 0 || context.control.Limit() > 0)
    return assert(false), ERR_END_MISMATCH;
  if(context.values.Size() > 0 || context.values.Limit() > 0)
    return assert(false), ERR_INVALID_VALUE_STACK;
  return ERR_SUCCESS;
}

void GenGlobalDebugInfo(llvm::GlobalVariable* v, code::Context& context, size_t line)
{
  if(context.dbuilder)
  {
    auto expr = context.dbuilder->createExpression(llvm::SmallVector<uint64_t, 1>{ llvm::dwarf::DW_OP_deref });

    v->addDebugInfo(context.dbuilder->createGlobalVariableExpression(
      context.dcu,
      v->getName(),
      "",
      context.dunit,
      line,
      CreateDebugType(v->getType(), context),
      !v->hasValidDeclarationLinkage(),
      expr));
  }
}

llvm::GlobalVariable* CreateGlobal(code::Context& context, llvmTy* ty, bool isconst, bool external, const Twine& name, size_t line, llvm::Constant* init = 0)
{
  auto r = new llvm::GlobalVariable(
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

  if(external)
    r->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
  GenGlobalDebugInfo(r, context, line);
  return r;
}

IR_ERROR CompileInitConstant(Instruction& instruction, Module& m, code::Context& context, llvm::Constant*& out);

IR_ERROR CompileInitGlobal(Module& m, varuint32 index, code::Context& context, llvm::Constant*& out)
{
  size_t i = index + m.importsection.memories; // Shift index to globals section
  if(i < m.importsection.globals)
  {
    auto external = ResolveExport(context.env, m.importsection.imports[i]);
    if(!external.first)
      return ERR_UNKNOWN_MODULE;
    if(!external.second)
      return ERR_UNKNOWN_EXPORT;
    if(external.second->kind != WASM_KIND_GLOBAL)
      return ERR_INVALID_GLOBAL_IMPORT_TYPE;
    return CompileInitGlobal(*external.first, external.second->index, context, out);
  }
  i -= m.importsection.globals;
  if(i < m.global.n_globals)
    return CompileInitConstant(m.global.globals[i].init, m, context, out);
  return ERR_INVALID_GLOBAL_INDEX;
}

IR_ERROR CompileInitConstant(Instruction& instruction, Module& m, code::Context& context, llvm::Constant*& out)
{
  if(instruction.opcode == OP_get_global)
    return CompileInitGlobal(m, instruction.immediates[0]._varuint32, context, out);
  return CompileConstant(instruction, context, out);
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

// Returns a table struct containing the element_type and a type index
llvmTy* GetTableType(varsint7 element_type, code::Context& context)
{
  return llvm::StructType::create({ GetLLVMType(element_type, context), GetLLVMType(TE_i32, context) });
}

IR_ERROR CompileModule(const Environment* env, code::Context& context)
{
  context.llvm = new llvm::Module(context.m.name.str(), context.context);
  context.llvm->setTargetTriple(context.machine->getTargetTriple().getTriple());
  context.llvm->setDataLayout(context.machine->createDataLayout());
  context.intptrty = context.builder.getIntPtrTy(context.llvm->getDataLayout(), 0);

  if(env->flags & ENV_DEBUG)
  {
    assert(context.m.path != nullptr);
    Path path = GetAbsolutePath(context.m.path);
    context.dbuilder = new llvm::DIBuilder(*context.llvm);

    context.dcu = context.dbuilder->createCompileUnit(
      llvm::dwarf::DW_LANG_C99,
      context.dbuilder->createFile(path.File().Get(), path.BaseDir().Get()),
      "inNative Runtime v" IR_VERSION_STRING,
      env->optimize != 0,
      llvm::StringRef(),
      WASM_MAGIC_VERSION,
      context.m.name.str());

    context.diF32 = context.dbuilder->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
    context.diF64 = context.dbuilder->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
    context.diI32 = context.dbuilder->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
    context.diI64 = context.dbuilder->createBasicType("i64", 64, llvm::dwarf::DW_ATE_signed);
    context.diI1 = context.dbuilder->createBasicType("i1", 1, llvm::dwarf::DW_ATE_boolean);
    context.diVoid = context.dbuilder->createUnspecifiedType("void");

    context.dunit = context.dbuilder->createFile(context.dcu->getFilename(), context.dcu->getDirectory());
  }

  // Define a unique init function for performing module initialization
  context.init = Func::Create(
    FuncTy::get(context.builder.getVoidTy(), false),
    Func::ExternalLinkage,
    CanonicalName(context.m.name.str(), "innative_internal_init"),
    context.llvm);

  llvm::DILocation* baselocation = 0;

  if(context.dbuilder)
  {
    FunctionDebugInfo(context.init, context, true, 0);
    baselocation = llvm::DILocation::get(context.context, context.init->getSubprogram()->getLine(), 0, context.init->getSubprogram());
    context.builder.SetCurrentDebugLocation(baselocation);
  }

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


  fn_memdump = Func::Create(FuncTy::get(context.builder.getVoidTy(), { context.builder.getInt8PtrTy(0), context.builder.getInt64Ty() }, false), Func::ExternalLinkage, "_innative_internal_env_memdump", context.llvm);
  fn_print = Func::Create(FuncTy::get(context.builder.getVoidTy(), { context.builder.getInt64Ty() }, false), Func::ExternalLinkage, "_innative_internal_env_print_compiler", context.llvm);

  context.functions.reserve(context.m.importsection.functions + context.m.function.n_funcdecl);
  context.tables.reserve(context.m.importsection.tables - context.m.importsection.functions + context.m.table.n_tables);
  context.memories.reserve(context.m.importsection.memories - context.m.importsection.tables + context.m.memory.n_memories);
  context.globals.reserve(context.m.importsection.globals - context.m.importsection.memories + context.m.global.n_globals);

  // Import function prototypes
  for(varuint32 i = 0; i < context.m.importsection.functions; ++i)
  {
    context.functions.emplace_back();
    context.functions.back().internal = GetIntrinsic(context.m.importsection.imports[i], context);
    if(context.functions.back().internal == nullptr)
    {
      auto index = context.m.importsection.imports[i].func_desc.type_index;

      if(index >= context.m.type.n_functions)
        return assert(false), ERR_INVALID_TYPE_INDEX;

      auto fname = CanonImportName(context.m.importsection.imports[i]);
      khiter_t iter = code::kh_get_importhash(context.importhash, fname.c_str());
      if(iter != kh_end(context.importhash))
        context.functions.back().internal = kh_val(context.importhash, iter);
      else
      {
        context.functions.back().internal = CompileFunction(
          context.m.type.functions[index],
          fname,
          context);
        context.functions.back().internal->setLinkage(Func::ExternalLinkage);
        int r;
        khiter_t iter = code::kh_put_importhash(context.importhash, context.functions.back().internal->getName().data(), &r);
        kh_val(context.importhash, iter) = context.functions.back().internal;
      }

      auto e = ResolveExport(*env, context.m.importsection.imports[i]);
      if(!e.second)
      {
        context.functions.back().imported = context.functions.back().internal;
        context.functions.back().imported->setLinkage(Func::ExternalLinkage);
        context.functions.back().imported->setCallingConv(GetCallingConvention(context.m.importsection.imports[i]));
        //if(context.dbuilder)
        //  FunctionDebugInfo(context.functions.back().imported, context, false, context.m.importsection.imports[i].func_desc.debug.line);

        llvm::Twine name = (context.m.importsection.imports[i].func_desc.debug.name.get()) ?
          "@" + context.functions.back().imported->getName() + "#internal" :
          context.m.importsection.imports[i].func_desc.debug.name.str() + ("#" + std::to_string(i));

        context.functions.back().internal = WrapFunction(
          context.functions.back().imported,
          name,
          context);
      }
      //else if(context.dbuilder)
      //  FunctionDebugInfo(context.functions.back().internal, context, false, context.m.importsection.imports[i].func_desc.debug.line);
    }
  }

  // Import tables
  for(varuint32 i = context.m.importsection.functions; i < context.m.importsection.tables; ++i)
    context.tables.push_back(CreateGlobal(
      context,
      GetTableType(context.m.importsection.imports[i].table_desc.element_type, context)->getPointerTo(0),
      false,
      true,
      CanonImportName(context.m.importsection.imports[i]),
      context.m.importsection.imports[i].table_desc.debug.line));

  // Import memory
  for(varuint32 i = context.m.importsection.tables; i < context.m.importsection.memories; ++i)
    context.memories.push_back(CreateGlobal(
      context,
      context.builder.getInt8PtrTy(0),
      false,
      true,
      CanonImportName(context.m.importsection.imports[i]),
      context.m.importsection.imports[i].mem_desc.debug.line));

  // Import global variables
  for(varuint32 i = context.m.importsection.memories; i < context.m.importsection.globals; ++i)
  {
    context.globals.push_back(CreateGlobal(
      context,
      GetLLVMType(context.m.importsection.imports[i].global_desc.type, context),
      !context.m.importsection.imports[i].global_desc.mutability,
      true,
      CanonImportName(context.m.importsection.imports[i]),
      context.m.importsection.imports[i].global_desc.debug.line));
  }

  // Cache internal function start index
  if(context.m.function.n_funcdecl != context.m.code.n_funcbody)
    return assert(false), ERR_INVALID_FUNCTION_BODY;
  size_t code_index = context.functions.size();

  // Declare function prototypes
  for(varuint32 i = 0; i < context.m.function.n_funcdecl; ++i)
  {
    auto index = context.m.function.funcdecl[i];
    if(index >= context.m.type.n_functions)
      return assert(false), ERR_INVALID_TYPE_INDEX;

    context.functions.emplace_back();
    context.functions.back().internal = CompileFunction(
      context.m.type.functions[index],
      std::string(!context.m.code.funcbody[i].debug.name.size() ? "func" : context.m.code.funcbody[i].debug.name.str())
      + "#" + std::to_string(context.functions.size())
      + "@" + context.m.name.str(),
      context);

    if(context.dbuilder)
      FunctionDebugInfo(context.functions.back().internal, context, true, context.m.code.funcbody[i].debug.line);
  }

  if(baselocation)
    context.builder.SetCurrentDebugLocation(baselocation);

  // Declare tables and allocate in init function
  for(varuint32 i = 0; i < context.m.table.n_tables; ++i)
  {
    auto type = GetTableType(context.m.table.tables[i].element_type, context)->getPointerTo(0);
    context.tables.push_back(CreateGlobal(
      context,
      type,
      false,
      false,
      CanonicalName(context.m.name.str(), "table#", i),
      context.m.table.tables[i].debug.line,
      llvm::ConstantPointerNull::get(type)));

    uint64_t bytewidth = context.llvm->getDataLayout().getTypeAllocSize(context.tables.back()->getType()->getElementType()->getPointerElementType());
    if(!bytewidth)
      return assert(false), ERR_INVALID_TABLE_TYPE;

    CallInst* call = context.builder.CreateCall(
      context.memgrow,
      {
        llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy(0)),
        context.builder.getInt64(context.m.table.tables[i].resizable.minimum * bytewidth),
        context.builder.getInt64((context.m.table.tables[i].resizable.flags & WASM_LIMIT_HAS_MAXIMUM) ? (context.m.table.tables[i].resizable.maximum * bytewidth) : 0)
      });

    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context);
    context.builder.CreateStore(context.builder.CreatePointerCast(call, type), context.tables.back());
  }

  // Declare linear memory spaces and allocate in init function
  for(varuint32 i = 0; i < context.m.memory.n_memories; ++i)
  {
    MemoryDesc& mem = context.m.memory.memories[i];
    auto type = context.builder.getInt8PtrTy(0);
    auto sz = context.builder.getInt64(((uint64_t)mem.limits.minimum) << 16);
    auto max = context.builder.getInt64((mem.limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? (((uint64_t)mem.limits.maximum) << 16) : 0ULL);
    context.memories.push_back(CreateGlobal(context, type, false, false, CanonicalName(context.m.name.str(), "linearmemory#", i), mem.debug.line, llvm::ConstantPointerNull::get(type)));

    CallInst* call = context.builder.CreateCall(context.memgrow, { llvm::ConstantPointerNull::get(type), sz, max });
    InsertConditionalTrap(context.builder.CreateICmpEQ(context.builder.CreatePtrToInt(call, context.intptrty), CInt::get(context.intptrty, 0)), context);
    context.builder.CreateStore(call, context.memories.back());
  }

  IR_ERROR err;

  // Declare global variables
  for(varuint32 i = 0; i < context.m.global.n_globals; ++i)
  {
    llvm::Constant* init;
    if(err = CompileInitConstant(context.m.global.globals[i].init, context.m, context, init))
      return err;

    context.globals.push_back(CreateGlobal(
      context,
      GetLLVMType(context.m.global.globals[i].desc.type, context),
      !context.m.global.globals[i].desc.mutability,
      false,
      CanonicalName(context.m.name.str(), "globalvariable#", i),
      context.m.global.globals[i].desc.debug.line,
      init));
  }

  if(baselocation)
    context.builder.SetCurrentDebugLocation(baselocation);

  // Set ENV_HOMOGENIZE_FUNCTIONS flag appropriately.
  auto wrapperfn = (env->flags & ENV_HOMOGENIZE_FUNCTIONS) ? &HomogenizeFunction : &WrapFunction;

  // Process exports by modifying global variables or function definitions as needed
  for(varuint32 i = 0; i < context.m.exportsection.n_exports; ++i)
  {
    Export& e = context.m.exportsection.exports[i];
    auto canonical = CanonicalName(context.m.name.str(), e.name.str());
    switch(e.kind)
    {
    case WASM_KIND_FUNCTION:
      context.functions[e.index].exported = (*wrapperfn)(context.functions[e.index].imported ? context.functions[e.index].imported : context.functions[e.index].internal, canonical, context, Func::ExternalLinkage, llvm::CallingConv::C);
      context.functions[e.index].exported->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_TABLE:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, context.tables[e.index])->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_MEMORY:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, context.memories[e.index])->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_GLOBAL:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, context.globals[e.index])->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    }
  }

  if(baselocation)
    context.builder.SetCurrentDebugLocation(baselocation);

  // Process data section by appending to the init function
  for(varuint32 i = 0; i < context.m.data.n_data; ++i)
  {
    DataInit& d = context.m.data.data[i]; // First we declare a constant array that stores the data in the EXE
    auto data = llvm::ConstantDataArray::get(context.context, llvm::makeArrayRef<uint8_t>(d.data.get(), d.data.get() + d.data.size()));
    auto val = new llvm::GlobalVariable(*context.llvm, data->getType(), true, llvm::GlobalValue::LinkageTypes::PrivateLinkage, data, CanonicalName(0, "data", i));
    GenGlobalDebugInfo(val, context, 0);

    llvm::Constant* offset;
    if(err = CompileInitConstant(d.offset, context.m, context, offset))
      return err;

    // Then we create a memcpy call that copies this data to the appropriate location in the init function
    context.builder.CreateCall(fn_memcpy,
      {
        context.builder.CreateInBoundsGEP(context.builder.getInt8Ty(), context.builder.CreateLoad(context.memories[d.index]), offset),
        context.builder.CreateInBoundsGEP(data->getType(), val, {context.builder.getInt32(0), context.builder.getInt32(0)}),
        context.builder.getInt64(GetTotalSize(data->getType()))
      });
  }

  // Process element section by appending to the init function
  for(uint64_t i = 0; i < context.m.element.n_elements; ++i)
  {
    TableInit& e = context.m.element.elements[i]; // First we declare a constant array that stores the data in the EXE
    TableDesc* t = ModuleTable(context.m, e.index);
    if(!t)
      return assert(false), ERR_INVALID_TABLE_INDEX;

    if(t->element_type == TE_anyfunc)
    {
      llvmTy* target = GetLLVMType(TE_anyfunc, context);
      llvm::Constant* offset;
      if(err = CompileInitConstant(e.offset, context.m, context, offset))
        return err;

      // Go through and resolve all indices to function pointers
      for(uint64_t j = 0; j < e.n_elements; ++j)
      {
        if(e.elements[j] >= context.functions.size())
          return assert(false), ERR_INVALID_FUNCTION_INDEX;

        // Store function pointer in correct table memory location
        auto ptr = context.builder.CreateGEP(context.builder.CreateLoad(context.tables[e.index]), { context.builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)), context.builder.getInt32(0) });
        context.builder.CreateAlignedStore(context.builder.CreatePointerCast(context.functions[e.elements[j]].internal, target), ptr, context.llvm->getDataLayout().getPointerSize());

        varuint32 index = GetFirstType(ModuleFunctionType(context.m, e.elements[j]), context);
        if(index == (varuint32)~0)
          return assert(false), ERR_INVALID_FUNCTION_INDEX;

        ptr = context.builder.CreateGEP(context.builder.CreateLoad(context.tables[e.index]), { context.builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)), context.builder.getInt32(1) });
        context.builder.CreateAlignedStore(context.builder.getInt32(index), ptr, 4);
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

    if(fn)
    {
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

  return ERR_SUCCESS;
}

IR_ERROR OutputObjectFile(code::Context& context, const char* out)
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

  if(context.machine->addPassesToEmitFile(pass, dest, nullptr, FileType))
  {
    llvm::errs() << "TheTargetMachine can't emit a file of this type";
    return assert(false), ERR_FATAL_FILE_ERROR;
  }

  pass.run(*context.llvm);
  dest.flush();
  return ERR_SUCCESS;
}

namespace innative {
  IR_ERROR CompileEnvironment(const Environment* env, const char* filepath)
  {
    llvm::LLVMContext llvm_context;
    Path file(filepath);
    Path workdir = GetWorkingDir();
    Path programpath(env->sdkpath);
    uint64_t eflags = env->flags;

    SetWorkingDir(programpath.BaseDir().Get().c_str());
    utility::DeferLambda<std::function<void()>> defer([&]() { SetWorkingDir(workdir.Get().c_str()); });

    if(!file.IsAbsolute())
      file = workdir + file;

    llvm::IRBuilder<> builder(llvm_context);
    code::Context* start = nullptr;
    IR_ERROR err = ERR_SUCCESS;

    code::Context* context = tmalloc<code::Context>(*env, env->n_modules);
    string triple = llvm::sys::getProcessTriple();

    // Set up our target architecture, necessary up here so our code generation knows how big a pointer is
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    string llvm_err;
    auto arch = llvm::TargetRegistry::lookupTarget(triple, llvm_err);

    if(!arch)
    {
      llvm::errs() << llvm_err;
      return assert(false), ERR_FATAL_UNKNOWN_TARGET;
    }

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
#ifdef IR_PLATFORM_POSIX
    if(eflags&ENV_LIBRARY)
      RM = llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
#endif
    llvm::SubtargetFeatures subtarget_features;
    llvm::StringMap<bool> feature_map;
    if(llvm::sys::getHostCPUFeatures(feature_map))
    {
      for(auto &feature : feature_map)
      {
        subtarget_features.AddFeature(feature.first(), feature.second);
      }
    }
    auto machine = arch->createTargetMachine(triple, llvm::sys::getHostCPUName(), subtarget_features.getString(), opt, RM, llvm::None);

    for(varuint32 i = 0; i < env->n_modules; ++i)
    {
      new(context + i) code::Context{ *env, env->modules[i], llvm_context, 0, builder, machine, code::kh_init_importhash() };
      if((err = CompileModule(env, context[i])) < 0)
        return err;

      // If module has a start function, create a main entry point function
      if(context[i].start != nullptr)
      {
        if(start != nullptr)
          return ERR_MULTIPLE_ENTRY_POINTS;
        start = context + i;
      }
    }

    if(!env->n_modules)
      return ERR_FATAL_INVALID_MODULE;

    // Initialize all modules and call start function (if it exists)
    if(!start) // We always create an initialization function, even for DLLs, to do proper initialization of modules
      start = &context[0];
    if(start->start == nullptr && !(eflags&ENV_LIBRARY))
      return ERR_FATAL_INVALID_MODULE; // We can't compile an EXE without an entry point

    FuncTy* mainTy = FuncTy::get(builder.getVoidTy(), false);
#ifdef IR_PLATFORM_WIN32
    if(eflags&ENV_LIBRARY)
      mainTy = FuncTy::get(builder.getInt32Ty(), { builder.getInt8PtrTy(), start->intptrty, builder.getInt8PtrTy() }, false);
#endif

    Func* exit = Func::Create(
      FuncTy::get(builder.getVoidTy(), { builder.getInt32Ty() }, false),
      Func::ExternalLinkage,
      "_innative_internal_env_exit",
      start->llvm);

    Func* main = Func::Create(mainTy, Func::ExternalLinkage, IR_INIT_FUNCTION);

#ifdef IR_PLATFORM_WIN32
    if(eflags&ENV_LIBRARY)
      main->setCallingConv(llvm::CallingConv::X86_StdCall);
#endif
    if(start->dbuilder)
    {
      FunctionDebugInfo(main, *start, true, 0);
      builder.SetCurrentDebugLocation(llvm::DILocation::get(start->context, main->getSubprogram()->getLine(), 0, main->getSubprogram()));
    }

    BB* initblock = BB::Create(llvm_context, "start_entry", main);
    builder.SetInsertPoint(initblock);
    for(size_t i = 0; i < env->n_modules; ++i)
    {
      if(context + i == start)
        builder.CreateCall(context[i].init, {});
      else
      {
        Func* stub = Func::Create(context[i].init->getFunctionType(),
          context[i].init->getLinkage(),
          context[i].init->getName(),
          start->llvm); // Create function prototype in main module
        builder.CreateCall(stub, {});
      }
    }

    if(start->start != nullptr)
      builder.CreateCall(start->start, {});

    if(eflags&ENV_LIBRARY)
    {
#ifdef IR_PLATFORM_WIN32
      builder.CreateRet(builder.getInt32(1)); // On windows, the DLL init function must always return 1
#else
      builder.CreateRetVoid();
#endif
    }
    else // If this isn't a DLL, then the init function is actual the process entry point, which must call _exit()
    {
      builder.CreateCall(exit, builder.getInt32(0));
      main->setDoesNotReturn();
      builder.CreateUnreachable(); // This function never returns
    }

    start->llvm->getFunctionList().push_back(main);

    // Annotate functions
    AnnotateFunctions(env, context);

    // Optimize all modules
    for(size_t i = 0; i < env->n_modules; ++i)
    {
    }

    // Finalize all modules
    for(varuint32 i = 0; i < env->n_modules; ++i)
    {
      if(context[i].dbuilder)
        context[i].dbuilder->finalize();

      if(env->flags&ENV_EMIT_LLVM)
      {
        std::error_code EC;
        llvm::raw_fd_ostream dest(string(context[i].llvm->getName()) + ".llvm", EC, llvm::sys::fs::F_None);
        context[i].llvm->print(dest, nullptr);
      }

      // Verify module
      std::error_code EC;
      llvm::raw_fd_ostream dest(1, false, true);
      if(llvm::verifyModule(*context[i].llvm, &dest))
        return ERR_FATAL_INVALID_MODULE;
    }

    {
      vector<string> cache;
#ifdef IR_PLATFORM_WIN32
      vector<const char*> linkargs = { "/ERRORREPORT:QUEUE", "/INCREMENTAL:NO", "/NOLOGO",
        "/nodefaultlib", /*"/MANIFEST", "/MANIFEST:embed",*/ "/SUBSYSTEM:CONSOLE", "/VERBOSE",
        "/LARGEADDRESSAWARE", "/OPT:REF", "/OPT:ICF", "/STACK:10000000", "/DYNAMICBASE", "/NXCOMPAT",
        "/MACHINE:X64", "/machine:x64", "/ENTRY:" IR_INIT_FUNCTION };

      if(eflags&ENV_LIBRARY)
        linkargs.push_back("/DLL");
      if(eflags&ENV_DEBUG)
        linkargs.push_back("/DEBUG");

      vector<string> targets = { string("/OUT:") + file.Get(), "/LIBPATH:" + programpath.BaseDir().Get(), "/LIBPATH:" + workdir.Get() };
#elif defined(IR_PLATFORM_POSIX)
      vector<const char*> linkargs = {  };

      if(eflags&ENV_LIBRARY)
        linkargs.push_back("-shared");
      if(!(eflags&ENV_DEBUG))
        linkargs.push_back("--strip-debug");

      vector<string> targets = { string("--output=") + file.Get(), "-L" + programpath.BaseDir().Get(), "-L" + workdir.Get() };
#else 
#error unknown platform
#endif

      // Write all in-memory environments to cache files
      for(Embedding* cur = env->embeddings; cur != nullptr; cur = cur->next)
      {
        if(cur->size > 0) // If the size is greater than 0, this is an in-memory embedding
        {
          union { Embedding* p; size_t z; } u = { cur };
          cache.push_back(IR_STATIC_FLAG + std::to_string(u.z) + IR_ENV_EXTENSION + IR_STATIC_EXTENSION);
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

      size_t target_init = targets.size();

      // Generate object code
      for(size_t i = 0; i < env->n_modules; ++i)
      {
        assert(context[i].m.name.get() != nullptr);
        targets.emplace_back(std::string(context[i].m.name.str(), context[i].m.name.size()) + ".o");
        OutputObjectFile(context[i], targets.back().c_str());
      }

      for(auto& v : targets)
        linkargs.push_back(v.c_str());

#ifdef IR_PLATFORM_POSIX
      if(eflags&ENV_LIBRARY) // https://stackoverflow.com/questions/9759880/automatically-executed-functions-when-loading-shared-libraries
        linkargs.push_back("-init=" IR_INIT_FUNCTION);
      else // If this isn't a shared library, we must specify an entry point instead of an init function
        linkargs.push_back("--entry=" IR_INIT_FUNCTION);
#endif

      // Link object code
      if(env->linker != 0)
      {
#ifdef IR_PLATFORM_WIN32
        const char* quote = "\"";
#elif defined(IR_PLATFORM_POSIX)
        const char* quote = "";
#endif
        std::string cmd;
        cmd += quote; // for windows we have to double quote the entire bloody command because system() actually calls "cmd /c <string>"
        cmd += quote;
        cmd += env->linker;
        cmd += quote;

        size_t sz = cmd.size();
        for(auto arg : linkargs)
          sz += strlen(arg);
        cmd.reserve(sz + 1 + linkargs.size() * 3);
        for(auto arg : linkargs)
        {
          cmd += ' ';
          cmd += quote;
          cmd += arg;
          cmd += quote;
        }

        cmd += quote;

        std::cout << "Executing external linker command: " << cmd.c_str() << std::endl;
        int err = system(cmd.c_str());
        if(err != 0)
          return assert(false), ERR_FATAL_LINK_ERROR;
      }
      else
      {
        llvm::raw_fd_ostream dest(1, false, true);
#ifdef IR_PLATFORM_WIN32
        if(!lld::coff::link(linkargs, false, dest))
#else
        if(!lld::elf::link(linkargs, false, dest))
#endif
        {
          for(auto& v : cache)
            std::remove(v.c_str());
          return assert(false), ERR_FATAL_LINK_ERROR;
        }
      }

      // Delete cache files
      for(auto& v : cache)
        std::remove(v.c_str());
      for(; target_init < targets.size(); ++target_init)
        std::remove(targets[target_init].c_str());
    }

    return ERR_SUCCESS;
  }
}