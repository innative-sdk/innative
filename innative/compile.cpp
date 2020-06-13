// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "utility.h"
#include "optimize.h"
#include "compile.h"
#include "debug.h"
#include "link.h"
#include "innative/export.h"
#include "jit.h"

#define DIVIDER ":"

using Func    = llvm::Function;
using FuncTy  = llvm::FunctionType;
using llvmTy  = llvm::Type;
using llvmVal = llvm::Value;
using BB      = llvm::BasicBlock;
using llvm::CallInst;
using llvm::ConstantFP;

namespace innative {
  __KHASH_IMPL(importhash, , const char*, llvm::GlobalObject*, 1, kh_str_hash_func, kh_str_hash_equal);
}

using namespace innative;
using namespace utility;

llvmTy* Compiler::GetLLVMType(varsint7 type)
{
  switch(type)
  {
  case TE_i32: return llvmTy::getInt32Ty(ctx);
  case TE_i64: return llvmTy::getInt64Ty(ctx);
  case TE_f32: return llvmTy::getFloatTy(ctx);
  case TE_f64: return llvmTy::getDoubleTy(ctx);
  case TE_void: return llvmTy::getVoidTy(ctx);
  case TE_funcref:
    return FuncTy::get(llvmTy::getVoidTy(ctx), false)->getPointerTo(0); // placeholder (*void)() function pointer
  case TE_cref: return llvmTy::getInt8PtrTy(ctx);
  }

  assert(false);
  return nullptr;
}

llvmTy* Compiler::GetLLVMTypes(varsint7* types, varuint32 count)
{
  if(count == 0)
    return llvmTy::getVoidTy(ctx);
  if(count == 1)
    return GetLLVMType(types[0]);

  llvm::SmallVector<llvmTy*, 4> llvmtypes;
  for(varuint32 i = 0; i < count; ++i)
    llvmtypes.push_back(GetLLVMType(types[count - i - 1]));
  return llvm::StructType::get(ctx, llvmtypes);
}

llvmTy* Compiler::GetLLVMTypeSig(varsint64 sig)
{
  if(sig < 0)
    return GetLLVMType(static_cast<varsint7>(sig));
  if(sig >= m.type.n_functypes)
    return nullptr;
  return GetLLVMTypes(m.type.functypes[sig].returns, m.type.functypes[sig].n_returns);
}

WASM_TYPE_ENCODING Compiler::GetTypeEncoding(llvmTy* t)
{
  assert(!t->isStructTy());

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
  if(t->isPointerTy() && t->getPointerElementType()->isIntegerTy())
    return TE_cref;
  if(t->isPointerTy() && t->getPointerElementType()->isStructTy())
    assert(false);

  return TE_NONE;
}

FuncTy* Compiler::GetFunctionType(FunctionType& signature)
{
  llvmTy* ret = GetLLVMTypes(signature.returns, signature.n_returns);

  if(signature.n_params > 0)
  {
    std::vector<llvmTy*> args;
    for(varuint32 i = 0; i < signature.n_params; ++i)
      args.push_back(GetLLVMType(signature.params[i]));

    return FuncTy::get(ret, args, false);
  }
  return FuncTy::get(ret, false);
}

Func* Compiler::PassFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                             llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{
  // fn->setCallingConv(callconv);
  fn->setLinkage(linkage);
  fn->setName(canonical);
  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(fn, name, env.optimize != 0, true, true, fn->getSubprogram()->getFile(),
                                fn->getSubprogram()->getLine(), 0);

  debugger->SetSPLocation(builder, fn->getSubprogram());
  return fn;
}

Func* Compiler::WrapFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                             llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{
  Func* wrap = Func::Create(fn->getFunctionType(), linkage, canonical, mod);
  wrap->setCallingConv(callconv);
  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(wrap, name, env.optimize != 0, true, true, fn->getSubprogram()->getFile(),
                                fn->getSubprogram()->getLine(), 0);

  debugger->SetSPLocation(builder, wrap->getSubprogram());
  auto prev = builder.GetInsertBlock();

  BB* bb = BB::Create(ctx, "wrapper_block", wrap);
  builder.SetInsertPoint(bb);

  std::vector<llvmVal*> values;
  for(auto& arg : wrap->args())
    values.push_back(&arg);
  auto val = builder.CreateCall(fn, values);
  val->setCallingConv(fn->getCallingConv());
  val->setAttributes(fn->getAttributes());

  if(!wrap->getReturnType()->isVoidTy())
    builder.CreateRet(val);
  else
    builder.CreateRetVoid();

  builder.SetInsertPoint(prev);
  return wrap;
}

Func* Compiler::GenericFunction(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                                llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{ // generalize function into void(char* params, char* results)
  auto functy   = FuncTy::get(llvmTy::getVoidTy(ctx), { llvmTy::getInt8PtrTy(ctx), llvmTy::getInt8PtrTy(ctx) }, false);
  Func* generic = Func::Create(functy, linkage, canonical, mod);
  generic->setCallingConv(callconv);
  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(generic, name, env.optimize != 0, true, true, fn->getSubprogram()->getFile(),
                                fn->getSubprogram()->getLine(), 0);

  debugger->SetSPLocation(builder, generic->getSubprogram());
  auto prev = builder.GetInsertBlock();

  BB* bb = BB::Create(ctx, "generic_block", generic);
  builder.SetInsertPoint(bb);

  std::vector<llvmVal*> values;
  auto params = generic->getArg(0);
  auto results = generic->getArg(1);

  int offset = 0;
  for(auto& arg : fn->args())
  {
    values.push_back(builder.CreateLoad(builder.CreateBitCast(builder.CreateInBoundsGEP(params, { builder.getInt32(offset) }), arg.getType())));
    offset += mod->getDataLayout().getTypeSizeInBits(arg.getType()) / 8;
    // TODO: structs must be broken apart and repacked
  }

  auto val = builder.CreateCall(fn, values);
  val->setCallingConv(fn->getCallingConv());
  val->setAttributes(fn->getAttributes());

  offset   = 0;
  auto ret = fn->getReturnType();
  if(ret->isStructTy()) {
    for(unsigned int i = 0; i < ret->getStructNumElements(); ++i)
    {
      builder.CreateStore(builder.CreateInBoundsGEP(val, { builder.getInt32(0), builder.getInt32(i) }),
                          builder.CreateBitCast(builder.CreateInBoundsGEP(results, { builder.getInt32(offset) }),
                                                ret->getStructElementType(i)->getPointerTo()),
                          false);
      offset += mod->getDataLayout().getTypeSizeInBits(ret->getStructElementType(i));
    }
  }
  else if(!ret->isVoidTy())
    builder.CreateStore(val, results, false);

  builder.CreateRetVoid();
  builder.SetInsertPoint(prev);
  return generic;
}

bool Compiler::CheckType(varsint7 ty, llvmTy* t)
{
  switch(ty)
  {
  case TE_i32: return t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 32;
  case TE_i64: return t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 64;
  case TE_f32: return t->isFloatTy();
  case TE_f64: return t->isDoubleTy();
  case TE_void: return t->isVoidTy();
  case TE_cref: return t->isPointerTy() && t->getPointerElementType()->isIntegerTy();
  }

  return true;
}

bool Compiler::CheckSig(varsint64 sig, llvmTy* t, Module& m)
{
  if(sig < 0)
    return CheckType(static_cast<varsint7>(sig), t);
  if(sig >= m.type.n_functypes)
    return false;
  auto n_sig = m.type.functypes[sig].n_returns;
  if(n_sig == 0)
    return CheckType(TE_void, t);
  if(n_sig == 1)
    return CheckType(m.type.functypes[sig].returns[0], t);

  if(!t->isPointerTy() || !t->getPointerElementType()->isStructTy())
    return false;
  for(varuint32 i = 0; i < n_sig; ++i)
    if(!CheckType(m.type.functypes[sig].returns[i], t->getPointerElementType()->getStructElementType(i)))
      return false;
  return true;
}

IN_ERROR Compiler::PopType(varsint7 ty, llvmVal*& v, bool peek)
{
  if(ty == TE_void)
    v = 0;
  else if(!values.Size())
    return ERR_INVALID_VALUE_STACK;
  else if(!values.Peek()) // polymorphic value
  {
    switch(ty)
    {
    case TE_i32:
    case TE_i64:
    case TE_f32:
    case TE_f64: v = llvm::Constant::getNullValue(GetLLVMType(ty)); break;
    default: return ERR_INVALID_TYPE;
    }
  }
  else if(ty == TE_cref && values.Peek()->getType()->isIntegerTy())
  { // If this is true, we need to do an int -> cref conversion
    v = builder.CreatePtrToInt(builder.CreateLoad(GetPairPtr(memories[0], 0)), builder.getInt64Ty());
    v = builder.CreateAdd(builder.CreateZExt(peek ? values.Peek() : values.Pop(), builder.getInt64Ty()), v, "", true, true);
    v = builder.CreateIntToPtr(v, GetLLVMType(TE_cref));
    return ERR_SUCCESS;
  }
  else if(!CheckType(ty, values.Peek()->getType()))
    return ERR_INVALID_TYPE;
  else if(peek)
    v = values.Peek();
  else
    v = values.Pop();

  return ERR_SUCCESS;
}

IN_ERROR Compiler::PopStruct(varsint64 sig, llvmVal*& v, bool peek)
{
  if(sig < 0)
    return PopType(static_cast<varsint7>(sig), v, peek);
  if(sig >= m.type.n_functypes)
    return ERR_INVALID_FUNCTION_SIG;
  auto n_sig = m.type.functypes[sig].n_returns;
  if(n_sig == 0)
    return PopType(TE_void, v);
  if(n_sig == 1)
    return PopType(m.type.functypes[sig].returns[0], v);

  if(!values.Size())
    return ERR_INVALID_VALUE_STACK;

  llvm::SmallVector<llvm::Value*, 4> elems;
  llvm::SmallVector<llvm::Type*, 4> types;

  for(varuint32 i = 0; i < n_sig; ++i)
  {
    if((i < values.Size() && !values[i]) || (i >= values.Size() && !values[values.Size() - 1])) // polymorphic value
      elems.push_back(llvm::Constant::getNullValue(GetLLVMType(m.type.functypes[sig].returns[n_sig - 1 - 1])));
    else if(!CheckType(m.type.functypes[sig].returns[n_sig - i - 1], values[i]->getType()))
      return ERR_INVALID_TYPE;
    else
      elems.push_back(values[i]);
    types.push_back(elems.back()->getType());
  }

  // Now we have to actually pop values off the stack
  if(!peek)
    for(varuint32 i = 0; i < n_sig; ++i)
    {
      if(!values.Peek())
        break;
      values.Pop();
    }

  auto s = llvm::StructType::get(ctx, types);
  v      = builder.CreateAlloca(s);

  for(varuint32 i = 0; i < n_sig; ++i)
    builder.CreateStore(elems[i], builder.CreateInBoundsGEP(v, { builder.getInt32(0), builder.getInt32(i) }), false);

  return ERR_SUCCESS;
}

// Returns a table struct containing the element_type and a type index
llvm::StructType* Compiler::GetTableType(varsint7 element_type)
{
  return llvm::StructType::create({ GetLLVMType(element_type), GetLLVMType(TE_i32) });
}

llvm::StructType* Compiler::GetPairType(llvmTy* ty) { return llvm::StructType::create({ ty, GetLLVMType(TE_i64) }); }

llvmVal* Compiler::GetPairPtr(llvm::GlobalVariable* v, int index)
{
  return builder.CreateInBoundsGEP(v, { builder.getInt32(0), builder.getInt32(index) });
}
llvm::Constant* Compiler::GetPairNull(llvm::StructType* ty)
{
  return llvm::ConstantStruct::get(ty, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ty->getElementType(0))),
                                   builder.getInt64(0));
}

llvmVal* Compiler::MaskShiftBits(llvmVal* value)
{
  // WASM requires that a shift count greater than the bit width of the type is wrapped, which matches x86 behavior but is
  // undefined in LLVM, so we make this explicit.
  return builder.CreateAnd(value, CInt::get(value->getType(), value->getType()->getIntegerBitWidth() - 1));
}

BB* Compiler::PushLabel(const char* name, varsint64 sig, uint8_t opcode, Func* fnptr, llvm::DILocalScope* scope,
                        bool discard)
{
  BB* bb = BB::Create(ctx, name, fnptr);
  debugger->PushBlock(scope, builder.getCurrentDebugLocation());

  control.Push(Block{ bb, 0, values.Limit(), sig, opcode });

  if(discard) // If discard is true, this is the function declaration, which ignores the function parameters
    return bb;

  assert(GetBlockSigParams(sig, m) <= values.Size());
  // Set limit to current stack size (minus block parameters) to prevent a block from popping past it's parameters
  values.SetLimit(values.Size() + values.Limit() - GetBlockSigParams(sig, m));

  if(values.Size() > 0 &&
     !values.Peek()) // If we're in an unreachable segment, just push another placeholder on to the stack
    values.Push(nullptr);

  return bb;
}

BB* Compiler::BindLabel(BB* block)
{
  if(block->getParent() != nullptr) // Because this always happens after a branch, even if we have nothing to bind to, we
                                    // must create a new block for LLVM
    block = BB::Create(ctx, "bind_block", nullptr);

  builder.GetInsertBlock()->getParent()->getBasicBlockList().push_back(block);
  builder.SetInsertPoint(block);
  assert(block->getParent() != nullptr);
  return block;
}

IN_ERROR Compiler::PushResult(BlockResult** root, llvmVal* result, BB* block, const Environment& env)
{
  BlockResult* next = *root;
  *root             = tmalloc<BlockResult>(env, 1);
  if(!*root)
    return ERR_FATAL_OUT_OF_MEMORY;

  new(*root) BlockResult{ result, block, next };
  return ERR_SUCCESS;
}

IN_ERROR Compiler::PushMultiReturn(llvmVal* arg, varsint64 sig)
{
  varuint32 n_sig = GetBlockSigResults(sig, m);
  if(!n_sig)
    return ERR_INVALID_VALUE_STACK;
  if(n_sig == 1)
    return PushReturn(arg);

  auto ty = arg->getType();
  if(!ty->isPointerTy() || !ty->getPointerElementType()->isStructTy() || sig >= m.type.n_functypes)
    return ERR_INVALID_FUNCTION_SIG;
  if(ty->getPointerElementType()->getStructNumElements() != n_sig)
    return ERR_INVALID_BLOCK_SIGNATURE;

  // Push struct members in reverse order
  for(varuint32 i = n_sig; i-- > 0;)
  {
    llvmVal* val = builder.CreateLoad(builder.CreateInBoundsGEP(arg, { builder.getInt32(0), builder.getInt32(i) }));
    if(m.type.functypes[sig].returns[i] == TE_cref && val->getType()->isIntegerTy())
    { // If this is true, we need to do an int -> cref conversion
      auto v = builder.CreatePtrToInt(builder.CreateLoad(GetPairPtr(memories[0], 0)), builder.getInt64Ty());
      v      = builder.CreateAdd(builder.CreateZExt(val, builder.getInt64Ty()), v, "", true, true);
      val    = builder.CreateIntToPtr(v, GetLLVMType(TE_cref));
    }
    if(IN_ERROR err = PushReturn(val))
      return err;
  }

  return ERR_SUCCESS;
}

// Adds current value stack to target branch according to that branch's signature.
IN_ERROR Compiler::AddBranch(Block& target)
{
  IN_ERROR err    = ERR_SUCCESS;
  varuint32 n_sig = GetBlockSigResults(target.sig, m);
  if(n_sig > 0)
  {
    llvmVal* value;
    err = PopStruct(target.sig, value, true);
    if(!err)
      err = PushResult(&target.results, value, builder.GetInsertBlock(), env); // Push result
  }
  return err;
}

// Pops a label off the control stack, verifying that the value stack matches the signature and building PHI nodes as
// necessary
IN_ERROR Compiler::PopLabel(BB* block, llvmVal* push)
{
  varuint32 n_sig = GetBlockSigResults(control.Peek().sig, m);
  if(n_sig > 0)
  {
    // If there are results from other branches, perform a PHI merge. Otherwise, leave the value stack alone
    if(control.Peek().results != nullptr)
    {
      unsigned int count = 1; // Start with 1 for our current branch's values
      for(auto i = control.Peek().results; i != nullptr; i = i->next)
        ++count; // Count number of additional results

      llvm::PHINode* phi = builder.CreatePHI(push->getType(), count,
                                             "phi"); // TODO: Account for multiple return values once they are added
      phi->addIncoming(push, block); // Pop this branches values off value stack, add using proper insert block

      for(auto i = control.Peek().results; i != nullptr; i = i->next)
        phi->addIncoming(i->v, i->b);

      push = phi; // Push phi nodes on to stack
    }
  }
  else if(control.Peek().results != nullptr)
    return ERR_INVALID_VALUE_STACK;

  if(values.Size() > 0 && !values.Peek()) // Pop at most 1 polymorphic type off the stack.
    values.Pop();
  if(values.Size() > 0) // value stack should be completely empty now
    return ERR_INVALID_VALUE_STACK;

  if(push)
    PushMultiReturn(push, control.Peek().sig);

  values.SetLimit(control.Peek().limit);
  control.Pop();
  debugger->PopBlock();

  return ERR_SUCCESS;
}

void Compiler::PolymorphicStack()
{
  while(values.Size() > 0)
    values.Pop();
  values.Push(nullptr);
  BB* graveyard = BB::Create(ctx, "graveyard", builder.GetInsertBlock()->getParent());
  builder.SetInsertPoint(graveyard);
}

IN_ERROR Compiler::InsertConditionalTrap(llvmVal* cond)
{
  // Define a failure block that all errors jump to via a conditional branch which simply traps
  auto trapblock = BB::Create(ctx, "trap_block", builder.GetInsertBlock()->getParent());
  auto contblock = BB::Create(ctx, "trap_continue", builder.GetInsertBlock()->getParent());

  builder.CreateCondBr(cond, trapblock, contblock);
  builder.SetInsertPoint(trapblock);
  CompileTrap();

  builder.SetInsertPoint(contblock);
  return ERR_SUCCESS;
}

llvmVal* Compiler::GetMemPointer(llvmVal* base, llvm::PointerType* pointer_type, varuint32 memory, varuint32 offset)
{
  assert(memories.size() > 0);
  llvmVal* src          = !memory ? memlocal : static_cast<llvmVal*>(GetPairPtr(memories[memory], 0));
  llvm::IntegerType* ty = machine->getPointerSizeInBits(memory) == 32 ? builder.getInt32Ty() : builder.getInt64Ty();

  // If our native integer size is larger than the webassembly memory pointer size, then overflow is not possible and we
  // can bypass the check.
  bool bypass = ty->getBitWidth() > base->getType()->getIntegerBitWidth();
  base        = builder.CreateZExtOrTrunc(base, ty);

  llvmVal* loc;
  if(env.flags & ENV_CHECK_MEMORY_ACCESS) // In strict mode, generate a check that traps if this is an invalid memory access
  {
    llvmVal* end = builder.CreateIntCast(GetMemSize(memories[memory]), ty, false);
    llvmVal* cond;
    Func* uadd_with_overflow = llvm::Intrinsic::getDeclaration(mod, llvm::Intrinsic::uadd_with_overflow, { ty });

    if(bypass) // If we can bypass the overflow check because we have enough bits, only check the upper bound
    {
      loc = builder.CreateAdd(base, CInt::get(ty, offset, false), "", true, true);
      auto upper =
        builder.CreateAdd(loc, CInt::get(ty, pointer_type->getPointerElementType()->getPrimitiveSizeInBits() / 8, false));
      cond = builder.CreateICmpUGT(upper, end, "invalid_mem_access_cond");
    }
    else
    {
      llvmVal* v        = builder.CreateCall(uadd_with_overflow, { base, CInt::get(ty, offset, false) });
      llvmVal* overflow = builder.CreateExtractValue(v, 1);
      loc               = builder.CreateExtractValue(v, 0);

      v          = builder.CreateCall(uadd_with_overflow,
                             { loc,
                               CInt::get(ty, pointer_type->getPointerElementType()->getPrimitiveSizeInBits() / 8, false) });
      overflow   = builder.CreateOr(overflow, builder.CreateExtractValue(v, 1), "invalid_mem_access_cond_overflow");
      auto upper = builder.CreateExtractValue(v, 0);
      cond       = builder.CreateOr(overflow, builder.CreateICmpUGT(upper, end, "invalid_mem_access_cond_upper"),
                              "invalid_mem_access_cond");
    }

    InsertConditionalTrap(cond);
  }
  else
    loc = builder.CreateAdd(base, CInt::get(ty, offset, false), "", true, true);

  return builder.CreatePointerCast(builder.CreateInBoundsGEP(builder.CreateLoad(src), loc), pointer_type);
}

IN_ERROR Compiler::InsertTruncTrap(double max, double min, llvm::Type* ty)
{
  if((env.flags & ENV_CHECK_FLOAT_TRUNC) && values.Size() > 0 && values.Peek() != nullptr)
  {
    return InsertConditionalTrap(builder.CreateOr(
      (ty->isFloatTy() ? builder.CreateICmpEQ(builder.CreateAnd(builder.CreateBitCast(values.Peek(), builder.getInt32Ty()),
                                                                builder.getInt32(0x7F800000)),
                                              builder.getInt32(0x7F800000)) :
                         builder.CreateICmpEQ(builder.CreateAnd(builder.CreateBitCast(values.Peek(), builder.getInt64Ty()),
                                                                builder.getInt64(0x7FF0000000000000)),
                                              builder.getInt64(0x7FF0000000000000))),
      builder.CreateOr(builder.CreateFCmpOGT(values.Peek(), ConstantFP::get(ty, max)),
                       builder.CreateFCmpOLT(values.Peek(), ConstantFP::get(ty, min)))));
  }
  return ERR_SUCCESS;
};

void Compiler::DumpCompilerState()
{
  FILE* out = env.log;
  fputs("values: [", out);

  size_t total = values.Size() + values.Limit();
  for(size_t i = 0; i < total; ++i)
  {
    if(i == values.Limit())
      fputs(" |", out);
    if(!values[i])
      fputs(" Poly", out);
    else
      switch(GetTypeEncoding(values[i]->getType()))
      {
      case TE_i32: fputs(" i32", out); break;
      case TE_i64: fputs(" i64", out); break;
      case TE_f32: fputs(" f32", out); break;
      case TE_f64: fputs(" f64", out); break;
      }
  }

  fputs(" ]\n", out);
  fputs("control: [", out);

  for(size_t i = 0; i < control.Size(); ++i)
  {
    switch(control[i].sig)
    {
    case TE_i32: fputs(" i32", out); break;
    case TE_i64: fputs(" i64", out); break;
    case TE_f32: fputs(" f32", out); break;
    case TE_f64: fputs(" f64", out); break;
    }

    FPRINTF(out, ":%i", (int)control[i].op);
  }

  fputs(" ]\n\n", out);
  fflush(out);
}

uint64_t Compiler::GetLocalOffset(llvm::AllocaInst* p)
{
  return llvm::cast<llvm::ConstantInt>(
           llvm::cast<llvm::ConstantAsMetadata>(p->getMetadata(IN_LOCAL_INDEX_METADATA)->getOperand(0))->getValue())
    ->getZExtValue();
}

llvm::Value* Compiler::GetLocal(varuint32 index)
{
  auto i = std::upper_bound(locals.begin(), locals.end(), index,
                            [](varuint32 v, llvm::AllocaInst* r) { return v < GetLocalOffset(r); });
  if(i == locals.begin())
    return nullptr;
  auto offset = GetLocalOffset(*--i);
  assert(index >= offset);
  index -= static_cast<decltype(index)>(offset);
  if(!index)
    return *i;
  auto size = llvm::cast<llvm::ConstantInt>((*i)->getArraySize())->getZExtValue();
  if(index >= size)
    return nullptr;
  return builder.CreateInBoundsGEP(*i, { builder.getInt32(index) });
}

llvm::GlobalVariable* Compiler::CreateGlobal(llvmTy* ty, bool isconst, bool external, llvm::StringRef name,
                                             const llvm::Twine& canonical, size_t line, llvm::Constant* init = 0)
{
  auto r = new llvm::GlobalVariable(*mod, ty, isconst,
                                    external ? llvm::GlobalValue::LinkageTypes::ExternalLinkage :
                                               llvm::GlobalValue::LinkageTypes::InternalLinkage,
                                    init, canonical, nullptr, llvm::GlobalValue::NotThreadLocal, 0, !init);

  if(external)
    r->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
  debugger->DebugGlobal(r, name, line);
  return r;
}

uint64_t Compiler::GetTotalSize(llvmTy* t)
{
  return t->getArrayNumElements() * (t->getArrayElementType()->getPrimitiveSizeInBits() / 8);
}

const Compiler::Intrinsic* Compiler::GetIntrinsic(Import& imp)
{
  if(IsSystemImport(imp.module_name, env.system))
  {
    for(auto& v : intrinsics)
    {
      if(!strcmp(v.name, imp.export_name.str()))
        return &v;
    }
  }
  return nullptr;
}

Func* Compiler::TopLevelFunction(llvm::LLVMContext& context, llvm::IRBuilder<>& builder, const char* name, llvm::Module* m)
{
  auto test = builder.getVoidTy();
  Func* fn  = Func::Create(FuncTy::get(builder.getVoidTy(), false), Func::ExternalLinkage, name, m);

  BB* initblock = BB::Create(context, "entry", fn);
  builder.SetInsertPoint(initblock);
  return fn;
}

void Compiler::ExportFunction(FunctionSet& fn,
                              Func* (Compiler::*wrapper)(Func* fn, llvm::StringRef name, const llvm::Twine& canonical,
                                                         llvm::GlobalValue::LinkageTypes linkage,
                                                         llvm::CallingConv::ID callconv),
                              llvm::StringRef name, const llvm::Twine& canonical)
{
  if(!fn.exported)
  {
    ABI abi   = CURRENT_ABI;
    auto base = fn.imported ? fn.imported : fn.internal;

    debugger->SetSPLocation(builder, base->getSubprogram());
    fn.exported = (this->*wrapper)(base, name, canonical, Func::ExternalLinkage, llvm::CallingConv::C);
  }
}

int innative::GetCallingConvention(const Import& imp)
{
  if(imp.kind != WASM_KIND_FUNCTION)
    return llvm::CallingConv::MaxID;

  const char* str = !imp.module_name.str() ? nullptr : strrchr(imp.module_name.str(), '!');
  if(!str)
    return llvm::CallingConv::C;
  ++str;
  if(!STRICMP(str, "C"))
    return llvm::CallingConv::C;
  if(!STRICMP(str, "STD"))
    return llvm::CallingConv::X86_StdCall;
  if(!STRICMP(str, "FAST"))
    return llvm::CallingConv::X86_FastCall;
  if(!STRICMP(str, "THIS"))
    return llvm::CallingConv::X86_ThisCall;
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

IN_ERROR Compiler::CompileModule(varuint32 m_idx)
{
  mod = new llvm::Module(m.name.str(), ctx);
  mod->setTargetTriple(machine->getTargetTriple().getTriple());
  mod->setDataLayout(machine->createDataLayout());
  intptrty = builder.getIntPtrTy(mod->getDataLayout(), 0);

  debugger.reset(Debugger::Create(*this));
  if(!debugger)
    return ERR_FATAL_FILE_ERROR;

  // Define a unique init function for performing module initialization
  init =
    TopLevelFunction(ctx, builder, CanonicalName(StringSpan::From(m.name), StringSpan::From(IN_INIT_POSTFIX)).c_str(), mod);

  // Declare C runtime function prototypes that we assume exist on the system
  memgrow = Func::Create(FuncTy::get(builder.getInt8PtrTy(0),
                                     { builder.getInt8PtrTy(0), builder.getInt64Ty(), builder.getInt64Ty(),
                                       builder.getInt64Ty()->getPointerTo() },
                                     false),
                         Func::ExternalLinkage, "_innative_internal_env_grow_memory", mod);
  memgrow->setReturnDoesNotAlias(); // This is a system memory allocation function, so the return value does not alias

  atomic_notify = Func::Create(FuncTy::get(builder.getInt32Ty(), { builder.getInt8PtrTy(0), builder.getInt32Ty() }, false),
                               Func::ExternalLinkage, "_innative_internal_env_atomic_notify", mod);
  atomic_notify->setCallingConv(llvm::CallingConv::C);

  atomic_wait32 = Func::Create(FuncTy::get(builder.getInt32Ty(),
                                           { builder.getInt8PtrTy(0), builder.getInt32Ty(), builder.getInt64Ty() }, false),
                               Func::ExternalLinkage, "_innative_internal_env_atomic_wait32", mod);
  atomic_wait32->setCallingConv(llvm::CallingConv::C);

  atomic_wait64 = Func::Create(FuncTy::get(builder.getInt32Ty(),
                                           { builder.getInt8PtrTy(0), builder.getInt64Ty(), builder.getInt64Ty() }, false),
                               Func::ExternalLinkage, "_innative_internal_env_atomic_wait64", mod);
  atomic_wait64->setCallingConv(llvm::CallingConv::C);

  Func* fn_memcpy = Func::Create(
    FuncTy::get(builder.getVoidTy(), { builder.getInt8PtrTy(0), builder.getInt8PtrTy(0), builder.getInt64Ty() }, false),
    Func::ExternalLinkage, "_innative_internal_env_memcpy", mod);

  Func* fn_memfree =
    Func::Create(FuncTy::get(builder.getVoidTy(), { builder.getInt8PtrTy(0), builder.getInt64Ty() }, false),
                 Func::ExternalLinkage, "_innative_internal_env_free_memory", mod);

  debugger->FunctionDebugInfo(init, IN_INIT_POSTFIX + (DIVIDER + std::string(m.name.str())), env.optimize != 0, true, true,
                              nullptr, 0, 0);

  functions.reserve(m.importsection.functions + m.function.n_funcdecl);
  tables.reserve(m.importsection.tables - m.importsection.functions + m.table.n_tables);
  memories.reserve(m.importsection.memories - m.importsection.tables + m.memory.n_memories);
  globals.reserve(m.importsection.globals - m.importsection.memories + m.global.n_globals);

  // Import function prototypes
  for(varuint32 i = 0; i < m.importsection.functions; ++i)
  {
    auto& imp = m.importsection.imports[i];
    functions.emplace_back();
    functions.back().intrinsic = GetIntrinsic(imp);
    if(functions.back().intrinsic == nullptr)
    {
      auto index = imp.func_desc.type_index;

      if(index >= m.type.n_functypes)
        return ERR_INVALID_TYPE_INDEX;

      auto fname    = CanonImportName(imp, env.system);
      khiter_t iter = kh_get_importhash(importhash, fname.c_str());
      if(iter != kh_end(importhash))
        functions.back().internal = static_cast<Func*>(kh_val(importhash, iter));
      else
      {
        functions.back().internal = CompileFunction(m.type.functypes[index], fname);
        functions.back().internal->setLinkage(Func::ExternalLinkage);
        int r;
        khiter_t iter            = kh_put_importhash(importhash, functions.back().internal->getName().data(), &r);
        kh_val(importhash, iter) = functions.back().internal;
      }
      functions.back().internal->setMetadata(
        IN_MEMORY_GROW_METADATA, llvm::MDNode::get(ctx, {})); // Assume all external functions invalidate the memory cache

      auto e = ResolveExport(env, imp);
      if(!e.second)
      {
        functions.back().imported = functions.back().internal;
        functions.back().imported->setLinkage(Func::ExternalLinkage);
        functions.back().imported->setCallingConv(GetCallingConvention(imp));

        auto& debugname = imp.func_desc.debug.name;
        auto canonical  = !(debugname.get()) ? functions.back().imported->getName() + DIVIDER "internal" :
                                              debugname.str() + ("_" + std::to_string(i));
        auto name = !(debugname.get()) ? std::string(imp.export_name.str()) + DIVIDER "internal" : debugname.str();
        WrapFunction(functions.back().imported, name, canonical, Func::ExternalLinkage, InternalConvention);
      }
    }
  }

  // Import tables
  for(varuint32 i = m.importsection.functions; i < m.importsection.tables; ++i)
  {
    auto canonical = CanonImportName(m.importsection.imports[i], env.system);
    auto name =
      m.importsection.imports[i].export_name.str() + std::string(DIVIDER) + m.importsection.imports[i].module_name.str();

    khiter_t iter = kh_get_importhash(importhash, canonical.c_str());
    if(iter != kh_end(importhash))
      tables.push_back(static_cast<llvm::GlobalVariable*>(kh_val(importhash, iter)));
    else
    {
      auto pair       = ResolveTrueExport(env, m.importsection.imports[i]);
      auto table_desc = ModuleTable(*pair.first, pair.second->index);
      auto ty         = GetPairType(GetTableType(table_desc->element_type)->getPointerTo(0));

      tables.push_back(CreateGlobal(ty, false, true, name, canonical, table_desc->debug.line));

      int r;
      iter                     = kh_put_importhash(importhash, tables.back()->getName().data(), &r);
      kh_val(importhash, iter) = tables.back();
    }
  }

  // Import memory
  for(varuint32 i = m.importsection.tables; i < m.importsection.memories; ++i)
  {
    auto canonical = CanonImportName(m.importsection.imports[i], env.system);
    auto name =
      m.importsection.imports[i].export_name.str() + std::string(DIVIDER) + m.importsection.imports[i].module_name.str();

    khiter_t iter = kh_get_importhash(importhash, canonical.c_str());
    if(iter != kh_end(importhash))
      memories.push_back(static_cast<llvm::GlobalVariable*>(kh_val(importhash, iter)));
    else
    {
      auto pair     = ResolveTrueExport(env, m.importsection.imports[i]);
      auto mem_desc = ModuleMemory(*pair.first, pair.second->index);
      auto ty       = GetPairType(builder.getInt8PtrTy(0));

      memories.push_back(CreateGlobal(ty, false, true, name, canonical, mem_desc->debug.line));

      auto max = builder.getInt64(
        ((mem_desc->limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? ((uint64_t)mem_desc->limits.maximum) : 0ULL) << 16);
      memories.back()->setMetadata(IN_MEMORY_MAX_METADATA, llvm::MDNode::get(ctx, { llvm::ConstantAsMetadata::get(max) }));

      int r;
      iter                     = kh_put_importhash(importhash, memories.back()->getName().data(), &r);
      kh_val(importhash, iter) = memories.back();
    }
  }

  // Import global variables
  for(varuint32 i = m.importsection.memories; i < m.importsection.globals; ++i)
  {
    auto canonical = CanonImportName(m.importsection.imports[i], env.system);
    auto name =
      m.importsection.imports[i].export_name.str() + std::string(DIVIDER) + m.importsection.imports[i].module_name.str();

    khiter_t iter = kh_get_importhash(importhash, canonical.c_str());
    if(iter != kh_end(importhash))
      globals.push_back(static_cast<llvm::GlobalVariable*>(kh_val(importhash, iter)));
    else
    {
      globals.push_back(CreateGlobal(GetLLVMType(m.importsection.imports[i].global_desc.type),
                                     !m.importsection.imports[i].global_desc.mutability, true, name, canonical,
                                     m.importsection.imports[i].global_desc.debug.line));

      int r;
      iter                     = kh_put_importhash(importhash, globals.back()->getName().data(), &r);
      kh_val(importhash, iter) = globals.back();
    }
  }

  // Cache internal function start index
  if(m.function.n_funcdecl != m.code.n_funcbody)
    return ERR_INVALID_FUNCTION_BODY;
  size_t code_index = functions.size();

  // Declare function prototypes
  for(varuint32 i = 0; i < m.function.n_funcdecl; ++i)
  {
    auto decl = m.function.funcdecl[i];
    if(decl.type_index >= m.type.n_functypes)
      return ERR_INVALID_TYPE_INDEX;

    functions.emplace_back();
    functions.back().internal = CompileFunction(m.type.functypes[decl.type_index],
                                                std::string(!decl.debug.name.size() ? "func" : decl.debug.name.str()) +
                                                  "_" + std::to_string(functions.size()) + DIVIDER + m.name.str());

    debugger->FuncDecl(functions.back().internal, m.code.funcbody[i].column, decl.debug.line, env.optimize != 0);
    auto name      = functions.back().internal->getName() + DIVIDER "external";
    auto wrapperfn = &Compiler::PassFunction;
    ExportFunction(functions.back(), wrapperfn, name.str(), name);
  }

  {
    auto ptrTy = builder.getInt8PtrTy(0);
    std::vector<llvm::Constant*> exported;

    std::transform(functions.begin(), functions.end(), std::back_inserter(exported), [ptrTy](FunctionSet& v) {
      return v.exported ? llvm::ConstantExpr::getBitCast(v.exported, ptrTy) : llvm::ConstantPointerNull::get(ptrTy);
    });

    auto gexported = llvm::ConstantArray::get(llvm::ArrayType::get(ptrTy, functions.size()), exported);

    exported_functions =
      new llvm::GlobalVariable(*mod, gexported->getType(), true, llvm::GlobalValue::PrivateLinkage, gexported);
  }

  debugger->SetSPLocation(builder, init->getSubprogram());
  auto DeclareGlobal = [this](varuint32 i, DebugInfo& debug, bool constant, llvmTy* type, const char* fallback,
                              llvm::Constant* init) -> llvm::GlobalVariable* {
    auto canonical =
      CanonicalName(StringSpan::From(m.name), StringSpan::From(!debug.name.get() ? fallback : debug.name.str()), i);
    auto name = Compiler::CppString(m.name.str()) +
                ("::" + (!debug.name.get() ? fallback + ("_" + std::to_string(i)) : std::string(debug.name.str())));
    return CreateGlobal(type, constant, false, name, canonical, debug.line, init);
  };

  // Declare tables and allocate in init function
  for(varuint32 i = 0; i < m.table.n_tables; ++i)
  {
    auto type = GetTableType(m.table.tables[i].element_type)->getPointerTo(0);
    auto pair = GetPairType(type);
    tables.push_back(DeclareGlobal(i, m.table.tables[i].debug, false, pair, "table", GetPairNull(pair)));

    uint64_t bytewidth = mod->getDataLayout().getTypeAllocSize(type->getElementType());
    if(!bytewidth)
      return ERR_INVALID_TABLE_TYPE;

    CallInst* call =
      builder.CreateCall(memgrow, { llvm::ConstantPointerNull::get(builder.getInt8PtrTy(0)),
                                    builder.getInt64(m.table.tables[i].resizable.minimum * bytewidth),
                                    builder.getInt64((m.table.tables[i].resizable.flags & WASM_LIMIT_HAS_MAXIMUM) ?
                                                       (m.table.tables[i].resizable.maximum * bytewidth) :
                                                       0),
                                    GetPairPtr(tables.back(), 1) });

    call->setCallingConv(memgrow->getCallingConv());

    InsertConditionalTrap(builder.CreateICmpEQ(builder.CreatePtrToInt(call, intptrty), CInt::get(intptrty, 0)));
    builder.CreateStore(builder.CreatePointerCast(call, type), GetPairPtr(tables.back(), 0), false);
  }

  // Declare linear memory spaces and allocate in init function
  for(varuint32 i = 0; i < m.memory.n_memories; ++i)
  {
    MemoryDesc& mem = m.memory.memories[i];
    auto type       = builder.getInt8PtrTy(0);
    auto pair       = GetPairType(type);
    auto sz         = builder.getInt64(((uint64_t)mem.limits.minimum) << 16);
    auto max =
      builder.getInt64(((mem.limits.flags & WASM_LIMIT_HAS_MAXIMUM) ? ((uint64_t)mem.limits.maximum) : 0ULL) << 16);
    memories.push_back(DeclareGlobal(i, mem.debug, false, pair, "linearmemory", GetPairNull(pair)));
    memories.back()->setMetadata(IN_MEMORY_MAX_METADATA, llvm::MDNode::get(ctx, { llvm::ConstantAsMetadata::get(max) }));

    CallInst* call =
      builder.CreateCall(memgrow, { llvm::ConstantPointerNull::get(type), sz, max, GetPairPtr(memories.back(), 1) });
    call->setCallingConv(memgrow->getCallingConv());
    InsertConditionalTrap(builder.CreateICmpEQ(builder.CreatePtrToInt(call, intptrty), CInt::get(intptrty, 0)));
    builder.CreateStore(call, GetPairPtr(memories.back(), 0), false);
  }

  IN_ERROR err;

  // Declare global variables
  for(varuint32 i = 0; i < m.global.n_globals; ++i)
  {
    llvm::Constant* init;
    if(err = CompileInitConstant(m.global.globals[i].init, m, init))
      return err;

    globals.push_back(DeclareGlobal(i, m.global.globals[i].desc.debug, !m.global.globals[i].desc.mutability,
                                    GetLLVMType(m.global.globals[i].desc.type), "globalvariable", init));
  }

  debugger->SetSPLocation(builder, init->getSubprogram());

  // Process data section by appending to the init function
  for(varuint32 i = 0; i < m.data.n_data; ++i)
  {
    DataInit& d = m.data.data[i]; // First we declare a constant array that stores the data in the EXE
    auto data = llvm::ConstantDataArray::get(ctx, llvm::makeArrayRef<uint8_t>(d.data.get(), d.data.get() + d.data.size()));
    auto val  = new llvm::GlobalVariable(*mod, data->getType(), true, llvm::GlobalValue::LinkageTypes::PrivateLinkage, data,
                                        CanonicalName(StringSpan{ 0, 0 }, StringSpan::From("data"), i));
    debugger->DebugGlobal(val, val->getName(), 0);

    llvm::Constant* offset;
    if(err = CompileInitConstant(d.offset, m, offset))
      return err;

    // Then we create a memcpy call that copies this data to the appropriate location in the init function
    builder
      .CreateCall(fn_memcpy,
                  { builder.CreateInBoundsGEP(builder.CreateLoad(GetPairPtr(memories[d.index], 0)), offset),
                    builder.CreateInBoundsGEP(data->getType(), val, { builder.getInt32(0), builder.getInt32(0) }),
                    builder.getInt64(GetTotalSize(data->getType())) })
      ->setCallingConv(fn_memcpy->getCallingConv());
  }

  // Process element section by appending to the init function
  for(varuint32 i = 0; i < m.element.n_elements; ++i)
  {
    TableInit& e = m.element.elements[i]; // First we declare a constant array that stores the data in the EXE
    TableDesc* t = ModuleTable(m, e.index);
    if(!t)
      return ERR_INVALID_TABLE_INDEX;

    if(t->element_type == TE_funcref)
    {
      llvmTy* target = GetLLVMType(TE_funcref);
      llvm::Constant* offset;
      if(err = CompileInitConstant(e.offset, m, offset))
        return err;

      // Go through and resolve all indices to function pointers
      for(varuint32 j = 0; j < e.n_elements; ++j)
      {
        if(e.elements[j] >= functions.size())
          return ERR_INVALID_FUNCTION_INDEX;

        // Store function pointer in correct table memory location
        auto ptr =
          builder.CreateGEP(builder.CreateLoad(GetPairPtr(tables[e.index], 0)),
                            { builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)), builder.getInt32(0) });
        builder.CreateAlignedStore(builder.CreatePointerCast(functions[e.elements[j]].internal, target), ptr,
                                   mod->getDataLayout().getPointerSize(), false);

        varuint32 index = GetFirstType(ModuleFunctionType(m, e.elements[j]));
        if(index == (varuint32)~0)
          return ERR_INVALID_FUNCTION_INDEX;

        ptr = builder.CreateGEP(builder.CreateLoad(GetPairPtr(tables[e.index], 0)),
                                { builder.CreateAdd(offset, CInt::get(offset->getType(), j, true)), builder.getInt32(1) });
        builder.CreateAlignedStore(builder.getInt32(index), ptr, 4, false);
      }
    }
  }

  // Terminate init function
  builder.CreateRetVoid();

  // Create cleanup function
  exit =
    TopLevelFunction(ctx, builder, CanonicalName(StringSpan::From(m.name), StringSpan::From(IN_EXIT_POSTFIX)).c_str(), mod);

  debugger->FunctionDebugInfo(exit, IN_EXIT_POSTFIX + (DIVIDER + std::string(m.name.str())), env.optimize != 0, true, true,
                              nullptr, 0, 0);
  debugger->SetSPLocation(builder, exit->getSubprogram());

  for(size_t i = m.importsection.memories - m.importsection.tables; i < memories.size();
      ++i) // Don't accidentally delete imported linear memories
    builder
      .CreateCall(fn_memfree,
                  { builder.CreateLoad(GetPairPtr(memories[i], 0)), builder.CreateLoad(GetPairPtr(memories[i], 1)) })
      ->setCallingConv(fn_memfree->getCallingConv());

  for(size_t i = m.importsection.tables - m.importsection.functions; i < tables.size();
      ++i) // Don't accidentally delete imported tables
    builder
      .CreateCall(fn_memfree,
                  { builder.CreatePointerCast(builder.CreateLoad(GetPairPtr(tables[i], 0)), builder.getInt8PtrTy(0)),
                    builder.CreateLoad(GetPairPtr(tables[i], 1)) })
      ->setCallingConv(fn_memfree->getCallingConv());

  // Terminate cleanup function
  builder.CreateRetVoid();

  // Generate code for each function body
  for(varuint32 i = 0; i < m.code.n_funcbody; ++i)
  {
    assert(!functions[code_index].imported);
    Func* fn = functions[code_index].internal;

    if(fn)
    {
      if((err = CompileFunctionBody(fn, code_index, functions[code_index].memlocal, m.function.funcdecl[i],
                                    m.code.funcbody[i])) < 0)
        return err;
    }
    ++code_index;
  }

  // If the start section exists, lift the start function to the context so our environment knows about it.
  if(m.knownsections & (1 << WASM_SECTION_START))
  {
    if(m.start >= functions.size())
      return ERR_INVALID_START_FUNCTION;
    assert(!functions[m.start].imported);
    start = functions[m.start].internal;
  }

  {
    auto ptrTy = builder.getInt8PtrTy(0);

    std::vector<llvm::Constant*> vtables;
    std::vector<llvm::Constant*> vmemories;
    std::vector<llvm::Constant*> vglobals;

    std::transform(tables.begin(), tables.end(), std::back_inserter(vtables),
                   [ptrTy](llvm::GlobalVariable* v) { return llvm::ConstantExpr::getBitCast(v, ptrTy); });
    std::transform(memories.begin(), memories.end(), std::back_inserter(vmemories),
                   [ptrTy](llvm::GlobalVariable* v) { return llvm::ConstantExpr::getBitCast(v, ptrTy); });
    std::transform(globals.begin(), globals.end(), std::back_inserter(vglobals),
                   [ptrTy](llvm::GlobalVariable* v) { return llvm::ConstantExpr::getBitCast(v, ptrTy); });

    auto gname     = llvm::ConstantDataArray::getString(ctx, llvm::StringRef(m.name.str(), m.name.size()));
    auto gtables   = llvm::ConstantArray::get(llvm::ArrayType::get(ptrTy, vtables.size()), vtables);
    auto gmemories = llvm::ConstantArray::get(llvm::ArrayType::get(ptrTy, vmemories.size()), vmemories);
    auto gglobals  = llvm::ConstantArray::get(llvm::ArrayType::get(ptrTy, vglobals.size()), vglobals);
    std::array<llvm::Constant*, 10> values = {
      new llvm::GlobalVariable(*mod, gname->getType(), true, llvm::GlobalValue::PrivateLinkage, gname),
      builder.getInt32(m.version),
      builder.getInt32((uint32)tables.size()),
      new llvm::GlobalVariable(*mod, gtables->getType(), true, llvm::GlobalValue::PrivateLinkage, gtables),
      builder.getInt32((uint32)memories.size()),
      new llvm::GlobalVariable(*mod, gmemories->getType(), true, llvm::GlobalValue::PrivateLinkage, gmemories),
      builder.getInt32((uint32)globals.size()),
      new llvm::GlobalVariable(*mod, gglobals->getType(), true, llvm::GlobalValue::PrivateLinkage, gglobals),
      builder.getInt32((uint32)functions.size()),
      exported_functions,
    };
    auto metadata = llvm::ConstantStruct::getAnon(values);
    auto v = new llvm::GlobalVariable(*mod, metadata->getType(), true, llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                                      metadata, CanonicalName(StringSpan(), StringSpan::From(IN_METADATA_PREFIX), m_idx));
    v->setDLLStorageClass(llvm::GlobalValue::DLLExportStorageClass);
  }

  return ERR_SUCCESS;
}

void Compiler::PostOrderTraversal(llvm::Function* f)
{
  if(!f || f->isDeclaration() || f->getMetadata(IN_MEMORY_GROW_METADATA) != nullptr ||
     f->getMetadata(IN_FUNCTION_TRAVERSED) != nullptr)
    return;

  f->setMetadata(IN_FUNCTION_TRAVERSED, llvm::MDNode::get(f->getContext(), {}));

  for(auto& i : llvm::instructions(f))
  {
    if(auto cs = llvm::CallSite(&i))
    {
      if(auto called = cs.getCalledFunction())
      {
        PostOrderTraversal(called);
        if(auto md = called->getMetadata(IN_MEMORY_GROW_METADATA))
        {
          f->setMetadata(IN_MEMORY_GROW_METADATA, md);
          break;
        }
      }
    }
  }
}

// Runs a pass to propagate all memory_grow metadata up the call graph, then adds store instructions where necessary
void Compiler::AddMemLocalCaching()
{
  if(!memories.size())
    return;

  for(auto fn : functions) // Because it's crucial we cover the entire call graph, we just go through every single
                           // function that has a definition
  {
    PostOrderTraversal(fn.internal);
    PostOrderTraversal(fn.exported);
    PostOrderTraversal(fn.imported);
  }

  for(auto fn : functions)
  {
    if(fn.internal)
      fn.internal->setMetadata(IN_FUNCTION_TRAVERSED,
                               0); // This cleanup is optional, but keeps the IR clean when inspecting it.
    if(fn.exported)
      fn.exported->setMetadata(IN_FUNCTION_TRAVERSED, 0);
    if(fn.imported)
      fn.imported->setMetadata(IN_FUNCTION_TRAVERSED, 0);

    if(fn.memlocal != nullptr)
    {
      Func* f = (fn.memlocal->getFunction() == fn.exported) ?
                  fn.exported :
                  ((fn.memlocal->getFunction() == fn.imported) ?
                     fn.imported :
                     ((fn.memlocal->getFunction() == fn.internal) ? fn.internal : nullptr));

      assert(f != nullptr);
      for(auto& i : llvm::instructions(f))
      {
        if(auto cs = llvm::CallSite(&i))
        {
          if(auto called = cs.getCalledFunction())
          {
            if(called->getMetadata(IN_MEMORY_GROW_METADATA) != nullptr)
            {
              builder.SetInsertPoint(
                &i); // Setting the insert point doesn't actually gaurantee the instructions come after the call
              auto load = builder.CreateLoad(GetPairPtr(memories[0], 0));
              load->moveAfter(&i); // So we manually move them after the call instruction just to be sure.
              builder.CreateStore(load, fn.memlocal, false)->moveAfter(load);
            }
          }
        }
      }
    }
  }
}

// Resolve all exports in the module they originated from (in case any module is exporting an import)
void Compiler::ResolveModuleExports(const Environment* env, Module* root, llvm::LLVMContext& context)
{
  auto wrapperfn = &Compiler::PassFunction;

  for(varuint32 j = 0; j < root->exportsection.n_exports; ++j)
  {
    Module* m = root;
    Export* e = &m->exportsection.exports[j];

    // Calculate the canonical name we wish to export as using the initial export object
    auto mname     = (m->knownsections & WASM_SECTION_C_LINKAGE) ? StringSpan{ 0, 0 } : StringSpan::From(m->name);
    auto canonical = CanonicalName(mname, StringSpan::From(e->name));

    // Resolve the export/module pair to the concrete source
    for(;;)
    {
      Import* imp = ResolveImport(*m, *e);

      if(!imp)
        break;

      auto pair = ResolveExport(*env, *imp);
      m         = pair.first;
      e         = pair.second;
    }

    Compiler* compiler = m->cache;

    switch(e->kind)
    {
    case WASM_KIND_FUNCTION:
      if(!compiler->functions[e->index].exported)
      {
        auto name = std::string(e->name.str()) + DIVIDER + m->name.str();
        compiler->ExportFunction(compiler->functions[e->index], wrapperfn, name, canonical);
        compiler->functions[e->index].exported->setDLLStorageClass(
          llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      }
      else
        llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, compiler->functions[e->index].exported)
          ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_TABLE:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, compiler->tables[e->index])
        ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_MEMORY:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, compiler->memories[e->index])
        ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    case WASM_KIND_GLOBAL:
      llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, compiler->globals[e->index])
        ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      break;
    }
  }
}

IN_ERROR innative::CompileEnvironment(Environment* env, const char* outfile, bool generics)
{
  if(!outfile || !outfile[0])
    return ERR_FATAL_NO_OUTPUT_FILE;

  path file = utility::GetPath(outfile);

  if(!file.is_absolute())
    file = utility::GetWorkingDir() / file;

  // Construct the LLVM environment and current working directories
  if(!env->context)
    env->context = new llvm::LLVMContext();

  llvm::IRBuilder<> builder(*env->context);
  bool has_start = false;
  IN_ERROR err   = ERR_SUCCESS;

  std::string triple = llvm::sys::getProcessTriple();

  // Set up our target architecture, necessary up here so our code generation knows how big a pointer is
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string llvm_err;
  auto arch = llvm::TargetRegistry::lookupTarget(triple, llvm_err);

  if(!arch)
  {
    llvm::errs() << llvm_err;
    return ERR_FATAL_UNKNOWN_TARGET;
  }

  for(varuint32 i = 0; i < env->n_modules; ++i)
    if(env->modules[i].knownsections & (1 << WASM_SECTION_START))
      has_start = true;

  if((!has_start || env->flags & ENV_NO_INIT) && !(env->flags & ENV_LIBRARY))
  {
    env->flags |= ENV_LIBRARY; // Attempting to compile a library as an EXE is a common error, so we fix it for you.
    fprintf(
      env->log,
      "WARNING: Compiling dynamic library because no start function was found! If this was intended, use '-f library' next time.\n");
  }

  // Detect current CPU feature set and create machine target for LLVM
  llvm::TargetOptions opt;
  auto RM = llvm::Optional<llvm::Reloc::Model>();
#ifdef IN_PLATFORM_POSIX
  if(env->flags & ENV_LIBRARY)
    RM = llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
#endif
  llvm::SubtargetFeatures subtarget_features;
  llvm::StringMap<bool> feature_map;
  if(llvm::sys::getHostCPUFeatures(feature_map))
  {
    for(auto& feature : feature_map)
    {
      subtarget_features.AddFeature(feature.first(), feature.second);
    }
  }
  auto machine =
    arch->createTargetMachine(triple, llvm::sys::getHostCPUName(), subtarget_features.getString(), opt, RM, llvm::None);

  if(!env->n_modules)
    return ERR_FATAL_NO_MODULES;

  if(env->optimize & ENV_OPTIMIZE_FAST_MATH)
  {
    llvm::FastMathFlags fmf;
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_REASSOCIATE)
      fmf.setAllowReassoc();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_NAN)
      fmf.setNoNaNs();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_INF)
      fmf.setNoInfs();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO)
      fmf.setNoSignedZeros();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL)
      fmf.setAllowReassoc();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_CONTRACT)
      fmf.setAllowContract();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS)
      fmf.setApproxFunc();
    builder.setFastMathFlags(fmf);
  }

  std::vector<Module*> new_modules;
  has_start = false;

  // Compile all modules
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    if(!i || !env->modules[i].cache) // Always recompile the 0th module because it stores the main entry point.
    {
      if(env->modules[i].cache)
        DeleteCache(*env, env->modules[i]);
      env->modules[i].cache =
        new Compiler{ *env,    env->modules[i], *env->context,        0,
                      builder, machine,         kh_init_importhash(), GetLinkerObjectPath(*env, env->modules[i], file) };
      if(!env->modules[i].cache->objfile.empty())
        remove(env->modules[i].cache->objfile);

      if((err = env->modules[i].cache->CompileModule(i)) < 0)
        return err;
      new_modules.push_back(env->modules + i);
    }
    has_start |= env->modules[i].cache->start != nullptr;
  }

  if((!has_start || env->flags & ENV_NO_INIT) && !(env->flags & ENV_LIBRARY))
    return ERR_INVALID_START_FUNCTION;

  for(auto m : new_modules)
    Compiler::ResolveModuleExports(env, m, *env->context);

  for(auto m : new_modules)
    m->cache->AddMemLocalCaching();

  // Create cleanup function
  Compiler& mainctx = *env->modules[0].cache;
  Func* cleanup     = Compiler::TopLevelFunction(*env->context, builder, IN_EXIT_FUNCTION, mainctx.mod);
  mainctx.debugger->FunctionDebugInfo(cleanup, IN_EXIT_FUNCTION, mainctx.env.optimize != 0, true, true, nullptr, 0, 0);
  mainctx.debugger->SetSPLocation(mainctx.builder, cleanup->getSubprogram());

  builder.CreateCall(mainctx.exit, {})->setCallingConv(mainctx.exit->getCallingConv());

  for(size_t i = 1; i < env->n_modules; ++i)
  {
    Func* stub = Func::Create(env->modules[i].cache->exit->getFunctionType(), env->modules[i].cache->exit->getLinkage(),
                              env->modules[i].cache->exit->getName(),
                              mainctx.mod); // Create function prototype in main module
    builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
  }

  builder.CreateRetVoid();

  // Create main function that calls all init functions for all modules and all start functions
  Func* main = Compiler::TopLevelFunction(*env->context, builder, IN_INIT_FUNCTION, nullptr);
  mainctx.debugger->FunctionDebugInfo(main, IN_INIT_FUNCTION, mainctx.env.optimize != 0, true, true, nullptr, 0, 0);
  mainctx.debugger->SetSPLocation(mainctx.builder, main->getSubprogram());

  builder.CreateCall(mainctx.init, {})->setCallingConv(mainctx.init->getCallingConv());

  for(size_t i = 1; i < env->n_modules; ++i)
  {
    Func* stub = Func::Create(env->modules[i].cache->init->getFunctionType(), env->modules[i].cache->init->getLinkage(),
                              env->modules[i].cache->init->getName(),
                              mainctx.mod); // Create function prototype in main module
    builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
  }

  // Call every single start function in all modules AFTER we initialize them.
  if(mainctx.start != nullptr)
    builder.CreateCall(mainctx.start, {})->setCallingConv(mainctx.start->getCallingConv());

  for(size_t i = 1; i < env->n_modules; ++i)
  {
    if(env->modules[i].cache->start != nullptr)
    {
      // Catch the case where an import from this module is being called from another module
      Func* stub = mainctx.mod->getFunction(env->modules[i].cache->start->getName());
      if(!stub)
      {
        auto alias = mainctx.mod->getNamedAlias(env->modules[i].cache->start->getName());
        if(alias)
          stub = llvm::cast<Func>(alias->getAliasee());
        else
          stub = Func::Create(env->modules[i].cache->start->getFunctionType(), env->modules[i].cache->start->getLinkage(),
                              env->modules[i].cache->start->getName(),
                              mainctx.mod); // Create function prototype in main module
      }
      builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
    }
  }

  if(env->flags & ENV_LIBRARY)
  {
    if(env->flags & ENV_NO_INIT)
    {
      main->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      cleanup->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
    }
    builder.CreateRetVoid();
  }
  else // If this isn't a DLL, then the init function is actual the process entry point, which must clean up and
       // call _exit()
  {
    // Get prototype for the environment exit function
    Func* fn_exit = Func::Create(FuncTy::get(builder.getVoidTy(), { builder.getInt32Ty() }, false), Func::ExternalLinkage,
                                 "_innative_internal_env_exit", mainctx.mod);
    fn_exit->setDoesNotReturn();

    builder.CreateCall(cleanup, {})->setCallingConv(cleanup->getCallingConv()); // Call cleanup function
    builder.CreateCall(fn_exit, builder.getInt32(0))->setCallingConv(fn_exit->getCallingConv());
    main->setDoesNotReturn();
    builder.CreateUnreachable(); // This function never returns
  }

  mainctx.mod->getFunctionList().push_back(main);

#ifdef IN_PLATFORM_WIN32
  // The windows linker requires this to be defined. It's not actually used, just... defined.
  new llvm::GlobalVariable(*mainctx.mod, builder.getInt32Ty(), false, llvm::GlobalValue::ExternalLinkage,
                           builder.getInt32(
                             0), // This value doesn't matter, it just needs to be something so LLVM exports the symbol.
                           "_fltused");

  if(env->flags & ENV_LIBRARY)
  {
    Func* mainstub =
      Func::Create(FuncTy::get(builder.getInt32Ty(),
                               { builder.getInt8PtrTy(), mainctx.builder.getInt32Ty(), builder.getInt8PtrTy() }, false),
                   Func::ExternalLinkage, IN_INIT_FUNCTION "-stub");
    mainstub->setCallingConv(llvm::CallingConv::X86_StdCall);
    BB* entryblock = BB::Create(*env->context, "entry", mainstub);
    builder.SetInsertPoint(entryblock);
    mainctx.debugger->FuncDecl(mainstub, 0, mainctx.m.start_line, mainctx.env.optimize != 0);
    mainctx.debugger->SetSPLocation(builder, mainstub->getSubprogram());

    if(!(env->flags & ENV_NO_INIT)) // Only actually initialize things on DLL load if we actually want to, otherwise
                                    // create a stub function
    {
      BB* endblock  = BB::Create(*env->context, "end", mainstub);
      BB* initblock = BB::Create(*env->context, "init", mainstub);
      BB* exitblock = BB::Create(*env->context, "exit", mainstub);

      llvm::SwitchInst* s = builder.CreateSwitch(mainstub->arg_begin() + 1, endblock, 2);
      s->addCase(mainctx.builder.getInt32(1), initblock); // DLL_PROCESS_ATTACH
      s->addCase(mainctx.builder.getInt32(0), exitblock); // DLL_PROCESS_DETACH

      builder.SetInsertPoint(initblock);
      builder.CreateCall(main, {})->setCallingConv(main->getCallingConv());
      builder.CreateBr(endblock);

      builder.SetInsertPoint(exitblock);
      builder.CreateCall(cleanup, {})->setCallingConv(cleanup->getCallingConv());
      builder.CreateBr(endblock);

      builder.SetInsertPoint(endblock);
    }

    builder.CreateRet(builder.getInt32(1)); // Always return 1, since an error will trap instead.
    mainctx.mod->getFunctionList().push_back(mainstub);
  }
#endif

  if(env->optimize & ENV_OPTIMIZE_OMASK)
    OptimizeModules(env);

  return LinkEnvironment(env, outfile);
}

void* innative::LoadJITFunction(void* env, const char* s)
{
  auto sym = reinterpret_cast<Environment*>(env)->jit->Lookup(s);
  if(!sym)
    return 0;
  return reinterpret_cast<void*>(sym.get().getAddress());
}

#include "tools.h"

IN_ERROR innative::CompileEnvironmentJIT(Environment* env, bool expose_process)
{
  // Set up our target architecture, necessary up here so our code generation knows how big a pointer is
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  // Build the JIT and LLVM contexts
  if(!env->jit)
  {
    if(!env->context)
      env->context = new llvm::LLVMContext();

    auto JTMB = llvm::orc::JITTargetMachineBuilder::detectHost();

    if(!JTMB)
      return ERR_JIT_TARGET_MACHINE_FAILURE;

    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if(!DL)
      return ERR_JIT_DATA_LAYOUT_ERROR;

    // If the context already exists, we take ownership of it
    env->jit =
      new JITContext(std::move(*JTMB), std::move(*DL), std::unique_ptr<llvm::LLVMContext>(env->context), env->whitelist);
    // env->context = nullptr;

    if(expose_process)
      if(env->jit->CompileObject(0))
        return ERR_JIT_LINK_PROCESS_FAILURE;
  }

  auto machine = env->jit->GetTargetMachine();
  if(!machine)
    return ERR_JIT_TARGET_MACHINE_FAILURE;

  llvm::IRBuilder<> builder(*env->context);
  IN_ERROR err = ERR_SUCCESS;

  if(!env->n_modules)
    return ERR_FATAL_NO_MODULES;

  if(env->optimize & ENV_OPTIMIZE_FAST_MATH)
  {
    llvm::FastMathFlags fmf;
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_REASSOCIATE)
      fmf.setAllowReassoc();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_NAN)
      fmf.setNoNaNs();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_INF)
      fmf.setNoInfs();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO)
      fmf.setNoSignedZeros();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL)
      fmf.setAllowReassoc();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_CONTRACT)
      fmf.setAllowContract();
    if(env->optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS)
      fmf.setApproxFunc();
    builder.setFastMathFlags(fmf);
  }

  std::vector<Module*> new_modules;

  // Compile all modules
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    if(!i || !env->modules[i].cache) // Always recompile the 0th module because it stores the main entry point.
    {
      if(env->modules[i].cache)
        DeleteCache(*env, env->modules[i]);
      env->modules[i].cache = new Compiler{ *env,    env->modules[i],     env->jit->GetContext(), 0,
                                            builder, machine.get().get(), kh_init_importhash(),   path() };

      if((err = env->modules[i].cache->CompileModule(i)) < 0)
        return err;

      new_modules.push_back(env->modules + i);
    }
  }

  for(auto m : new_modules)
    Compiler::ResolveModuleExports(env, m, *env->context);

  for(auto m : new_modules)
    m->cache->AddMemLocalCaching();

  // Create cleanup function, but only if the user would have to manually call it
  if(env->flags & ENV_NO_INIT)
  {
    Compiler& mainctx = *env->modules[0].cache;
    Func* cleanup     = Compiler::TopLevelFunction(*env->context, builder, IN_EXIT_FUNCTION, mainctx.mod);
    mainctx.debugger->FunctionDebugInfo(cleanup, IN_EXIT_FUNCTION, mainctx.env.optimize != 0, true, true, nullptr, 0, 0);
    mainctx.debugger->SetSPLocation(mainctx.builder, cleanup->getSubprogram());

    builder.CreateCall(mainctx.exit, {})->setCallingConv(mainctx.exit->getCallingConv());

    for(size_t i = 1; i < env->n_modules; ++i)
    {
      Func* stub = Func::Create(env->modules[i].cache->exit->getFunctionType(), env->modules[i].cache->exit->getLinkage(),
                                env->modules[i].cache->exit->getName(),
                                mainctx.mod); // Create function prototype in main module
      builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
    }

    // For JIT, we always expose the main and cleanup functions
    builder.CreateRetVoid();
    cleanup->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);

    // Create main function that calls all init functions for all modules and all start functions
    Func* main = Compiler::TopLevelFunction(*env->context, builder, IN_INIT_FUNCTION, nullptr);
    mainctx.debugger->FunctionDebugInfo(main, IN_INIT_FUNCTION, mainctx.env.optimize != 0, true, true, nullptr, 0, 0);
    mainctx.debugger->SetSPLocation(mainctx.builder, main->getSubprogram());

    builder.CreateCall(mainctx.init, {})->setCallingConv(mainctx.init->getCallingConv());

    for(size_t i = 1; i < env->n_modules; ++i)
    {
      Func* stub = Func::Create(env->modules[i].cache->init->getFunctionType(), env->modules[i].cache->init->getLinkage(),
                                env->modules[i].cache->init->getName(),
                                mainctx.mod); // Create function prototype in main module
      builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
    }

    // Call every single start function in all modules AFTER we initialize them.
    if(mainctx.start != nullptr)
      builder.CreateCall(mainctx.start, {})->setCallingConv(mainctx.start->getCallingConv());

    for(size_t i = 1; i < env->n_modules; ++i)
    {
      if(env->modules[i].cache->start != nullptr)
      {
        // Catch the case where an import from this module is being called from another module
        Func* stub = mainctx.mod->getFunction(env->modules[i].cache->start->getName());
        if(!stub)
        {
          auto alias = mainctx.mod->getNamedAlias(env->modules[i].cache->start->getName());
          if(alias)
            stub = llvm::cast<Func>(alias->getAliasee());
          else
            stub = Func::Create(env->modules[i].cache->start->getFunctionType(), env->modules[i].cache->start->getLinkage(),
                                env->modules[i].cache->start->getName(),
                                mainctx.mod); // Create function prototype in main module
        }
        builder.CreateCall(stub, {})->setCallingConv(stub->getCallingConv());
      }
    }

    builder.CreateRetVoid();
    main->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);

    mainctx.mod->getFunctionList().push_back(main);
  }

  // JIT all modules
  for(varuint32 i = 0; i < env->n_modules; ++i)
  {
    if(env->jit->CompileModule(std::unique_ptr<llvm::Module>(env->modules[i].cache->mod)))
      return ERR_JIT_COMPILE_FAILURE;
  }

  // If we're auto-initializing, call all the init functions, then all the start functions. Unfortunately, LLVM deletes
  // function names after we JIT so we have to store them before calling them
  if(!(env->flags & ENV_NO_INIT))
  {
    std::vector<llvm::StringRef> refs;

    for(size_t i = 0; i < env->n_modules; ++i)
      refs.push_back(env->modules[i].cache->init->getName());
    for(size_t i = 0; i < env->n_modules; ++i)
      if(env->modules[i].cache->start)
        refs.push_back(env->modules[i].cache->start->getName());

    for(auto n : refs)
      reinterpret_cast<IN_Entrypoint>(env->jit->Lookup(n).get().getAddress())();
  }

  return ERR_SUCCESS;
}
