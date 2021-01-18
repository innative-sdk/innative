// Copyright (c)2021 Fundament Software
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

template<typename... Args>
inline IN_ERROR LogErrorLLVM(const Environment& env, const char* format, IN_ERROR err, llvm::Error&& llvmerr, Args... args)
{
  if(env.loglevel < LOG_FATAL)
    return err;

  llvm::handleAllErrors(std::move(llvmerr), [&](const llvm::ErrorInfoBase& info) {
    char buf[32];
    (*env.loghook)(&env, format, EnumToString(ERR_ENUM_MAP, (int)err, buf, sizeof(buf)), args..., info.message().c_str());
    (*env.loghook)(&env, "\n");
  });
  return err;
}

Compiler::Compiler(Environment& _env, Module& _m, llvm::LLVMContext& _ctx, llvm::Module* _mod, llvm::IRBuilder<>& _builder,
                   llvm::TargetMachine* _machine, kh_importhash_t* _importhash, const path& _objfile) :
  env(_env),
  m(_m),
  ctx(_ctx),
  mod(_mod),
  builder(_builder),
  machine(_machine),
  importhash(_importhash),
  objfile(_objfile),
  exported_functions(0),
  memlocal(0),
  intptrty(0),
  mempairty(0),
  init(0),
  exit(0),
  start(0),
  fn_memgrow(0),
  fn_memcpy(0),
  fn_memmove(0),
  fn_memset(0),
  fn_memcmp(0),
  atomic_notify(0),
  atomic_wait32(0),
  atomic_wait64(0),
  current(0),
  instruction_counter(0)
{
  memset(logbuf, 0, sizeof(logbuf));
}

void Compiler::_logprefix()
{
  if(current != nullptr)
  {
    auto name = current->getName().str();
    (*env.loghook)(&env, "[%s:%s] ", m.name.str(), name.c_str());
  }
  else
    (*env.loghook)(&env, "[%s] ", m.name.str());
}

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

Func* Compiler::PassFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                             llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{
  // fn->setCallingConv(callconv);
  fn->setLinkage(linkage);
  fn->setName(canonical);
  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(fn, name, true, true, nullptr, fn->getSubprogram()->getFile(),
                                fn->getSubprogram()->getLine(), 0);

  debugger->SetSPLocation(builder, fn->getSubprogram());
  return fn;
}

Func* Compiler::WrapFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                             llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{
  Func* wrap = Func::Create(fn->getFunctionType(), linkage, canonical, mod);
  wrap->setCallingConv(callconv);
  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(wrap, name, true, true, nullptr, fn->getSubprogram()->getFile(),
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

Func* Compiler::GenericFunction(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                                llvm::GlobalValue::LinkageTypes linkage, llvm::CallingConv::ID callconv)
{ // generalize function into void(char* params, char* results)
  auto functy   = FuncTy::get(llvmTy::getVoidTy(ctx), { llvmTy::getInt8PtrTy(ctx), llvmTy::getInt8PtrTy(ctx) }, false);
  Func* generic = Func::Create(functy, linkage, canonical, mod);
  generic->setCallingConv(callconv);
  generic->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);

  if(fn->getSubprogram())
    debugger->FunctionDebugInfo(generic, name, true, true, nullptr, fn->getSubprogram()->getFile(),
                                fn->getSubprogram()->getLine(), 0);

  debugger->SetSPLocation(builder, generic->getSubprogram());
  auto prev = builder.GetInsertBlock();

  BB* bb = BB::Create(ctx, "generic_block", generic);
  builder.SetInsertPoint(bb);

  std::vector<llvmVal*> values;
  auto params  = generic->getArg(0);
  auto results = generic->getArg(1);

  uint32_t offset = 0;
  for(auto& arg : fn->args())
  {
    values.push_back(builder.CreateLoad(builder.CreateBitCast(
      builder.CreateInBoundsGEP(params, { builder.getInt32(offset) }), arg.getType()->getPointerTo())));
    offset += uint32_t(mod->getDataLayout().getTypeSizeInBits(arg.getType()) / 8);
    // TODO: structs must be broken apart and repacked
  }

  auto val = builder.CreateCall(fn, values);
  val->setCallingConv(fn->getCallingConv());
  val->setAttributes(fn->getAttributes());

  offset   = 0;
  auto ret = fn->getReturnType();
  if(ret->isStructTy())
  {
    for(unsigned int i = 0; i < ret->getStructNumElements(); ++i)
    {
      builder.CreateStore(builder.CreateExtractValue(val, { i }),
                          builder.CreateBitCast(builder.CreateInBoundsGEP(results, { builder.getInt32(offset) }),
                                                ret->getStructElementType(i)->getPointerTo()),
                          false);
      offset += uint32_t(mod->getDataLayout().getTypeSizeInBits(ret->getStructElementType(i)) / 8);
    }
  }
  else if(!ret->isVoidTy())
    builder.CreateStore(val, builder.CreateBitCast(results, ret->getPointerTo()), false);

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
    return _log("%s: Found empty stack while trying to pop %s", ERR_INVALID_VALUE_STACK,
                EnumToString(TYPE_ENCODING_MAP, ty, logbuf, sizeof(logbuf)));
  else if(!values.Peek()) // polymorphic value
  {
    switch(ty)
    {
    case TE_i32:
    case TE_i64:
    case TE_f32:
    case TE_f64: v = llvm::Constant::getNullValue(GetLLVMType(ty)); break;
    default:
      return _log("%s: can't pop polymorphic value of type %s", ERR_INVALID_TYPE,
                  EnumToString(TYPE_ENCODING_MAP, ty, logbuf, sizeof(logbuf)));
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
    _logtype("%s: Tried to pop %s but found ", ERR_INVALID_TYPE, values.Peek()->getType(),
                 EnumToString(TYPE_ENCODING_MAP, ty, logbuf, sizeof(logbuf)));
  else if(peek)
    v = values.Peek();
  else
    v = values.Pop();

  return ERR_SUCCESS;
}

IN_ERROR Compiler::PopSig(varsint64 sig, llvm::SmallVector<llvmVal*, 2>& v, bool peek, bool loop)
{
  if(sig < 0)
  {
    if(loop)
      return ERR_SUCCESS;
    llvmVal* push;
    IN_ERROR err = PopType(static_cast<varsint7>(sig), push, peek);
    if(push)
      v.push_back(push);
    return err;
  }

  if(sig >= m.type.n_functypes)
    return _log("%s: type index %u is greater than n_functypes (%u)", ERR_INVALID_FUNCTION_SIG, sig, m.type.n_functypes);
  auto n_sig = loop ? m.type.functypes[sig].n_params : m.type.functypes[sig].n_returns;
  auto sigs  = loop ? m.type.functypes[sig].params : m.type.functypes[sig].returns;
  if(n_sig == 0)
    return ERR_SUCCESS;
  if(n_sig == 1)
  {
    llvmVal* push;
    IN_ERROR err = PopType(sigs[0], push, peek);
    assert(push != nullptr);
    v.push_back(push);
    return err;
  }
  if(!values.Size())
    return _log("%s: Value stack cannot be empty if signature has length > 0", ERR_INVALID_VALUE_STACK);

  bool polyflag = false;
  for(varuint32 i = 0; i < n_sig; ++i)
  {
    if(!polyflag && i > values.Size()) // Value stack is wrong size
      return _log("%s: Value stack was empty when trying to pop %s", ERR_INVALID_VALUE_STACK,
                  EnumToString(TYPE_ENCODING_MAP, sigs[n_sig - i - 1], logbuf, sizeof(logbuf)));
    if(polyflag || (polyflag = !values[i])) // polymorphic value
      v.push_back(llvm::Constant::getNullValue(GetLLVMType(sigs[n_sig - i - 1])));
    else if(!CheckType(sigs[n_sig - i - 1], values[i]->getType()))
      return _logtype("%s: %s cannot match with ", ERR_INVALID_TYPE, values[i]->getType(),
                          EnumToString(TYPE_ENCODING_MAP, sigs[n_sig - i - 1], logbuf, sizeof(logbuf)));
    else
      v.push_back(values[i]);
  }

  // Now we have to actually pop values off the stack
  if(!peek)
    for(varuint32 i = 0; i < n_sig; ++i)
    {
      if(!values.Peek())
        break;
      values.Pop();
    }

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

IN_ERROR Compiler::PushResult(BlockResult** root, llvm::SmallVector<llvmVal*, 2>& result, BB* block, const Environment& env)
{
  BlockResult* next = *root;
  *root             = tmalloc<BlockResult>(env, 1);
  if(!*root)
    return _log("%s: ran out of memory in %s()", ERR_FATAL_OUT_OF_MEMORY, __func__);

  new(*root) BlockResult{ result, block, next };
  return ERR_SUCCESS;
}

IN_ERROR Compiler::PushStructReturn(llvmVal* arg, varsint64 sig)
{
  varuint32 n_sig = GetBlockSigResults(sig, m);
  if(!n_sig)
    return _log("%s: Tried to push a struct but signature had no return values.", ERR_INVALID_VALUE_STACK);
  if(n_sig == 1)
    return PushReturn(arg);

  auto ty = arg->getType();
  if(!ty->isStructTy() || sig >= m.type.n_functypes)
    return _logtype("%s: Expected struct ", ERR_INVALID_FUNCTION_SIG, ty);
  if(ty->getStructNumElements() != n_sig)
    return _log("%s: Struct had %u elements, but signature has %u returns.", ERR_INVALID_BLOCK_SIGNATURE,
                ty->getStructNumElements(), n_sig);

  auto sigs = m.type.functypes[sig].returns;

  // Push struct members in reverse order
  for(varuint32 i = n_sig; i-- > 0;)
  {
    llvmVal* val = builder.CreateExtractValue(arg, { i });

    if(sigs[i] == TE_cref && val->getType()->isIntegerTy())
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
IN_ERROR Compiler::AddBranch(Block& target, bool loop)
{
  IN_ERROR err    = ERR_SUCCESS;
  varuint32 n_sig = loop ? GetBlockSigParams(target.sig, m) : GetBlockSigResults(target.sig, m);
  if(n_sig > 0)
  {
    llvm::SmallVector<llvmVal*, 2> value;
    err = PopSig(target.sig, value, true, loop);
    if(!err && value.size() > 0)
    {
      if(!target.n_phi)
        err = PushResult(&target.results, value, builder.GetInsertBlock(), env); // Push result
      else
      {
        if(target.n_phi != value.size())
          return _log("%s: Target expected %u returns, but value stack only has %u.", ERR_INVALID_VALUE_STACK, target.n_phi,
                      value.size());
        for(size_t i = 0; i < target.n_phi; ++i)
          target.phi[i]->addIncoming(value[i], builder.GetInsertBlock());
      }
    }
  }
  return err;
}

// Pops a label off the control stack, verifying that the value stack matches the signature and building PHI nodes as
// necessary
IN_ERROR Compiler::PopLabel(BB* block, llvm::SmallVector<llvmVal*, 2>& push)
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

      llvm::SmallVector<llvm::PHINode*, 2> phi;
      for(size_t i = 0; i < push.size(); ++i)
      {
        phi.push_back(builder.CreatePHI(push[i]->getType(), count, "phi" + std::to_string(i)));
        phi.back()->addIncoming(push[i], block); // Pop this branches values off value stack, add using proper insert block

        for(auto p = control.Peek().results; p != nullptr; p = p->next)
        {
          assert(i < p->v.size());
          phi.back()->addIncoming(p->v[i], p->b);
        }

        push[i] = phi.back(); // Replace the value with the phi node
      }
    }
  }
  else if(control.Peek().results != nullptr)
    return _log("%s: Signature has 0 results but control stack results aren't empty.", ERR_INVALID_VALUE_STACK);

  if(values.Size() > 0 && !values.Peek()) // Pop at most 1 polymorphic type off the stack.
    values.Pop();
  if(values.Size() > 0) // value stack should be completely empty now
    return _log("%s: Value stack should be empty but has %u values.", ERR_INVALID_VALUE_STACK, values.Size());

  PushReturns(push);

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

IN_ERROR Compiler::InsertBoundsCheck(llvm::Type* inputTy, llvmVal* end, std::initializer_list<llvmVal*> offsets,
                                     llvmVal* accessSize, llvmVal*& loc)
{
  auto sizet               = builder.getIntNTy(machine->getPointerSizeInBits(0));
  bool bypass              = sizet->getBitWidth() > inputTy->getPrimitiveSizeInBits();
  Func* uadd_with_overflow = llvm::Intrinsic::getDeclaration(mod, llvm::Intrinsic::uadd_with_overflow, { sizet });

  end = builder.CreateZExtOrTrunc(end, sizet);
  loc = nullptr;
  llvmVal* upper;
  llvmVal* cond = builder.getInt1(false);

  auto appendVal = [&](llvmVal*& acc, llvmVal* val) {
    val = builder.CreateZExtOrTrunc(val, sizet);
    if(!acc)
      acc = val;
    else if(bypass)
      acc = builder.CreateAdd(acc, val, "", true, true);
    else
    {
      auto v = builder.CreateCall(uadd_with_overflow, { acc, val });
      acc    = builder.CreateExtractValue(v, 0);
      cond   = builder.CreateOr(cond, builder.CreateExtractValue(v, 1));
    }
  };

  for(auto& val : offsets)
  {
    appendVal(loc, val);
  }
  appendVal(upper = loc, accessSize);

  cond = builder.CreateOr(cond, builder.CreateICmpUGT(upper, end), "bounds_check_cond");
  InsertConditionalTrap(cond);

  return ERR_SUCCESS;
}

llvmVal* Compiler::GetMemPointer(llvmVal* base, llvm::PointerType* pointer_type, varuint32 memory, varuint32 offset)
{
  llvm::IntegerType* ty = machine->getPointerSizeInBits(0) == 32 ? builder.getInt32Ty() : builder.getInt64Ty();
  llvmVal* elemSize     = CInt::get(ty, pointer_type->getPointerElementType()->getPrimitiveSizeInBits() / 8, false);

  return GetMemPointerRegion(base, pointer_type, elemSize, memory, offset);
}

llvmVal* Compiler::GetMemPointerRegion(llvmVal* base, llvm::PointerType* pointer_type, llvmVal* byteLength,
                                       varuint32 memory, varuint32 offset)
{
  assert(memories.size() > 0);
  llvmVal* src = !memory ? memlocal : nullptr;
  if(!src)
    src = static_cast<llvmVal*>(GetPairPtr(memories[memory], 0));
  llvm::IntegerType* ty = machine->getPointerSizeInBits(0) == 32 ? builder.getInt32Ty() : builder.getInt64Ty();

  // If our native integer size is larger than the webassembly memory pointer size, then overflow is not possible and we
  // can bypass the check.
  bool bypass = ty->getBitWidth() > base->getType()->getIntegerBitWidth() && false;
  base        = builder.CreateZExtOrTrunc(base, ty);

  llvmVal* loc;
  if(env.flags & ENV_CHECK_MEMORY_ACCESS) // In strict mode, generate a check that traps if this is an invalid memory access
  {
    llvmVal* end = GetMemSize(memories[memory]);
    InsertBoundsCheck(base->getType(), end, { base, CInt::get(ty, offset) }, byteLength, loc);
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
  (*env.loghook)(&env, "values: [");

  size_t total = values.Size() + values.Limit();
  for(size_t i = 0; i < total; ++i)
  {
    if(i == values.Limit())
      (*env.loghook)(&env, " |");
    if(!values[i])
      (*env.loghook)(&env, " Poly");
    else
      switch(GetTypeEncoding(values[i]->getType()))
      {
      case TE_i32: (*env.loghook)(&env, " i32"); break;
      case TE_i64: (*env.loghook)(&env, " i64"); break;
      case TE_f32: (*env.loghook)(&env, " f32"); break;
      case TE_f64: (*env.loghook)(&env, " f64"); break;
      }
  }

  (*env.loghook)(&env, " ]\n");
  (*env.loghook)(&env, "control: [");

  for(size_t i = 0; i < control.Size(); ++i)
  {
    switch(control[i].sig)
    {
    case TE_i32: (*env.loghook)(&env, " i32"); break;
    case TE_i64: (*env.loghook)(&env, " i64"); break;
    case TE_f32: (*env.loghook)(&env, " f32"); break;
    case TE_f64: (*env.loghook)(&env, " f64"); break;
    }

    (*env.loghook)(&env, ":%i", (int)control[i].op);
  }

  (*env.loghook)(&env, " ]\n\n");
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
                                             llvm::StringRef canonical, size_t line, llvm::Constant* init = 0)
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
                              Func* (Compiler::*wrapper)(Func* fn, llvm::StringRef name, llvm::StringRef canonical,
                                                         llvm::GlobalValue::LinkageTypes linkage,
                                                         llvm::CallingConv::ID callconv),
                              llvm::StringRef name, llvm::StringRef canonical)
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

  if(env.flags & ENV_COUNT_INSTRUCTIONS)
  {
    instruction_counter = new llvm::GlobalVariable(*mod, builder.getInt64Ty(), false, Func::CommonLinkage,
                                                   builder.getInt64(0), IN_INSTRUCTION_COUNTER);
    instruction_counter->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
  }

  debugger.reset(Debugger::Create(*this));
  if(!debugger)
    return _log("%s: Failed to create Debugger in %s()", ERR_FATAL_DEBUG_OBJ_ERROR, __func__);

  // Define a unique init function for performing module initialization
  init =
    TopLevelFunction(ctx, builder, CanonicalName(StringSpan::From(m.name), StringSpan::From(IN_INIT_POSTFIX)).c_str(), mod);

#ifdef IN_PLATFORM_WIN32
  // Insert the flag initialization into module 0 on win32
  if(m_idx == 0)
  {
    builder.CreateCall(Func::Create(FuncTy::get(builder.getVoidTy(), { builder.getInt32Ty() }, false),
                                    Func::ExternalLinkage, "_innative_internal_env_init_isa_flags", *mod),
                       { builder.getInt32(machine->getTargetFeatureString().contains("+sse2") ? 1 : 0) });
  }
#endif

  // Declare C runtime function prototypes that we assume exist on the system
  fn_memgrow = Func::Create(FuncTy::get(builder.getInt8PtrTy(0),
                                     { builder.getInt8PtrTy(0), builder.getInt64Ty(), builder.getInt64Ty(),
                                       builder.getInt64Ty()->getPointerTo() },
                                     false),
                         Func::ExternalLinkage, "_innative_internal_env_grow_memory", mod);
  fn_memgrow->setReturnDoesNotAlias(); // This is a system memory allocation function, so the return value does not alias

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

  fn_memcpy = Func::Create(FuncTy::get(builder.getInt8PtrTy(),
                                    { builder.getInt8PtrTy(0), builder.getInt8PtrTy(0), builder.getInt64Ty() }, false),
                        Func::ExternalLinkage, "_innative_internal_env_memcpy", mod);

  fn_memmove = Func::Create(FuncTy::get(builder.getInt8PtrTy(),
                                     { builder.getInt8PtrTy(), builder.getInt8PtrTy(),
                                       builder.getIntNTy(machine->getPointerSizeInBits(0)) },
                                     false),
                         Func::ExternalLinkage, "_innative_internal_env_memmove", mod);

  fn_memset = Func::Create(FuncTy::get(builder.getInt8PtrTy(),
                                    { builder.getInt8PtrTy(), builder.getInt32Ty(),
                                      builder.getIntNTy(machine->getPointerSizeInBits(0)) },
                                    false),
                        Func::ExternalLinkage, "_innative_internal_env_memset", mod);

  fn_memcmp = Func::Create(FuncTy::get(builder.getInt32Ty(),
                                    { builder.getInt8PtrTy(), builder.getInt32Ty(),
                                      builder.getIntNTy(machine->getPointerSizeInBits(0)) },
                                    false),
                        Func::ExternalLinkage, "_innative_internal_env_memcmp", mod);

  Func* fn_memfree =
    Func::Create(FuncTy::get(builder.getVoidTy(), { builder.getInt8PtrTy(0), builder.getInt64Ty() }, false),
                 Func::ExternalLinkage, "_innative_internal_env_free_memory", mod);

  debugger->FunctionDebugInfo(init, IN_INIT_POSTFIX + (DIVIDER + std::string(m.name.str())), true, true, nullptr,
                              debugger->GetSourceFile(0), 0, 0);

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
        return _log("%s: There are only %u functypes, but type index was %u", ERR_INVALID_TYPE_INDEX, m.type.n_functypes,
                    index);

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
      // Assume all external functions invalidate the memory cache
      functions.back().internal->setMetadata(IN_MEMORY_GROW_METADATA, llvm::MDNode::get(ctx, {}));

      auto e = ResolveExport(env, imp);
      if(!e.second)
      {
        functions.back().imported = functions.back().internal;
        functions.back().imported->setLinkage(Func::ExternalLinkage);
        functions.back().imported->setCallingConv(GetCallingConvention(imp));

        auto& debugname = imp.func_desc.debug.name;
        std::string canonical  = !(debugname.get()) ? functions.back().imported->getName().str() + DIVIDER "internal" :
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
    return _log("%s: n_funcdecl (%u) does not match n_funcbody (%u)", ERR_INVALID_FUNCTION_BODY, m.function.n_funcdecl,
                m.code.n_funcbody);
  size_t code_index = functions.size();

  // Declare function prototypes
  for(varuint32 i = 0; i < m.function.n_funcdecl; ++i)
  {
    auto decl = m.function.funcdecl[i];
    if(decl.type_index >= m.type.n_functypes)
      return _log("%s: There are %u types, but type_index was %u in function %u", ERR_INVALID_TYPE_INDEX,
                  m.type.n_functypes, decl.type_index, i);

    functions.emplace_back();
    functions.back().internal = CompileFunction(m.type.functypes[decl.type_index],
                                                std::string(!decl.debug.name.size() ? "func" : decl.debug.name.str()) +
                                                  "_" + std::to_string(functions.size()) + DIVIDER + m.name.str());

    debugger->FuncDecl(functions.back().internal, m.code.funcbody[i].column, decl.debug.line);
    std::string name      = functions.back().internal->getName().str() + DIVIDER "external";
    auto wrapperfn = &Compiler::PassFunction;
    ExportFunction(functions.back(), wrapperfn, name, name);
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
      return _logtype("%s: Got bytewidth of 0 for ", ERR_INVALID_TABLE_TYPE, type);

    CallInst* call =
      builder.CreateCall(fn_memgrow, { llvm::ConstantPointerNull::get(builder.getInt8PtrTy(0)),
                                    builder.getInt64(m.table.tables[i].resizable.minimum * bytewidth),
                                    builder.getInt64((m.table.tables[i].resizable.flags & WASM_LIMIT_HAS_MAXIMUM) ?
                                                       (m.table.tables[i].resizable.maximum * bytewidth) :
                                                       0),
                                    GetPairPtr(tables.back(), 1) });

    call->setCallingConv(fn_memgrow->getCallingConv());

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
      builder.CreateCall(fn_memgrow, { llvm::ConstantPointerNull::get(type), sz, max, GetPairPtr(memories.back(), 1) });
    call->setCallingConv(fn_memgrow->getCallingConv());
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

  // Process element section by appending to the init function
  for(varuint32 i = 0; i < m.element.n_elements; ++i)
  {
    TableInit& e      = m.element.elements[i];
    auto element_type = (e.flags & WASM_ELEM_CARRIES_ELEMEXPRS) ? e.elem_type : TE_funcref;

    // Create a constant array of the data
    auto target    = GetLLVMType(element_type);
    auto null_val  = llvm::Constant::getNullValue(target);
    auto no_ty_val = builder.getInt32(0);
    auto elem_ty   = GetTableType(element_type);
    auto array_ty  = llvm::ArrayType::get(elem_ty, e.n_elements);
    std::vector<llvm::Constant*> elem_values;
    elem_values.reserve(e.n_elements);
    for(varuint32 j = 0; j < e.n_elements; ++j)
    {
      llvm::Optional<varuint32> elem = llvm::None;
      if(e.flags & WASM_ELEM_CARRIES_ELEMEXPRS)
      {
        if(e.elemexprs[j].opcode[0] == OP_ref_func)
          elem = e.elemexprs[j].immediates[0]._varuint32;
      }
      else
        elem = e.elements[j];

      llvm::Constant* value = null_val;
      llvm::Constant* type  = no_ty_val;
      if(elem.hasValue() && element_type == TE_funcref)
      {
        varuint32 index = GetFirstType(ModuleFunctionType(m, elem.getValue()));
        if(index == ~0u)
          return _log("%s: Failed to hash functione type", ERR_INVALID_FUNCTION_INDEX);

        value = llvm::ConstantExpr::getPointerCast(functions[elem.getValue()].internal, target);
        type  = builder.getInt32(index);
      }

      auto elem_val = llvm::ConstantStruct::get(elem_ty, value, type);
      elem_values.push_back(elem_val);
    }
    auto elem   = llvm::ConstantArray::get(array_ty, elem_values);
    auto global = new llvm::GlobalVariable(*mod, array_ty, true, llvm::GlobalValue::LinkageTypes::PrivateLinkage, elem,
                                           CanonicalName(StringSpan{ 0, 0 }, StringSpan::From("elem"), i));
    auto lenvar = new llvm::GlobalVariable(*mod, builder.getInt64Ty(), false,
                                           llvm::GlobalValue::LinkageTypes::PrivateLinkage,
                                           builder.getInt64(elem_values.size()),
                                           CanonicalName(StringSpan{ 0, 0 }, StringSpan::From("elem_len"), i));
    debugger->DebugGlobal(global, global->getName(), 0);
    debugger->DebugGlobal(lenvar, lenvar->getName(), 0);
    elem_globals.push_back({ elem, global, lenvar });

    if(!(e.flags & WASM_ELEM_PASSIVE))
    {
      llvm::Constant* offset;
      if(err = CompileInitConstant(e.offset, m, offset))
        return err;

      values.Push(offset);
      values.Push(builder.getInt32(0));
      values.Push(builder.getInt32(e.n_elements));
      CompileTableInit(e.index, i);
      CompileElemDrop(i);
    }
  }

  // Process data section by appending to the init function
  for(varuint32 i = 0; i < m.data.n_data; ++i)
  {
    DataInit& d = m.data.data[i]; // First we declare a constant array that stores the data in the EXE
    auto data = llvm::ConstantDataArray::get(ctx, llvm::makeArrayRef<uint8_t>(d.data.get(), d.data.get() + d.data.size()));
    auto val  = new llvm::GlobalVariable(*mod, data->getType(), true, llvm::GlobalValue::LinkageTypes::PrivateLinkage, data,
                                        CanonicalName(StringSpan{ 0, 0 }, StringSpan::From("data"), i));
    auto lenvar = new llvm::GlobalVariable(*mod, builder.getInt64Ty(), false,
                                           llvm::GlobalValue::LinkageTypes::PrivateLinkage, builder.getInt64(d.data.size()),
                                           CanonicalName(StringSpan{ 0, 0 }, StringSpan::From("data_len"), i));
    debugger->DebugGlobal(val, val->getName(), 0);
    debugger->DebugGlobal(lenvar, lenvar->getName(), 0);
    data_globals.push_back({ data, val, lenvar });

    // Only perform the init now if active
    if(!(d.flags & WASM_DATA_PASSIVE))
    {
      llvm::Constant* offset;
      if(err = CompileInitConstant(d.offset, m, offset))
        return err;

      // We have to actually check the bounds for this with a trap
      // now so just reuse the instruction code
      values.Push(offset);                                          // memory offset
      values.Push(builder.getInt32(0));                             // data offset
      values.Push(builder.getInt32(GetTotalSize(data->getType()))); // size
      CompileMemInit(d.index, i);
      CompileDataDrop(i);
    }
  }

  // Terminate init function
  builder.CreateRetVoid();

  // Create cleanup function
  exit =
    TopLevelFunction(ctx, builder, CanonicalName(StringSpan::From(m.name), StringSpan::From(IN_EXIT_POSTFIX)).c_str(), mod);

  debugger->FunctionDebugInfo(exit, IN_EXIT_POSTFIX + (DIVIDER + std::string(m.name.str())), true, true, nullptr,
                              debugger->GetSourceFile(0), 0, 0);
  debugger->SetSPLocation(builder, exit->getSubprogram());

  // Don't accidentally delete imported linear memories
  for(size_t i = m.importsection.memories - m.importsection.tables; i < memories.size(); ++i)
    builder
      .CreateCall(fn_memfree,
                  { builder.CreateLoad(GetPairPtr(memories[i], 0)), builder.CreateLoad(GetPairPtr(memories[i], 1)) })
      ->setCallingConv(fn_memfree->getCallingConv());

  // Don't accidentally delete imported tables
  for(size_t i = m.importsection.tables - m.importsection.functions; i < tables.size(); ++i)
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
      current = fn;
      if((err = CompileFunctionBody(fn, code_index, functions[code_index].memlocal, m.function.funcdecl[i],
                                    m.code.funcbody[i])) < 0)
      {
        current = nullptr;
        return err;
      }
      current = nullptr;
    }
    ++code_index;
  }

  // If the start section exists, lift the start function to the context so our environment knows about it.
  if(m.knownsections & (1 << WASM_SECTION_START))
  {
    if(m.start >= functions.size())
      return _log("%s: There are only %zu functions, but start index was %u", ERR_INVALID_START_FUNCTION, functions.size(),
                  m.start);
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

  // Because it's crucial we cover the entire call graph, we go through all functions with definitions
  for(auto fn : functions)
  {
    PostOrderTraversal(fn.internal);
    PostOrderTraversal(fn.exported);
    PostOrderTraversal(fn.imported);
  }

  for(auto fn : functions)
  {
    // This cleanup is optional, but keeps the IR clean when inspecting it.
    if(fn.internal)
      fn.internal->setMetadata(IN_FUNCTION_TRAVERSED, 0);
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
              // Setting the insert point doesn't actually gaurantee the instructions come after the call
              builder.SetInsertPoint(&i);
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
    {
      auto name = std::string(e->name.str()) + DIVIDER + m->name.str();
      auto& fn  = compiler->functions[e->index];

      if(!fn.exported)
      {
        compiler->ExportFunction(fn, wrapperfn, name, canonical);
        fn.exported->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
      }
#ifdef IN_PLATFORM_WIN32
      else if(env->jit) // GlobalAlias does the wrong thing on Windows JIT
        compiler->WrapFunction(fn.exported, name, canonical, Func::ExternalLinkage, fn.exported->getCallingConv())
          ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
#endif
      else
        llvm::GlobalAlias::create(llvm::GlobalValue::ExternalLinkage, canonical, fn.exported)
          ->setDLLStorageClass(llvm::GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);

      if(env->flags & ENV_GENERALIZE_FUNCTIONS)
        compiler->GenericFunction(fn.imported ? fn.imported : fn.internal, name + IN_GENERIC_POSTFIX,
                                  canonical + IN_GENERIC_POSTFIX, Func::ExternalLinkage, llvm::CallingConv::C);
    }
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

void AddRTLibCalls(Environment* env, llvm::IRBuilder<>& builder, Compiler& mainctx, bool isJIT)
{
#ifdef IN_PLATFORM_WIN32
  if(isJIT) // The JIT on windows will just fail if we don't define this symbol here. It seems to have magic properties.
    new llvm::GlobalVariable(*mainctx.mod, builder.getInt8Ty(), true, Func::WeakAnyLinkage, builder.getInt8(0),
                             "__ImageBase");

  // Create __chkstk function to satisfy the rtlibcalls
  auto chkstk_ms =
    Func::Create(FuncTy::get(builder.getVoidTy(), {}, false), Func::ExternalLinkage, "__chkstk_ms", mainctx.mod);
  chkstk_ms->addFnAttr(llvm::Attribute::Naked);

  Func* chkstk = Func::Create(chkstk_ms->getFunctionType(), Func::WeakAnyLinkage, "__chkstk", mainctx.mod);
  builder.SetInsertPoint(BB::Create(*env->context, "entry", chkstk));
  mainctx.debugger->FunctionDebugInfo(chkstk, "chkstk", true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0, 0);
  mainctx.debugger->SetSPLocation(builder, chkstk->getSubprogram());
  builder.CreateCall(chkstk_ms, {});
  builder.CreateRetVoid();
#endif

  // Create memcpy function to satisfy the rtlibcalls
  Func* fn_memcpy = Func::Create(mainctx.fn_memcpy->getFunctionType(), Func::WeakAnyLinkage, "memcpy", mainctx.mod);
  builder.SetInsertPoint(BB::Create(*env->context, "entry", fn_memcpy));
  mainctx.debugger->FunctionDebugInfo(fn_memcpy, "memcpy", true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0, 0);
  mainctx.debugger->SetSPLocation(builder, fn_memcpy->getSubprogram());
  CallInst* inner_memcpy =
    builder.CreateCall(mainctx.fn_memcpy, { fn_memcpy->getArg(0), fn_memcpy->getArg(1), fn_memcpy->getArg(2) });
  builder.CreateRet(inner_memcpy);

  // Create memmove function to satisfy the rtlibcalls
  Func* fn_memmove = Func::Create(mainctx.fn_memmove->getFunctionType(), Func::WeakAnyLinkage, "memmove", mainctx.mod);
  builder.SetInsertPoint(BB::Create(*env->context, "entry", fn_memmove));
  mainctx.debugger->FunctionDebugInfo(fn_memmove, "memmove", true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0, 0);
  mainctx.debugger->SetSPLocation(builder, fn_memmove->getSubprogram());
  CallInst* inner_memmove =
    builder.CreateCall(mainctx.fn_memmove, { fn_memmove->getArg(0), fn_memmove->getArg(1), fn_memmove->getArg(2) });
  builder.CreateRet(inner_memmove);

  // Create memset function to satisfy the rtlibcalls
  Func* fn_memset =
    Func::Create(mainctx.fn_memset->getFunctionType(), Func::LinkageTypes::WeakAnyLinkage, "memset", mainctx.mod);
  builder.SetInsertPoint(BB::Create(*env->context, "entry", fn_memset));
  mainctx.debugger->FunctionDebugInfo(fn_memset, "memset", true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0, 0);
  mainctx.debugger->SetSPLocation(builder, fn_memset->getSubprogram());
  CallInst* inner_memset =
    builder.CreateCall(mainctx.fn_memset, { fn_memset->getArg(0), fn_memset->getArg(1), fn_memset->getArg(2) });
  builder.CreateRet(inner_memset);

  // Create memcmp function to satisfy the rtlibcalls
  Func* fn_memcmp =
    Func::Create(mainctx.fn_memcmp->getFunctionType(), Func::LinkageTypes::WeakAnyLinkage, "memcmp", mainctx.mod);
  builder.SetInsertPoint(BB::Create(*env->context, "entry", fn_memcmp));
  mainctx.debugger->FunctionDebugInfo(fn_memcmp, "memcmp", true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0, 0);
  mainctx.debugger->SetSPLocation(builder, fn_memcmp->getSubprogram());
  CallInst* inner_memcmp =
    builder.CreateCall(mainctx.fn_memcmp, { fn_memcmp->getArg(0), fn_memcmp->getArg(1), fn_memcmp->getArg(2) });
  builder.CreateRet(inner_memcmp);
}

IN_ERROR innative::CompileEnvironment(Environment* env, const char* outfile)
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
    return LogErrorString(*env, "%s: %s does not exist - %s", ERR_FATAL_UNKNOWN_TARGET, triple.c_str(), llvm_err.c_str());
  }

  for(varuint32 i = 0; i < env->n_modules; ++i)
    if(env->modules[i].knownsections & (1 << WASM_SECTION_START))
      has_start = true;

  if((!has_start || env->flags & ENV_NO_INIT) && !(env->flags & ENV_LIBRARY))
  {
    env->flags |= ENV_LIBRARY; // Attempting to compile a library as an EXE is a common error, so we fix it for you.
    (*env->loghook)(
      env,
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
        new Compiler(*env,    env->modules[i], *env->context,        0,
                      builder, machine,         kh_init_importhash(), GetLinkerObjectPath(*env, env->modules[i], file));
      if(!env->modules[i].cache->objfile.empty())
        remove(env->modules[i].cache->objfile);

      if((err = env->modules[i].cache->CompileModule(i)) < 0)
        return err;
      new_modules.push_back(env->modules + i);
    }
    has_start |= env->modules[i].cache->start != nullptr;
  }

  if((!has_start || env->flags & ENV_NO_INIT) && !(env->flags & ENV_LIBRARY))
    return LogErrorString(*env, "%s: Can't compile an executable without a start function in at least one module.",
                          ERR_INVALID_START_FUNCTION);

  for(auto m : new_modules)
    Compiler::ResolveModuleExports(env, m, *env->context);

  for(auto m : new_modules)
    m->cache->AddMemLocalCaching();

  Compiler& mainctx = *env->modules[0].cache;
  AddRTLibCalls(env, builder, mainctx, false);

  // Create cleanup function
  Func* cleanup = Compiler::TopLevelFunction(*env->context, builder, IN_EXIT_FUNCTION, mainctx.mod);
  mainctx.debugger->FunctionDebugInfo(cleanup, IN_EXIT_FUNCTION, true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0,
                                      0);
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
  mainctx.debugger->FunctionDebugInfo(main, IN_INIT_FUNCTION, true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0,
                                      0);
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
        else // Create function prototype in main module
          stub = Func::Create(env->modules[i].cache->start->getFunctionType(), env->modules[i].cache->start->getLinkage(),
                              env->modules[i].cache->start->getName(), mainctx.mod);
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
    mainctx.debugger->FuncDecl(mainstub, 0, mainctx.m.start_line);
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
      return LogErrorLLVM(*env, "%s: ", ERR_JIT_TARGET_MACHINE_FAILURE, JTMB.takeError());

    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if(!DL)
      return LogErrorLLVM(*env, "%s: ", ERR_JIT_DATA_LAYOUT_ERROR, DL.takeError());

    // If the context already exists, we take ownership of it
    env->jit = new JITContext(std::move(*JTMB), std::move(*DL), std::unique_ptr<llvm::LLVMContext>(env->context), env);
    // env->context = nullptr;

    if(expose_process)
      if(env->jit->CompileEmbedding(nullptr))
        return LogErrorString(*env, "%s: Failed to expose current executable to JIT", ERR_JIT_LINK_PROCESS_FAILURE);

    for(Embedding* embed = env->embeddings; embed != nullptr; embed = embed->next)
      if(env->jit->CompileEmbedding(embed))
        return LogErrorString(*env, "%s: Failed to link %s (%i) to JIT", ERR_JIT_LINK_FAILURE, embed->name, embed->tag);
  }

  auto machine = env->jit->GetTargetMachine();
  if(!machine)
    return LogErrorLLVM(*env, "%s: ", ERR_JIT_TARGET_MACHINE_FAILURE, machine.takeError());

  llvm::IRBuilder<> builder(*env->context);
  IN_ERROR err = ERR_SUCCESS;

  if(!env->n_modules)
    return ERR_FATAL_NO_MODULES;

  switch(env->optimize & ENV_OPTIMIZE_OMASK)
  {
  case ENV_OPTIMIZE_O0: machine->get()->setOptLevel(llvm::CodeGenOpt::None); break;
  case ENV_OPTIMIZE_O1: machine->get()->setOptLevel(llvm::CodeGenOpt::Less); break;
  case ENV_OPTIMIZE_Os:
  case ENV_OPTIMIZE_O2: machine->get()->setOptLevel(llvm::CodeGenOpt::Default); break;
  case ENV_OPTIMIZE_O3: machine->get()->setOptLevel(llvm::CodeGenOpt::Aggressive); break;
  default: assert(false);
  }

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
      env->modules[i].cache = new Compiler(*env, env->modules[i], env->jit->GetContext(), 0, builder, machine.get().get(),
                                           kh_init_importhash(), path());

      if((err = env->modules[i].cache->CompileModule(i)) < 0)
        return err;

      new_modules.push_back(env->modules + i);
    }
  }

  for(auto m : new_modules)
    Compiler::ResolveModuleExports(env, m, *env->context);

  for(auto m : new_modules)
    m->cache->AddMemLocalCaching();

  Compiler& mainctx = *env->modules[0].cache;
  AddRTLibCalls(env, builder, mainctx, true);

  // Create cleanup function, but only if the user would have to manually call it
  if(env->flags & ENV_NO_INIT)
  {
    Func* cleanup = Compiler::TopLevelFunction(*env->context, builder, IN_EXIT_FUNCTION, mainctx.mod);
    mainctx.debugger->FunctionDebugInfo(cleanup, IN_EXIT_FUNCTION, true, true, nullptr, mainctx.debugger->GetSourceFile(0),
                                        0, 0);
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
    mainctx.debugger->FunctionDebugInfo(main, IN_INIT_FUNCTION, true, true, nullptr, mainctx.debugger->GetSourceFile(0), 0,
                                        0);
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
    if(auto e = env->jit->CompileModule(std::unique_ptr<llvm::Module>(env->modules[i].cache->mod)))
      return LogErrorLLVM(*env, "%s: failed to compile %s: ", ERR_JIT_COMPILE_FAILURE, std::move(e),
                          env->modules[i].name.str());
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
    {
      if(auto sym = env->jit->Lookup(n))
      {
        auto addr = sym.get().getAddress();
        reinterpret_cast<IN_Entrypoint>(addr)();
      }
      else
      {
        auto err = sym.takeError();
        std::string errMsg;
        llvm::raw_string_ostream buf{ errMsg };
        buf << err;
        auto str = buf.str();
        (*env->loghook)(env, "Error loading JIT module entry point: %s\n", str.c_str());
      }
    }
  }

  return ERR_SUCCESS;
}
