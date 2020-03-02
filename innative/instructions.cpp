// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "compile.h"
#include "utility.h"
#include "validate.h"

using namespace innative;
using namespace utility;
using Func    = llvm::Function;
using FuncTy  = llvm::FunctionType;
using llvmTy  = llvm::Type;
using llvmVal = llvm::Value;
using BB      = llvm::BasicBlock;
using llvm::CallInst;
using llvm::ConstantFP;

Func* Compiler::CompileFunction(FunctionType& signature, const llvm::Twine& name)
{
  Func* fn = Func::Create(GetFunctionType(signature),
                          ((env.flags & ENV_DEBUG) ? Func::ExternalLinkage : Func::InternalLinkage), name, mod);
  fn->setCallingConv(InternalConvention);
  return fn;
}

// Given a pointer to the appropriate builder function, pops two binary arguments off the stack and pushes the result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IN_ERROR Compiler::CompileBinaryOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  return PushReturn((builder.*op)(val1, val2, args...));
}

// Given an intrinsic function ID, pops two binary arguments off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IN_ERROR Compiler::CompileBinaryIntrinsic(llvm::Intrinsic::ID id, const llvm::Twine& name)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  return PushReturn(builder.CreateBinaryIntrinsic(id, val1, val2, nullptr, name));
}

// Given a function pointer to the appropriate builder function, pops one unary argument off the stack and pushes the result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
IN_ERROR Compiler::CompileUnaryOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, Args...), Args... args)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal* val1;
  if(err = PopType(Ty1, val1))
    return err;

  return PushReturn((builder.*op)(val1, args...));
}

// Given an intrinsic function ID, pops one unary argument off the stack and pushes the result, converting it to a Value*
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING TyR, typename... Args>
IN_ERROR Compiler::CompileUnaryIntrinsic(llvm::Intrinsic::ID id, const llvm::Twine& name, Args... args)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal* val1;
  if(err = PopType(Ty1, val1))
    return err;

  llvmVal* values[sizeof...(Args) + 1] = { val1, args... };

  Func* fn = llvm::Intrinsic::getDeclaration(mod, id, { val1->getType() });

  return PushReturn(builder.CreateCall(fn, values, name));
}

IN_ERROR Compiler::CompileSelectOp(const llvm::Twine& name, llvm::Instruction* from)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *cond, *valf, *valt;
  if(err = PopType(TE_i32, cond))
    return err;

  if(!values.Size())
    return ERR_INVALID_VALUE_STACK;
  if(!values.Peek()) // If a polymorphic type is on the stack, the result is a polymorphic type, so just do nothing
    return ERR_SUCCESS;

  valf = values.Pop();

  if(err = PopType(GetTypeEncoding(valf->getType()), valt))
    return err;

  return PushReturn(builder.CreateSelect(builder.CreateICmpNE(cond, builder.getInt32(0)), valt, valf, name, from));
}

template<WASM_TYPE_ENCODING TYPE, bool LEFT> IN_ERROR Compiler::CompileRotationOp(const char* name)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *count, *value;
  if(err = PopType(TYPE, count))
    return err;
  if(err = PopType(TYPE, value))
    return err;

  const int BITS = (TYPE == TE_i32) ? 32 : 64;
  llvmVal *l, *r;
  count        = MaskShiftBits(count);
  llvmVal* sub = builder.CreateSub(CInt::get(ctx, llvm::APInt(BITS, BITS, true)), count);

  if(LEFT) // value<<count | value>>(BITS-count);
  {
    l = builder.CreateShl(value, count);
    r = builder.CreateLShr(value, sub);
  }
  else // value>>count | value<<(BITS-count);
  {
    l = builder.CreateLShr(value, count);
    r = builder.CreateShl(value, sub);
  }

  return PushReturn(builder.CreateOr(l, r, name));
}

// BinaryOp for shift functions which has to apply a shift mask to one operand
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IN_ERROR Compiler::CompileBinaryShiftOp(llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  return PushReturn((builder.*op)(val1, MaskShiftBits(val2), args...));
}


IN_ERROR Compiler::CompileIfBlock(varsint7 sig)
{
  IN_ERROR err;
  llvmVal* cond;

  if(err = PopType(TE_i32, cond))
    return err;

  llvmVal* cmp = builder.CreateICmpNE(cond, builder.getInt32(0), "if_cond");

  Func* parent          = builder.GetInsertBlock()->getParent();
  BB* tblock            = BB::Create(ctx, "if_true", parent);
  BB* fblock            = BB::Create(ctx, "if_else", parent); // Create else stub
  BB* endblock          = PushLabel("if_end", sig, OP_if, nullptr, debugger->_curscope);
  control.Peek().ifelse = fblock;

  builder.CreateCondBr(cmp, tblock, fblock); // Insert branch in current block
  builder.SetInsertPoint(fblock);            // Point else stub at end block
  builder.CreateBr(endblock);

  builder.SetInsertPoint(tblock); // Start inserting code into true block
  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileElseBlock()
{
  if(control.Size() == 0 || control.Peek().op != OP_if)
    return ERR_IF_ELSE_MISMATCH;

  builder.CreateBr(control.Peek().block); // Add a branch-to-merge instruction to our if_true block

  // Instead of popping and pushing a new control label, we just re-purpose the existing one. This preserves the value
  // stack results.
  if(control.Peek().sig != TE_void)
  {
    IN_ERROR err;
    llvmVal* value;
    if(err = PopType(control.Peek().sig, value))
      return err;
    if(err = PushResult(&control.Peek().results, value, builder.GetInsertBlock(), env)) // Push result
      return err;
  }

  // Reset value stack, but ensure that we preserve a polymorphic value if we had pushed one before
  while(values.Size() > 1)
    values.Pop();
  if(values.Size() > 1 && values.Peek() != nullptr)
    values.Pop();

  control.Peek().op = OP_else;               // make this block an OP_else block
  BB* fblock        = control.Peek().ifelse; // Get stored else block
  fblock->begin()->eraseFromParent();        // Erase stub instruction
  fblock->removeFromParent();                // Required for correct label binding behavior
  BindLabel(fblock);                         // Bind if_false block to current position

  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileReturn(varsint7 sig)
{
  if(sig == TE_void)
    builder.CreateRetVoid();
  else
  {
    llvmVal* val;
    IN_ERROR err = PopType(sig, val);
    if(err)
      return err;

    builder.CreateRet(val);
  }

  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileEndBlock()
{
  BB* cur = builder.GetInsertBlock();
  if(control.Peek().block->getParent() != nullptr) // If the label wasn't bound, branch to the new one we create
  {
    BB* block = BindLabel(control.Peek().block);
    builder.SetInsertPoint(cur);
    builder.CreateBr(block);
    builder.SetInsertPoint(block);
  }
  else
  {
    builder.CreateBr(control.Peek().block);
    BindLabel(control.Peek().block);
  }

  auto cache = control.Peek();
  switch(cache.op) // Verify source operation
  {
  case OP_if:
    if(control.Peek().sig != TE_void) // An if statement with no else statement cannot return a value
      return ERR_EXPECTED_ELSE_INSTRUCTION;
  case OP_else:
  case OP_block:
  case OP_loop:
  case OP_return: break;
  default: return ERR_END_MISMATCH;
  }

  IN_ERROR err = PopLabel(cur); // Pop the label to assemble the phi node before pushing it.
  if(cache.op == OP_return)
    CompileReturn(cache.sig);

  return err;
}

void Compiler::CompileTrap()
{
  auto call = builder.CreateCall(llvm::Intrinsic::getDeclaration(mod, llvm::Intrinsic::trap), {});
  call->setDoesNotReturn();
  builder.CreateUnreachable();
}


IN_ERROR Compiler::CompileBranch(varuint32 depth)
{
  if(depth >= control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  Block& target = control[depth];
  builder.CreateBr(target.block);
  IN_ERROR err =
    (target.op != OP_loop) ?
      AddBranch(target) :
      ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
  PolymorphicStack();
  return err;
}

IN_ERROR Compiler::CompileIfBranch(varuint32 depth)
{
  if(depth >= control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  IN_ERROR err;
  llvmVal* cond;
  if(err = PopType(TE_i32, cond))
    return err;

  llvmVal* cmp = builder.CreateICmpNE(cond, builder.getInt32(0), "br_if_cond");

  // Because llvm requires explicit branches, we have to create a new block and append it to our current one
  BB* block = BB::Create(ctx, "br_if_cont", builder.GetInsertBlock()->getParent());

  Block& target = control[depth];
  builder.CreateCondBr(cmp, target.block, block);
  if(target.op != OP_loop)
  {
    if(err = AddBranch(target))
      return err;
  }
  builder.SetInsertPoint(
    block); // Start inserting code into continuation AFTER we add the branch, so the branch goes to the right place
  return ERR_SUCCESS; // Branches targeting loops just throw all their values away, so we don't need to build PHI nodes.
}
IN_ERROR Compiler::CompileBranchTable(varuint32 n_table, varuint32* table, varuint32 def)
{
  IN_ERROR err;
  llvmVal* index;
  if(err = PopType(TE_i32, index))
    return err;

  if(def >= control.Size())
    return ERR_INVALID_BRANCH_DEPTH;

  llvm::SwitchInst* s = builder.CreateSwitch(index, control[def].block, n_table);
  err                 = (control[def].op != OP_loop) ? AddBranch(control[def]) : ERR_SUCCESS;

  for(varuint32 i = 0; i < n_table && err == ERR_SUCCESS; ++i)
  {
    if(table[i] >= control.Size())
      return ERR_INVALID_BRANCH_DEPTH;

    Block& target = control[table[i]];
    s->addCase(builder.getInt32(i), target.block);
    err = (target.op != OP_loop) ? AddBranch(target) : ERR_SUCCESS;
  }

  PolymorphicStack();
  return err;
}

IN_ERROR Compiler::CompileCall(varuint32 index)
{
  if(index >= functions.size())
    return ERR_INVALID_FUNCTION_INDEX;

  // Check if this is an intrinsic. If it is, we instead call the intrinsic with the parameters and immediately return the
  // value, if it exists
  if(functions[index].intrinsic != nullptr)
  {
    IN_ERROR err;
    int num         = functions[index].intrinsic->num;
    llvmVal** ArgsV = tmalloc<llvmVal*>(env, num);
    if(num > 0 && !ArgsV)
      return ERR_FATAL_OUT_OF_MEMORY;

    for(unsigned int i = num; i-- > 0;)
    {
      if(err = PopType(functions[index].intrinsic->params[i], ArgsV[i]))
        return err;
    }

    llvmVal* out = nullptr;
    err          = (this->*functions[index].intrinsic->fn)(ArgsV, out);
    if(err >= 0 && out != nullptr)
      return PushReturn(out);

    return err;
  }

  // Because this is a static function call, we can call the imported C function directly with the appropriate calling
  // convention.
  Func* fn         = (!functions[index].imported) ? (functions[index].internal) : functions[index].imported;
  unsigned int num = fn->getFunctionType()->getNumParams();

  // Pop arguments in reverse order
  IN_ERROR err;
  llvmVal** ArgsV = tmalloc<llvmVal*>(env, num);
  if(num > 0 && !ArgsV)
    return ERR_FATAL_OUT_OF_MEMORY;

  for(unsigned int i = num; i-- > 0;)
  {
    if(err = PopType(GetTypeEncoding(fn->getFunctionType()->getParamType(i)), ArgsV[i]))
      return err;
  }

  CallInst* call = builder.CreateCall(fn, llvm::makeArrayRef(ArgsV, num));
  if(env.flags & ENV_DISABLE_TAIL_CALL) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(CallInst::TCK_NoTail);
  call->setCallingConv(fn->getCallingConv());
  call->setAttributes(fn->getAttributes());

  if(!fn->getReturnType()->isVoidTy()) // Only push a value if there is one to push
    return PushReturn(call);

  return ERR_SUCCESS;
}

llvmVal* Compiler::GetMemSize(llvm::GlobalVariable* target) { return builder.CreateLoad(GetPairPtr(target, 1)); }

// Gets the first type index that matches the given type, used as a stable type hash
varuint32 Compiler::GetFirstType(varuint32 type)
{
  if(type < m.type.n_functypes)
  {
    for(varuint32 i = 0; i < type; ++i)
      if(MatchFunctionType(m.type.functypes[i], m.type.functypes[type]))
        return i;
  }

  return type;
}

IN_ERROR Compiler::CompileIndirectCall(varuint32 index)
{
  index = GetFirstType(index);
  if(index >= m.type.n_functypes)
    return ERR_INVALID_TYPE_INDEX;

  IN_ERROR err;
  FunctionType& ftype = m.type.functypes[index];
  llvmVal* callee;
  if(err = PopType(TE_i32, callee))
    return err;

  if(tables.size() < 1)
    return ERR_INVALID_TABLE_INDEX;

  // Pop arguments in reverse order
  llvmVal** ArgsV = tmalloc<llvmVal*>(env, ftype.n_params);
  if(ftype.n_params > 0 && !ArgsV)
    return ERR_FATAL_OUT_OF_MEMORY;

  for(unsigned int i = ftype.n_params; i-- > 0;)
  {
    if(err = PopType(ftype.params[i], ArgsV[i]))
      return err;
  }

  uint64_t bytewidth = mod->getDataLayout().getTypeAllocSize(
    tables[0]->getType()->getElementType()->getContainedType(0)->getPointerElementType());

  if(env.flags & ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if index is out of bounds
  {
    InsertConditionalTrap(builder.CreateICmpUGE(builder.CreateIntCast(callee, builder.getInt64Ty(), false),
                                                builder.CreateUDiv(GetMemSize(tables[0]), builder.getInt64(bytewidth)),
                                                "indirect_call_oob_check"));
  }

  // Deference global variable to get the actual array of function pointers, index into them, then dereference that array
  // index to get the actual function pointer
  llvmVal* funcptr = builder.CreateLoad(builder.CreateInBoundsGEP(builder.CreateLoad(GetPairPtr(tables[0], 0)),
                                                                  { callee, builder.getInt32(0) }),
                                        "indirect_call_load_func_ptr");

  if(env.flags & ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if function pointer is NULL
    InsertConditionalTrap(
      builder.CreateICmpEQ(builder.CreatePtrToInt(funcptr, intptrty), CInt::get(intptrty, 0), "indirect_call_null_check"));

  // Now that we have the function pointer we have to actually cast back to the function signature that we expect, instead
  // of void()
  FuncTy* ty = GetFunctionType(ftype);
  funcptr    = builder.CreatePointerCast(funcptr, ty->getPointerTo(0));

  if(env.flags &
     ENV_CHECK_INDIRECT_CALL) // In strict mode, trap if the expected type does not match the actual type of the function
  {
    auto sig = builder.CreateLoad(
      builder.CreateInBoundsGEP(builder.CreateLoad(GetPairPtr(tables[0], 0)), { callee, builder.getInt32(1) }));
    InsertConditionalTrap(builder.CreateICmpNE(sig, builder.getInt32(index), "indirect_call_sig_check"));
  }

  // CreateCall will then do the final dereference of the function pointer to make the indirect call
  CallInst* call = builder.CreateCall(funcptr, llvm::makeArrayRef(ArgsV, ftype.n_params));
  if(memories.size() > 0)
    builder.CreateStore(builder.CreateLoad(GetPairPtr(memories[0], 0)), memlocal);

  builder.GetInsertBlock()->getParent()->setMetadata(IN_MEMORY_GROW_METADATA, llvm::MDNode::get(ctx, {}));

  if(env.flags & ENV_DISABLE_TAIL_CALL) // In strict mode, tail call optimization is not allowed
    call->setTailCallKind(CallInst::TCK_NoTail);
  call->setCallingConv(InternalConvention); // Always pick the fast convention, because the table is always set to
                                            // the internal wrapping function

  if(!ty->getReturnType()->isVoidTy()) // Only push a value if there is one to push
    return PushReturn(call);
  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileConstant(Instruction& instruction, llvm::Constant*& constant)
{
  switch(instruction.opcode)
  {
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
    constant = CInt::get(ctx, llvm::APInt(32, instruction.immediates[0]._varuint32, true));
    break;
  case OP_i64_const: constant = CInt::get(ctx, llvm::APInt(64, instruction.immediates[0]._varuint64, true)); break;
  case OP_f32_const: constant = ConstantFP::get(ctx, llvm::APFloat(instruction.immediates[0]._float32)); break;
  case OP_f64_const: constant = ConstantFP::get(ctx, llvm::APFloat(instruction.immediates[0]._float64)); break;
  default: return ERR_INVALID_INITIALIZER;
  }

  return ERR_SUCCESS;
}

template<bool SIGNED>
IN_ERROR Compiler::CompileLoad(varuint7 memory, varuint32 offset, varuint32 memflags, const char* name, llvmTy* ext,
                               llvmTy* ty)
{
  if(memories.size() < 1)
    return ERR_INVALID_MEMORY_INDEX;

  llvmVal* base;
  IN_ERROR err;
  if(err = PopType(TE_i32, base))
    return err;

  // TODO: In strict mode, we may have to disregard the alignment hint
  llvmVal* result =
    builder.CreateAlignedLoad(GetMemPointer(base, ty->getPointerTo(0), memory, offset), (1 << memflags), name);

  if(ext != nullptr)
    result = SIGNED ? builder.CreateSExt(result, ext) : builder.CreateZExt(result, ext);

  return PushReturn(result);
}

template<WASM_TYPE_ENCODING TY>
IN_ERROR Compiler::CompileStore(varuint7 memory, varuint32 offset, varuint32 memflags, const char* name,
                                llvm::IntegerType* ext)
{
  if(memories.size() < 1)
    return ERR_INVALID_MEMORY_INDEX;

  IN_ERROR err;
  llvmVal *value, *base;
  if(err = PopType(TY, value))
    return err;
  if(err = PopType(TE_i32, base))
    return err;

  llvmTy* PtrType = !ext ? GetLLVMType(TY) : ext;

  // TODO: In strict mode, we may have to disregard the alignment hint
  llvmVal* ptr = GetMemPointer(base, PtrType->getPointerTo(0), memory, offset);
  builder.CreateAlignedStore(!ext ? value : builder.CreateIntCast(value, ext, false), ptr, (1 << memflags), name);

  return ERR_SUCCESS;
}

// Gets memory size in pages, not bytes
llvmVal* Compiler::CompileMemSize(llvm::GlobalVariable* target)
{
  return builder.CreateIntCast(builder.CreateLShr(GetMemSize(target), 16), builder.getInt32Ty(), true);
}

IN_ERROR Compiler::CompileMemGrow(const char* name)
{
  if(memories.size() < 1)
    return ERR_INVALID_MEMORY_INDEX;

  IN_ERROR err;
  llvmVal* delta;
  if(err = PopType(TE_i32, delta))
    return err;

  llvmVal* old = CompileMemSize(memories[0]);

  auto max =
    llvm::cast<llvm::ConstantAsMetadata>(memories[0]->getMetadata(IN_MEMORY_MAX_METADATA)->getOperand(0))->getValue();
  CallInst* call = builder.CreateCall(memgrow,
                                      { builder.CreateLoad(GetPairPtr(memories[0], 0)),
                                        builder.CreateShl(builder.CreateZExt(delta, builder.getInt64Ty()), 16), max,
                                        GetPairPtr(memories[0], 1) },
                                      name);

  llvmVal* success = builder.CreateICmpNE(builder.CreatePtrToInt(call, intptrty), CInt::get(intptrty, 0));

  auto oldblock     = builder.GetInsertBlock();
  auto successblock = BB::Create(ctx, "grow_success", builder.GetInsertBlock()->getParent());
  auto contblock    = BB::Create(ctx, "grow_fail", builder.GetInsertBlock()->getParent());

  builder.CreateCondBr(success, successblock, contblock);
  builder.SetInsertPoint(successblock); // Only set new memory if call succeeded
  builder.CreateAlignedStore(call, GetPairPtr(memories[0], 0), builder.getInt64Ty()->getPrimitiveSizeInBits() / 8);
  builder.CreateStore(builder.CreateLoad(GetPairPtr(memories[0], 0)), memlocal);
  builder.CreateBr(contblock);

  builder.SetInsertPoint(contblock);

  auto phi = builder.CreatePHI(builder.getInt32Ty(), 2);
  phi->addIncoming(old, successblock);
  phi->addIncoming(CInt::get(builder.getInt32Ty(), -1, true), oldblock);

  builder.GetInsertBlock()->getParent()->setMetadata(IN_MEMORY_GROW_METADATA, llvm::MDNode::get(ctx, {}));
  return PushReturn(phi);
}

template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IN_ERROR Compiler::CompileSRem(const llvm::Twine& name)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  if(env.flags & ENV_CHECK_INT_DIVISION)
    InsertConditionalTrap(builder.CreateICmpEQ(val2, CInt::get(val2->getType(), 0, true)));

  // The specific case of INT_MIN % -1 is undefined behavior in LLVM and crashes on x86, but WASM requires that it return
  // 0, so we branch on that specific case.
  llvmVal* cond = builder.CreateAnd(builder.CreateICmpEQ(val1, (val1->getType()->getIntegerBitWidth() == 32) ?
                                                                 builder.getInt32(0x80000000) :
                                                                 builder.getInt64(0x8000000000000000)),
                                    builder.CreateICmpEQ(val2, CInt::get(val2->getType(), ~0ULL, true)));
  return PushReturn(builder.CreateSRem(builder.CreateSelect(cond, CInt::get(val1->getType(), 0, true), val1), val2, name));
}

// Given a function pointer to the appropriate builder function, pops two binary arguments off the stack and pushes the
// result
template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR, typename... Args>
IN_ERROR Compiler::CompileDiv(bool overflow, llvmVal* (llvm::IRBuilder<>::*op)(llvmVal*, llvmVal*, Args...), Args... args)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  llvmVal* cond = builder.CreateICmpEQ(val2, CInt::get(val2->getType(), 0, true));
  if(overflow)
    cond =
      builder.CreateOr(cond, builder.CreateAnd(builder.CreateICmpEQ(val1, (val1->getType()->getIntegerBitWidth() == 32) ?
                                                                            builder.getInt32(0x80000000) :
                                                                            builder.getInt64(0x8000000000000000)),
                                               builder.CreateICmpEQ(val2, CInt::get(val2->getType(), ~0ULL, true))));

  if(env.flags & ENV_CHECK_INT_DIVISION)
    InsertConditionalTrap(cond);
  return PushReturn((builder.*op)(val1, val2, args...));
}

template<WASM_TYPE_ENCODING Ty1, WASM_TYPE_ENCODING Ty2, WASM_TYPE_ENCODING TyR>
IN_ERROR Compiler::CompileFloatCmp(llvm::Intrinsic::ID id, const llvm::Twine& name)
{
  IN_ERROR err;

  // Pop in reverse order
  llvmVal *val2, *val1;
  if(err = PopType(Ty2, val2))
    return err;
  if(err = PopType(Ty1, val1))
    return err;

  // WASM requires we return an NaN if either operand is NaN
  auto nancheck = builder.CreateFCmpUNO(val1, val2);
  auto compare  = builder.CreateBinaryIntrinsic(id, val1, val2, nullptr, name);
  return PushReturn(builder.CreateSelect(nancheck, llvm::ConstantFP::getNaN(val1->getType()), compare));
}


IN_ERROR Compiler::CompileInstruction(Instruction& ins)
{
  // fputs(OPNAMES[ins.opcode], env.log);
  // fputc('\n', env.log);
  // DumpCompilerState();

  switch(ins.opcode)
  {
  case OP_unreachable:
    CompileTrap(); // Automatically terminates block as unreachable
    PolymorphicStack();
    return ERR_SUCCESS;
  case OP_nop: return ERR_SUCCESS;
  case OP_block:
    PushLabel("block", ins.immediates[0]._varsint7, OP_block, nullptr, debugger->_curscope);
    return ERR_SUCCESS;
  case OP_loop:
    PushLabel("loop", ins.immediates[0]._varsint7, OP_loop, nullptr, debugger->_curscope);
    builder.CreateBr(control.Peek().block); // Branch into next block
    BindLabel(control.Peek().block);
    return ERR_SUCCESS;
  case OP_if: return CompileIfBlock(ins.immediates[0]._varsint7);
  case OP_else: return CompileElseBlock();
  case OP_end: return CompileEndBlock();
  case OP_br: return CompileBranch(ins.immediates[0]._varuint32);
  case OP_br_if: return CompileIfBranch(ins.immediates[0]._varuint32);
  case OP_br_table:
    return CompileBranchTable(ins.immediates[0].n_table, ins.immediates[0].table, ins.immediates[1]._varuint32);
  case OP_return:
  {
    IN_ERROR err = CompileReturn(control[control.Size() - 1].sig);
    PolymorphicStack();
    return err;
  }

  // Call operators
  case OP_call: return CompileCall(ins.immediates[0]._varuint32);
  case OP_call_indirect:
    return CompileIndirectCall(ins.immediates[0]._varuint32);

    // Parametric operators
  case OP_drop:
    if(values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    if(values.Peek() != nullptr)
      values.Pop(); // We do not delete the value because it could be referenced elsewhere (e.g. in a branch)
    return ERR_SUCCESS;
  case OP_select:
    return CompileSelectOp(OPNAMES[ins.opcode], nullptr);

    // Variable access
  case OP_local_get:
  {
    auto local = GetLocal(ins.immediates[0]._varuint32);
    if(!local)
      return ERR_INVALID_LOCAL_INDEX;
    PushReturn(builder.CreateLoad(local));
    return ERR_SUCCESS;
  }
  case OP_local_set:
  case OP_local_tee:
  {
    auto local = GetLocal(ins.immediates[0]._varuint32);
    if(!local)
      return ERR_INVALID_LOCAL_INDEX;
    if(values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    builder.CreateStore(!values.Peek() ? llvm::Constant::getAllOnesValue(
                                           llvm::cast<llvm::PointerType>(local->getType())->getElementType()) :
                                         values.Peek(),
                        local);
    if(values.Peek() != nullptr &&
       ins.opcode == OP_local_set) // tee_local is the same as set_local except the operand isn't popped
      values.Pop();
    return ERR_SUCCESS;
  }
  case OP_global_set:
    if(ins.immediates[0]._varuint32 >= globals.size())
      return ERR_INVALID_GLOBAL_INDEX;
    if(values.Size() < 1)
      return ERR_INVALID_VALUE_STACK;
    builder.CreateStore(!values.Peek() ? llvm::Constant::getAllOnesValue(
                                           globals[ins.immediates[0]._varuint32]->getType()->getElementType()) :
                                         values.Pop(),
                        globals[ins.immediates[0]._varuint32]);
    debugger->DebugSetGlobal(ins.immediates[0]._varuint32);
    return ERR_SUCCESS;
  case OP_global_get:
    if(ins.immediates[0]._varuint32 >= globals.size())
      return ERR_INVALID_GLOBAL_INDEX;
    PushReturn(builder.CreateLoad(globals[ins.immediates[0]._varuint32]));
    return ERR_SUCCESS;

    // Memory-related operators
  case OP_i32_load:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr,
                              builder.getInt32Ty());
  case OP_i64_load:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr,
                              builder.getInt64Ty());
  case OP_f32_load:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr,
                              builder.getFloatTy());
  case OP_f64_load:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode], nullptr,
                              builder.getDoubleTy());
  case OP_i32_load8_s:
    return CompileLoad<true>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                             builder.getInt32Ty(), builder.getInt8Ty());
  case OP_i32_load8_u:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                              builder.getInt32Ty(), builder.getInt8Ty());
  case OP_i32_load16_s:
    return CompileLoad<true>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                             builder.getInt32Ty(), builder.getInt16Ty());
  case OP_i32_load16_u:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                              builder.getInt32Ty(), builder.getInt16Ty());
  case OP_i64_load8_s:
    return CompileLoad<true>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                             builder.getInt64Ty(), builder.getInt8Ty());
  case OP_i64_load8_u:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                              builder.getInt64Ty(), builder.getInt8Ty());
  case OP_i64_load16_s:
    return CompileLoad<true>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                             builder.getInt64Ty(), builder.getInt16Ty());
  case OP_i64_load16_u:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                              builder.getInt64Ty(), builder.getInt16Ty());
  case OP_i64_load32_s:
    return CompileLoad<true>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                             builder.getInt64Ty(), builder.getInt32Ty());
  case OP_i64_load32_u:
    return CompileLoad<false>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                              builder.getInt64Ty(), builder.getInt32Ty());
  case OP_i32_store:
    return CompileStore<TE_i32>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                nullptr);
  case OP_i64_store:
    return CompileStore<TE_i64>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                nullptr);
  case OP_f32_store:
    return CompileStore<TE_f32>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                nullptr);
  case OP_f64_store:
    return CompileStore<TE_f64>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                nullptr);
  case OP_i32_store8:
    return CompileStore<TE_i32>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                builder.getInt8Ty());
  case OP_i32_store16:
    return CompileStore<TE_i32>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                builder.getInt16Ty());
  case OP_i64_store8:
    return CompileStore<TE_i64>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                builder.getInt8Ty());
  case OP_i64_store16:
    return CompileStore<TE_i64>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                builder.getInt16Ty());
  case OP_i64_store32:
    return CompileStore<TE_i64>(0, ins.immediates[1]._varuint32, ins.immediates[0]._varuint32, OPNAMES[ins.opcode],
                                builder.getInt32Ty());
  case OP_memory_size: return PushReturn(CompileMemSize(memories[0]));
  case OP_memory_grow:
    return CompileMemGrow(OPNAMES[ins.opcode]);

    // Constants
  case OP_i32_const: // While we interpret this as unsigned, it is cast to a signed int.
  case OP_i64_const:
  case OP_f32_const:
  case OP_f64_const:
  {
    llvm::Constant* constant;
    IN_ERROR err = CompileConstant(ins, constant);
    if(!err)
      PushReturn(constant);
    return err;
  }

  // Comparison operators
  case OP_i32_eqz: values.Push(builder.getInt32(0)); // Fallthrough to OP_i32_eq
  case OP_i32_eq:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpEQ,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_ne:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpNE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_lt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSLT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_lt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpULT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_gt_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSGT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_gt_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpUGT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_le_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSLE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_le_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpULE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_ge_s:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSGE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i32_ge_u:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpUGE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_eqz: values.Push(builder.getInt64(0)); // Fallthrough to OP_i64_eq
  case OP_i64_eq:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpEQ,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_ne:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpNE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_lt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSLT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_lt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpULT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_gt_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSGT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_gt_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpUGT,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_le_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSLE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_le_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpULE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_ge_s:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpSGE,
                                                                       OPNAMES[ins.opcode]);
  case OP_i64_ge_u:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateICmpUGE,
                                                                       OPNAMES[ins.opcode]);
  case OP_f32_eq:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOEQ,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_ne:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpUNE,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_lt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOLT,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_gt:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOGT,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_le:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOLE,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_ge:
    return CompileBinaryOp<TE_f32, TE_f32, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOGE,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_eq:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOEQ,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_ne:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpUNE,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_lt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOLT,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_gt:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOGT,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_le:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOLE,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_ge:
    return CompileBinaryOp<TE_f64, TE_f64, TE_i32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFCmpOGE,
                                                                                      OPNAMES[ins.opcode], nullptr);

    // Numeric operators
  case OP_i32_clz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(llvm::Intrinsic::ctlz, OPNAMES[ins.opcode], builder.getInt1(false));
  case OP_i32_ctz:
    return CompileUnaryIntrinsic<TE_i32, TE_i32>(llvm::Intrinsic::cttz, OPNAMES[ins.opcode], builder.getInt1(false));
  case OP_i32_popcnt: return CompileUnaryIntrinsic<TE_i32, TE_i32>(llvm::Intrinsic::ctpop, OPNAMES[ins.opcode]);
  case OP_i32_add:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateAdd,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i32_sub:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateSub,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i32_mul:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateMul,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i32_div_s:
    return CompileDiv<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(true, &llvm::IRBuilder<>::CreateSDiv,
                                                                        OPNAMES[ins.opcode], false);
  case OP_i32_div_u:
    return CompileDiv<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(false, &llvm::IRBuilder<>::CreateUDiv,
                                                                        OPNAMES[ins.opcode], false);
  case OP_i32_rem_s: return CompileSRem<TE_i32, TE_i32, TE_i32>(OPNAMES[ins.opcode]);
  case OP_i32_rem_u:
    return CompileDiv<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(false, &llvm::IRBuilder<>::CreateURem,
                                                                  OPNAMES[ins.opcode]);
  case OP_i32_and:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i32_or:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i32_xor:
    return CompileBinaryOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&>(&llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i32_shl:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateShl,
                                                                                        OPNAMES[ins.opcode], false, false);
  case OP_i32_shr_s:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(&llvm::IRBuilder<>::CreateAShr,
                                                                                  OPNAMES[ins.opcode], false);
  case OP_i32_shr_u:
    return CompileBinaryShiftOp<TE_i32, TE_i32, TE_i32, const llvm::Twine&, bool>(&llvm::IRBuilder<>::CreateLShr,
                                                                                  OPNAMES[ins.opcode], false);
  case OP_i32_rotl: return CompileRotationOp<TE_i32, true>(OPNAMES[ins.opcode]);
  case OP_i32_rotr: return CompileRotationOp<TE_i32, false>(OPNAMES[ins.opcode]);
  case OP_i64_clz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(llvm::Intrinsic::ctlz, OPNAMES[ins.opcode], builder.getInt1(false));
  case OP_i64_ctz:
    return CompileUnaryIntrinsic<TE_i64, TE_i64>(llvm::Intrinsic::cttz, OPNAMES[ins.opcode], builder.getInt1(false));
  case OP_i64_popcnt: return CompileUnaryIntrinsic<TE_i64, TE_i64>(llvm::Intrinsic::ctpop, OPNAMES[ins.opcode]);
  case OP_i64_add:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateAdd,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i64_sub:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateSub,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i64_mul:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateMul,
                                                                                   OPNAMES[ins.opcode], false, false);
  case OP_i64_div_s:
    return CompileDiv<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(true, &llvm::IRBuilder<>::CreateSDiv,
                                                                        OPNAMES[ins.opcode], false);
  case OP_i64_div_u:
    return CompileDiv<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(false, &llvm::IRBuilder<>::CreateUDiv,
                                                                        OPNAMES[ins.opcode], false);
  case OP_i64_rem_s: return CompileSRem<TE_i64, TE_i64, TE_i64>(OPNAMES[ins.opcode]);
  case OP_i64_rem_u:
    return CompileDiv<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(false, &llvm::IRBuilder<>::CreateURem,
                                                                  OPNAMES[ins.opcode]);
  case OP_i64_and:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(&llvm::IRBuilder<>::CreateAnd, OPNAMES[ins.opcode]);
  case OP_i64_or:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(&llvm::IRBuilder<>::CreateOr, OPNAMES[ins.opcode]);
  case OP_i64_xor:
    return CompileBinaryOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&>(&llvm::IRBuilder<>::CreateXor, OPNAMES[ins.opcode]);
  case OP_i64_shl:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool, bool>(&llvm::IRBuilder<>::CreateShl,
                                                                                        OPNAMES[ins.opcode], false, false);
  case OP_i64_shr_s:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(&llvm::IRBuilder<>::CreateAShr,
                                                                                  OPNAMES[ins.opcode], false);
  case OP_i64_shr_u:
    return CompileBinaryShiftOp<TE_i64, TE_i64, TE_i64, const llvm::Twine&, bool>(&llvm::IRBuilder<>::CreateLShr,
                                                                                  OPNAMES[ins.opcode], false);
  case OP_i64_rotl: return CompileRotationOp<TE_i64, true>(OPNAMES[ins.opcode]);
  case OP_i64_rotr: return CompileRotationOp<TE_i64, false>(OPNAMES[ins.opcode]);
  case OP_f32_abs: return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f32_neg:
    return CompileUnaryOp<TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFNeg,
                                                                             OPNAMES[ins.opcode], nullptr);
  case OP_f32_ceil: return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f32_floor: return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f32_trunc:
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::trunc, OPNAMES[ins.opcode]);
    return CompileUnaryOp<TE_f32, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPTrunc,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_nearest: // We must round floats using IEEE 754-2008 roundToIntegralTowardZero, which rint gaurantees
    return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::nearbyint, OPNAMES[ins.opcode]);
  case OP_f32_sqrt: return CompileUnaryIntrinsic<TE_f32, TE_f32>(llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f32_add:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFAdd,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_sub:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFSub,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_mul:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFMul,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_div:
    return CompileBinaryOp<TE_f32, TE_f32, TE_f32, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFDiv,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f32_min: return CompileFloatCmp<TE_f32, TE_f32, TE_f32>(llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f32_max: return CompileFloatCmp<TE_f32, TE_f32, TE_f32>(llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f32_copysign:
    return CompileBinaryIntrinsic<TE_f32, TE_f32, TE_f32>(llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);
  case OP_f64_abs: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::fabs, OPNAMES[ins.opcode]);
  case OP_f64_neg:
    return CompileUnaryOp<TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFNeg,
                                                                             OPNAMES[ins.opcode], nullptr);
  case OP_f64_ceil: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::ceil, OPNAMES[ins.opcode]);
  case OP_f64_floor: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::floor, OPNAMES[ins.opcode]);
  case OP_f64_trunc: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::trunc, OPNAMES[ins.opcode]);
  case OP_f64_nearest: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::nearbyint, OPNAMES[ins.opcode]);
  case OP_f64_sqrt: return CompileUnaryIntrinsic<TE_f64, TE_f64>(llvm::Intrinsic::sqrt, OPNAMES[ins.opcode]);
  case OP_f64_add:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFAdd,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_sub:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFSub,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_mul:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFMul,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_div:
    return CompileBinaryOp<TE_f64, TE_f64, TE_f64, const llvm::Twine&, llvm::MDNode*>(&llvm::IRBuilder<>::CreateFDiv,
                                                                                      OPNAMES[ins.opcode], nullptr);
  case OP_f64_min: return CompileFloatCmp<TE_f64, TE_f64, TE_f64>(llvm::Intrinsic::minnum, OPNAMES[ins.opcode]);
  case OP_f64_max: return CompileFloatCmp<TE_f64, TE_f64, TE_f64>(llvm::Intrinsic::maxnum, OPNAMES[ins.opcode]);
  case OP_f64_copysign:
    return CompileBinaryIntrinsic<TE_f64, TE_f64, TE_f64>(llvm::Intrinsic::copysign, OPNAMES[ins.opcode]);

    // Conversions
  case OP_i32_wrap_i64:
    return CompileUnaryOp<TE_i64, TE_i32, llvmTy*, bool, const llvm::Twine&>(&llvm::IRBuilder<>::CreateIntCast,
                                                                             builder.getInt32Ty(), true,
                                                                             OPNAMES[ins.opcode]);
  case OP_i32_trunc_f32_s: // These truncation values are specifically picked to be the largest representable 32-bit
                           // floating point value that can be safely converted.
    InsertTruncTrap(2147483520.0, -2147483650.0, builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToSI,
                                                                       builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_f32_u: // This is truncation, so values of up to -0.999999... are actually valid unsigned integers
                           // because they are truncated to 0
    InsertTruncTrap(4294967040.0, -0.999999940, builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToUI,
                                                                       builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_f64_s: // Doubles can exactly represent 32-bit integers, so the real values are used
    InsertTruncTrap(2147483647.0, -2147483648.0, builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToSI,
                                                                       builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i32_trunc_f64_u:
    InsertTruncTrap(4294967295.0, -0.99999999999999989, builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToUI,
                                                                       builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_extend_i32_s:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const llvm::Twine&>(&llvm::IRBuilder<>::CreateIntCast,
                                                                             builder.getInt64Ty(), true,
                                                                             OPNAMES[ins.opcode]);
  case OP_i64_extend_i32_u:
    return CompileUnaryOp<TE_i32, TE_i64, llvmTy*, bool, const llvm::Twine&>(&llvm::IRBuilder<>::CreateIntCast,
                                                                             builder.getInt64Ty(), false,
                                                                             OPNAMES[ins.opcode]);
  case OP_i64_trunc_f32_s:
    InsertTruncTrap(9223371490000000000.0, -9223372040000000000.0, builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToSI,
                                                                       builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_f32_u:
    InsertTruncTrap(18446743000000000000.0, -0.999999940, builder.getFloatTy());
    return CompileUnaryOp<TE_f32, TE_i64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToUI,
                                                                       builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_f64_s: // Largest representable 64-bit floating point values that can be safely converted
    InsertTruncTrap(9223372036854774800.0, -9223372036854775800.0, builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToSI,
                                                                       builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_i64_trunc_f64_u:
    InsertTruncTrap(18446744073709550000.0, -0.99999999999999989, builder.getDoubleTy());
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPToUI,
                                                                       builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_convert_i32_s:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateSIToFP,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_i32_u:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateUIToFP,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_i64_s:
    return CompileUnaryOp<TE_i64, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateSIToFP,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_convert_i64_u:
    return CompileUnaryOp<TE_i64, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateUIToFP,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f32_demote_f64:
    return CompileUnaryOp<TE_f64, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPTrunc,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_i32_s:
    return CompileUnaryOp<TE_i32, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateSIToFP,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_i32_u:
    return CompileUnaryOp<TE_i32, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateUIToFP,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_i64_s:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateSIToFP,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_convert_i64_u:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateUIToFP,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);
  case OP_f64_promote_f32:
    return CompileUnaryOp<TE_f32, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateFPExt,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);

    // Reinterpretations
  case OP_i32_reinterpret_f32:
    return CompileUnaryOp<TE_f32, TE_i32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateBitCast,
                                                                       builder.getInt32Ty(), OPNAMES[ins.opcode]);
  case OP_i64_reinterpret_f64:
    return CompileUnaryOp<TE_f64, TE_i64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateBitCast,
                                                                       builder.getInt64Ty(), OPNAMES[ins.opcode]);
  case OP_f32_reinterpret_i32:
    return CompileUnaryOp<TE_i32, TE_f32, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateBitCast,
                                                                       builder.getFloatTy(), OPNAMES[ins.opcode]);
  case OP_f64_reinterpret_i64:
    return CompileUnaryOp<TE_i64, TE_f64, llvmTy*, const llvm::Twine&>(&llvm::IRBuilder<>::CreateBitCast,
                                                                       builder.getDoubleTy(), OPNAMES[ins.opcode]);
  default: return ERR_FATAL_UNKNOWN_INSTRUCTION;
  }

  assert(false); // ERROR NOT IMPLEMENTED
  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileFunctionBody(Func* fn, size_t indice, llvm::AllocaInst*& memref, FunctionDesc& desc,
                                       FunctionBody& body)
{
  // Ensure context is reset
  assert(!control.Size() && !control.Limit());
  assert(!values.Size() && !values.Limit());

  if(desc.type_index >= m.type.n_functypes)
    return ERR_INVALID_TYPE_INDEX;
  auto& sig = m.type.functypes[desc.type_index];

  if(sig.n_params != fn->arg_size())
    return ERR_SIGNATURE_MISMATCH;

  // Get return value
  varsint7 ret = TE_void;
  if(sig.n_returns > 0)
    ret = sig.returns[0];

  debugger->FuncBody(fn, indice, desc, body);
  PushLabel("exit", ret, OP_return, nullptr,
            fn->getSubprogram()); // Setup the function exit block that wraps everything

  builder.SetInsertPoint(BB::Create(ctx, "entry", fn)); // Setup initial basic block.
  locals.resize(0);
  locals.reserve(sig.n_params + body.n_locals);
  varuint32 index  = 0;
  varuint32 offset = 0;
  auto curloc      = builder.getCurrentDebugLocation();

  // We allocate parameters first, followed by local variables
  for(auto& arg : fn->args())
  {
    std::string name = "$p" + std::to_string(index + 1);
    if(desc.param_debug && desc.param_debug[index].name.size())
      name = desc.param_debug[index].name.str();

    assert(index < sig.n_params);
    auto ty = GetLLVMType(sig.params[index]);
    assert(ty == arg.getType());
    locals.push_back(builder.CreateAlloca(ty, nullptr, name));
    locals.back()->setMetadata(IN_LOCAL_INDEX_METADATA,
                               llvm::MDNode::get(ctx, { llvm::ConstantAsMetadata::get(builder.getInt32(offset++)) }));

    debugger->FuncParam(fn, index, desc);
    ++index;
    builder.CreateStore(&arg, locals.back()); // Store parameter (we can't use the parameter directly
                                              // because wasm lets you store to parameters)
  }

  for(varuint32 i = 0; i < body.n_locals; ++i)
  {
    std::string name = "$l" + std::to_string(offset + 1);
    if(body.locals[i].debug.name.size())
      name = body.locals[i].debug.name.str();

    auto ty    = GetLLVMType(body.locals[i].type);
    auto count = (body.locals[i].count > 1) ? builder.getInt32(body.locals[i].count) : nullptr;
    locals.push_back(builder.CreateAlloca(ty, count, name));
    locals.back()->setMetadata(IN_LOCAL_INDEX_METADATA,
                               llvm::MDNode::get(ctx, { llvm::ConstantAsMetadata::get(builder.getInt32(offset)) }));

    debugger->FuncLocal(fn, index, desc);
    builder.CreateStore(llvm::Constant::getNullValue(ty), locals.back());
    offset += body.locals[i].count;
  }

  unsigned int stacksize = 0;
  for(auto local : locals)
    stacksize += (local->getType()->getElementType()->getPrimitiveSizeInBits() / 8);

  if(memories.size() > 0)
  {
    memref = memlocal =
      builder.CreateAlloca(memories[0]->getType()->getElementType()->getContainedType(0), nullptr, "IN_!memlocal");
    builder.CreateStore(builder.CreateLoad(GetPairPtr(memories[0], 0)), memlocal);
    stacksize += (memlocal->getType()->getElementType()->getPrimitiveSizeInBits() / 8);
  }

  debugger->PostFuncBody(fn, body);

  // If we allocate more than 2048 bytes of stack space, make a stack probe so we can't blow past the gaurd page.
  if(stacksize > 2048)
    fn->addFnAttr("probe-stack");

  // Begin iterating through the instructions until there aren't any left
  for(varuint32 i = 0; i < body.n_body; ++i)
  {
    debugger->DebugIns(fn, body.body[i]);
    IN_ERROR err = CompileInstruction(body.body[i]);
    if(err < 0)
      return err;
  }

  memlocal = nullptr;
  if(values.Size() > 0 && !values.Peek()) // Pop at most 1 polymorphic type off the stack. Any additional ones are an error.
    values.Pop();
  if(body.body[body.n_body - 1].opcode != OP_end)
    return ERR_FATAL_EXPECTED_END_INSTRUCTION;
  if(control.Size() > 0 || control.Limit() > 0)
    return ERR_END_MISMATCH;
  if(values.Size() > 0 || values.Limit() > 0)
    return ERR_INVALID_VALUE_STACK;
  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileInitGlobal(Module& m, varuint32 index, llvm::Constant*& out)
{
  size_t i = index + m.importsection.memories; // Shift index to globals section
  if(i < m.importsection.globals)
  {
    auto external = ResolveExport(env, m.importsection.imports[i]);
    if(!external.first)
      return ERR_UNKNOWN_MODULE;
    if(!external.second)
      return ERR_UNKNOWN_EXPORT;
    if(external.second->kind != WASM_KIND_GLOBAL)
      return ERR_INVALID_GLOBAL_IMPORT_TYPE;
    return CompileInitGlobal(*external.first, external.second->index, out);
  }
  i -= m.importsection.globals;
  if(i < m.global.n_globals)
    return CompileInitConstant(m.global.globals[i].init, m, out);
  return ERR_INVALID_GLOBAL_INDEX;
}

IN_ERROR Compiler::CompileInitConstant(Instruction& instruction, Module& m, llvm::Constant*& out)
{
  if(instruction.opcode == OP_global_get)
    return CompileInitGlobal(m, instruction.immediates[0]._varuint32, out);
  return CompileConstant(instruction, out);
}
