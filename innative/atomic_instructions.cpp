// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "compile.h"
#include "utility.h"
#include "validate.h"
#include "atomic_instructions.h"

using namespace innative;
using namespace utility;
using namespace innative::atomic_details;

using Func    = llvm::Function;
using FuncTy  = llvm::FunctionType;
using llvmTy  = llvm::Type;
using llvmVal = llvm::Value;

IN_ERROR Compiler::InsertAlignmentTrap(llvmVal* ptr, varuint32 memory, varuint32 memflags)
{
  if(!memflags) // Only bother if alignment > 1
    return ERR_SUCCESS;

  auto alignment = 1ULL << memflags;
  auto sizet     = builder.getIntNTy(machine->getPointerSizeInBits(0));
  auto ptrtoint  = builder.CreatePtrToInt(ptr, sizet);
  auto zero      = CInt::get(sizet, 0);
  auto mask      = CInt::get(sizet, alignment - 1);
  auto cond      = builder.CreateICmpNE(builder.CreateAnd(ptrtoint, mask), zero);
  InsertConditionalTrap(cond);

  return ERR_SUCCESS;
}

IN_ERROR Compiler::InsertAtomicMemGet(Instruction& ins, llvmVal*& ptr, unsigned& align)
{
  IN_ERROR err;

  llvmVal* base;
  if(err = PopType(TE_i32, base))
    return err;

  varuint32 memflags = ins.immediates[0]._varuint32;
  varuint32 offset   = ins.immediates[1]._varuint32;
  varuint32 memory   = ins.immediates[2]._varuint32;
  align              = 1u << memflags;

  // GetMemPointer already checks for us that the address is in-bounds
  ptr = GetMemPointer(base, builder.getIntNTy(align * 8)->getPointerTo(), memory, offset);

  // Trap on alignment failure
  InsertAlignmentTrap(ptr, memory, memflags);

  return ERR_SUCCESS;
}

template<typename F> IN_ERROR Compiler::CompileAtomicMemInstruction(Instruction& ins, F&& inner)
{
  IN_ERROR err;
  unsigned align;
  llvmVal* ptr;

  if(err = InsertAtomicMemGet(ins, ptr, align))
    return err;

  return inner(align, ptr);
}

template<typename F> IN_ERROR Compiler::CompileAtomicMemInstruction(Instruction& ins, WASM_TYPE_ENCODING arg2Ty, F&& inner)
{
  IN_ERROR err;
  unsigned align;
  llvmVal *ptr, *arg2;

  if(err = PopType(arg2Ty, arg2))
    return err;
  if(err = InsertAtomicMemGet(ins, ptr, align))
    return err;

  return inner(align, ptr, arg2);
}

template<typename F>
IN_ERROR Compiler::CompileAtomicMemInstruction(Instruction& ins, WASM_TYPE_ENCODING arg2Ty, WASM_TYPE_ENCODING arg3Ty,
                                               F&& inner)
{
  IN_ERROR err;
  unsigned align;
  llvmVal *ptr, *arg2, *arg3;

  if(err = PopType(arg3Ty, arg3))
    return err;
  if(err = PopType(arg2Ty, arg2))
    return err;
  if(err = InsertAtomicMemGet(ins, ptr, align))
    return err;

  return inner(align, ptr, arg2, arg3);
}

IN_ERROR Compiler::CompileAtomicNotify(Instruction& ins, const char* name)
{
  return CompileAtomicMemInstruction(ins, TE_i32, [&](unsigned, llvmVal* ptr, llvmVal* count) {
    ptr       = builder.CreatePointerCast(ptr, builder.getInt8PtrTy());
    auto call = builder.CreateCall(atomic_notify, { ptr, count });
    return PushReturn(call);
  });
}

IN_ERROR Compiler::CompileAtomicWait(Instruction& ins, WASM_TYPE_ENCODING varTy, llvm::Function* wait_func,
                                     const char* name)
{
  // TODO: Trap if the memory isn't shared? Double-check this because
  // that sounds like the spec is confused. We should know whether the
  // memory will be shared or not at compile time... I can just insert an
  // unconditional trap if it isn't shared I guess but :shrug:

  return CompileAtomicMemInstruction(ins, varTy, TE_i64, [&](unsigned, llvmVal* ptr, llvmVal* expected, llvmVal* count) {
    ptr = builder.CreatePointerCast(ptr, builder.getInt8PtrTy());
    return PushReturn(builder.CreateCall(wait_func, { ptr, expected, count }));
  });
}

constexpr auto SeqCst = llvm::AtomicOrdering::SequentiallyConsistent;

IN_ERROR Compiler::CompileAtomicFence(const char* name)
{
  auto fence = builder.CreateFence(SeqCst);
  fence->setName(name);

  return ERR_SUCCESS;
}

IN_ERROR Compiler::CompileAtomicLoad(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name)
{
  return CompileAtomicMemInstruction(ins, [&](unsigned align, llvmVal* ptr) {
    auto varsize = unsigned(4 * -varTy); // TE_i32(-1) => 4; TE_i64(-2) => 8;
    auto varty   = builder.getIntNTy(varsize * 8);
    
    auto load = builder.CreateAlignedLoad(ptr->getType()->getPointerElementType(), ptr, llvm::Align::Align(align), name);
    load->setAtomic(SeqCst);

    llvmVal* result = load;
    // All atomic loads zero-ext when the atomic variable is smaller than the WASM value
    if(varsize > align)
      result = builder.CreateZExt(result, varty);

    return PushReturn(result);
  });
}

IN_ERROR Compiler::CompileAtomicStore(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name)
{
  return CompileAtomicMemInstruction(ins, varTy, [&](unsigned align, llvmVal* ptr, llvmVal* value) {
    auto varsize = unsigned(4 * -varTy); // TE_i32(-1) => 4; TE_i64(-2) => 8;
    auto memty   = builder.getIntNTy(align * 8);

    // We need to truncate first when the atomic field is smaller than the WASM value
    if(varsize > align)
      value = builder.CreateTrunc(value, memty);

    auto store = builder.CreateAlignedStore(value, ptr, llvm::Align::Align(align));
    store->setAtomic(SeqCst);

    return ERR_SUCCESS;
  });
}

IN_ERROR Compiler::CompileAtomicRMW(Instruction& ins, WASM_TYPE_ENCODING varTy, llvm::AtomicRMWInst::BinOp Op,
                                    const char* name)
{
  return CompileAtomicMemInstruction(ins, varTy, [&](unsigned align, llvmVal* ptr, llvmVal* newValue) {
    auto varsize = unsigned(4 * -varTy); // TE_i32(-1) => 4; TE_i64(-2) => 8;
    auto varty   = builder.getIntNTy(varsize * 8);
    auto memty   = builder.getIntNTy(align * 8);

    if(varsize > align)
      newValue = builder.CreateTrunc(newValue, memty);

    auto rmw = builder.CreateAtomicRMW(Op, ptr, newValue, llvm::Align::Align(align), SeqCst);
    rmw->setName(name);

    llvmVal* oldValue = rmw;
    if(varsize > align)
      oldValue = builder.CreateZExt(oldValue, varty);

    return PushReturn(oldValue);
  });
}

IN_ERROR Compiler::CompileAtomicCmpXchg(Instruction& ins, WASM_TYPE_ENCODING varTy, const char* name)
{
  return CompileAtomicMemInstruction(ins, varTy, varTy, [&](unsigned align, llvmVal* ptr, llvmVal* cmp, llvmVal* newVal) {
    auto varsize = unsigned(4 * -varTy); // TE_i32(-1) => 4; TE_i64(-2) => 8;
    auto varty   = builder.getIntNTy(varsize * 8);
    auto memty   = builder.getIntNTy(align * 8);

    if(varsize > align)
    {
      cmp    = builder.CreateTrunc(cmp, memty);
      newVal = builder.CreateTrunc(newVal, memty);
    }

    auto cmpxchg = builder.CreateAtomicCmpXchg(ptr, cmp, newVal, llvm::Align::Align(align), SeqCst, SeqCst);
    cmpxchg->setName(name);

    llvmVal* loaded = builder.CreateExtractValue(cmpxchg, 0);
    if(varsize > align)
      loaded = builder.CreateZExt(loaded, varty);

    return PushReturn(loaded);
  });
}

IN_ERROR Compiler::CompileAtomicInstruction(Instruction& ins)
{
  using RmwOp = llvm::AtomicRMWInst::BinOp;
  auto op     = ins.opcode[1];

  switch(op)
  {
  case OP_atomic_notify: return CompileAtomicNotify(ins, OP::NAMES[ins.opcode]);
  case OP_atomic_wait32: return CompileAtomicWait(ins, TE_i32, atomic_wait32, OP::NAMES[ins.opcode]);
  case OP_atomic_wait64: return CompileAtomicWait(ins, TE_i64, atomic_wait64, OP::NAMES[ins.opcode]);

  case OP_atomic_fence: return CompileAtomicFence(OP::NAMES[ins.opcode]);

  default: break; // clever decoding because my god that switch block was unweildy
  }

  if(op < OP_START || op >= OP_END)
    return LogErrorString(env, "%s: %hhi is not a recognized webassembly instruction.", ERR_FATAL_UNKNOWN_INSTRUCTION, op);

  auto opGroup = GetOpGroup(op);
  auto varTy   = GetOpTy(op);

  switch(opGroup)
  {
  case OpGroup::Load: return CompileAtomicLoad(ins, varTy, OP::NAMES[ins.opcode]);
  case OpGroup::Store: return CompileAtomicStore(ins, varTy, OP::NAMES[ins.opcode]);
  case OpGroup::Add: return CompileAtomicRMW(ins, varTy, RmwOp::Add, OP::NAMES[ins.opcode]);
  case OpGroup::Sub: return CompileAtomicRMW(ins, varTy, RmwOp::Sub, OP::NAMES[ins.opcode]);
  case OpGroup::And: return CompileAtomicRMW(ins, varTy, RmwOp::And, OP::NAMES[ins.opcode]);
  case OpGroup::Or: return CompileAtomicRMW(ins, varTy, RmwOp::Or, OP::NAMES[ins.opcode]);
  case OpGroup::Xor: return CompileAtomicRMW(ins, varTy, RmwOp::Xor, OP::NAMES[ins.opcode]);
  case OpGroup::Xchg: return CompileAtomicRMW(ins, varTy, RmwOp::Xchg, OP::NAMES[ins.opcode]);
  case OpGroup::CmpXchg: return CompileAtomicCmpXchg(ins, varTy, OP::NAMES[ins.opcode]);

  default:
    // This should be unreachable, the static_asserts enforce the invariants
    assert(false);
    return ERR_SUCCESS; // shut up compiler lol
  }
}
