// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "compile.h"

using namespace innative;

const Compiler::Intrinsic Compiler::intrinsics[] = {
  { "_innative_to_c", &Compiler::IN_Intrinsic_ToC, { TE_i64 }, 1 },
  { "_innative_from_c", &Compiler::IN_Intrinsic_FromC, { TE_i64 }, 1 },
  { "_innative_trap", &Compiler::IN_Intrinsic_Trap, {}, 0 },
  { "_innative_funcptr", &Compiler::IN_Intrinsic_FuncPtr, { TE_i32 }, 1 },
};

IN_ERROR Compiler::IN_Intrinsic_ToC(llvm::Value** params, llvm::Value*& out)
{
  if(!memories.size())
    return ERR_INVALID_MEMORY_INDEX;

  out = builder.CreateAdd(builder.CreatePtrToInt(builder.CreateLoad(GetPairPtr(memories[0], 0)), builder.getInt64Ty()),
                          params[0], "", true, true);
  return ERR_SUCCESS;
}

IN_ERROR Compiler::IN_Intrinsic_FromC(llvm::Value** params, llvm::Value*& out)
{
  if(!memories.size())
    return ERR_INVALID_MEMORY_INDEX;

  out = builder.CreateSub(builder.CreatePtrToInt(builder.CreateLoad(GetPairPtr(memories[0], 0)), builder.getInt64Ty()),
                          params[0], "", true, true);
  return ERR_SUCCESS;
}

IN_ERROR Compiler::IN_Intrinsic_Trap(llvm::Value** params, llvm::Value*& out)
{
  auto call = builder.CreateCall(llvm::Intrinsic::getDeclaration(mod, llvm::Intrinsic::trap), {});
  call->setDoesNotReturn();
  out = nullptr;
  return ERR_SUCCESS;
}

IN_ERROR Compiler::IN_Intrinsic_FuncPtr(llvm::Value** params, llvm::Value*& out)
{
  if(!params[0]->getType()->isIntegerTy())
    return ERR_INVALID_ARGUMENT_TYPE;

  InsertConditionalTrap(
    builder.CreateICmpUGE(builder.CreateIntCast(params[0], builder.getInt64Ty(), false),
                          builder.getInt64(exported_functions->getType()->getElementType()->getArrayNumElements())));

  // Deference global variable to get the actual array of function pointers, index into them, then dereference that array
  // index to get the actual function pointer
  llvm::Value* funcptr =
    builder.CreateLoad(builder.CreateInBoundsGEP(exported_functions, { builder.getInt32(0), params[0] }));

  out = builder.CreatePtrToInt(funcptr, builder.getInt64Ty());
  return ERR_SUCCESS;
}