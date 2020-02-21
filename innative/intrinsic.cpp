// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "intrinsic.h"
#include "compile.h"

using namespace innative;

namespace innative {
  namespace code {
    __KHASH_IMPL(importhash, , const char*, llvm::GlobalObject*, 1, kh_str_hash_func, kh_str_hash_equal);
  }
}

IN_ERROR innative::code::IN_Intrinsic_ToC(code::Context& context, llvm::Value** params, llvm::Value*& out)
{
  if(!context.memories.size())
    return ERR_INVALID_MEMORY_INDEX;

  out = context.builder.CreateAdd(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.GetPairPtr(context.memories[0], 0)),
                                                                 context.builder.getInt64Ty()),
                                  params[0], "", true, true);
  return ERR_SUCCESS;
}

IN_ERROR innative::code::IN_Intrinsic_FromC(code::Context& context, llvm::Value** params, llvm::Value*& out)
{
  if(!context.memories.size())
    return ERR_INVALID_MEMORY_INDEX;

  out = context.builder.CreateSub(
    context.builder.CreatePtrToInt(context.builder.CreateLoad(context.GetPairPtr(context.memories[0], 0)),
                                                                 context.builder.getInt64Ty()),
                                  params[0], "", true, true);
  return ERR_SUCCESS;
}

IN_ERROR innative::code::IN_Intrinsic_Trap(code::Context& context, llvm::Value** params, llvm::Value*& out)
{
  auto call = context.builder.CreateCall(llvm::Intrinsic::getDeclaration(context.llvm, llvm::Intrinsic::trap), {});
  call->setDoesNotReturn();
  out = nullptr;
  return ERR_SUCCESS;
}

IN_ERROR innative::code::IN_Intrinsic_FuncPtr(code::Context& context, llvm::Value** params, llvm::Value*& out)
{
  if(!params[0]->getType()->isIntegerTy())
    return ERR_INVALID_ARGUMENT_TYPE;

  InsertConditionalTrap(
    context.builder.CreateICmpUGE(context.builder.CreateIntCast(params[0], context.builder.getInt64Ty(), false),
                                  context.builder.getInt64(
                                    context.exported_functions->getType()->getElementType()->getArrayNumElements())),
    context);

  // Deference global variable to get the actual array of function pointers, index into them, then dereference that array
  // index to get the actual function pointer
  llvm::Value* funcptr = context.builder.CreateLoad(
    context.builder.CreateInBoundsGEP(context.exported_functions, { context.builder.getInt32(0), params[0] }));

  out = context.builder.CreatePtrToInt(funcptr, context.builder.getInt64Ty());
  return ERR_SUCCESS;
}