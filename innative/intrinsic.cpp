// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "intrinsic.h"

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

  out = context.builder.CreateAdd(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.memories[0]),
                                                                 context.builder.getInt64Ty()),
                                  params[0], "", true, true);
  return ERR_SUCCESS;
}

IN_ERROR innative::code::IN_Intrinsic_FromC(code::Context& context, llvm::Value** params, llvm::Value*& out)
{
  if(!context.memories.size())
    return ERR_INVALID_MEMORY_INDEX;

  out = context.builder.CreateSub(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.memories[0]),
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
  auto v = llvm::dyn_cast<llvm::ConstantInt, llvm::Value>(params[0]);
  if(!v)
    return ERR_INVALID_ARGUMENT_TYPE;
  uint64_t index = v->getValue().getLimitedValue();
  if(index >= context.functions.size())
    return ERR_INVALID_ARGUMENT_TYPE;
  llvm::Function* fn = (!context.functions[(size_t)index].imported) ? (context.functions[(size_t)index].internal) :
                                                                      context.functions[(size_t)index].imported;
  out = context.builder.CreatePtrToInt(fn, context.builder.getInt64Ty());
  return ERR_SUCCESS;
}