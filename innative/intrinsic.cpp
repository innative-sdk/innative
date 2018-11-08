// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "intrinsic.h"

using namespace innative;

namespace innative {
  namespace code {
    __KHASH_IMPL(importhash, , const char*, llvm::Function*, 1, kh_str_hash_func, kh_str_hash_equal);
  }
}

llvm::Function* innative::code::IR_Intrinsic_ToC(llvm::Function* f, struct code::Context& context)
{
  if(!f)
  {
    f = llvm::Function::Create(
      llvm::FunctionType::get(context.builder.getInt64Ty(), { context.builder.getInt64Ty() }, false),
      llvm::Function::InternalLinkage,
      "innative-intrinsic:to_c",
      context.llvm);
    f->setCallingConv(llvm::CallingConv::Fast);
    return f;
  }

  assert(context.memories.size() > 0);
  if(context.memories.size() > 0)
  {
    llvm::BasicBlock* bb = llvm::BasicBlock::Create(context.context, "to_block", f);
    context.builder.SetInsertPoint(bb);
    context.builder.CreateRet(context.builder.CreateAdd(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.memories[0]), context.builder.getInt64Ty()), f->args().begin(), "", true, true));
  }
  return f;
}

llvm::Function* innative::code::IR_Intrinsic_FromC(llvm::Function* f, struct code::Context& context)
{
  if(!f)
  {
    f = llvm::Function::Create(
      llvm::FunctionType::get(context.builder.getInt64Ty(), { context.builder.getInt64Ty() }, false),
      llvm::Function::InternalLinkage,
      "innative-intrinsic:from_c",
      context.llvm);
    f->setCallingConv(llvm::CallingConv::Fast);
    return f;
  }

  assert(context.memories.size() > 0);
  if(context.memories.size() > 0)
  {
    llvm::BasicBlock* bb = llvm::BasicBlock::Create(context.context, "from_block", f);
    context.builder.SetInsertPoint(bb);
    context.builder.CreateRet(context.builder.CreateSub(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.memories[0]), context.builder.getInt64Ty()), f->args().begin(), "", true, true));
  }
  return f;
}