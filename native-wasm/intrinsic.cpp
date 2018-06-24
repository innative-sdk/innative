// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "intrinsic.h"

llvm::Function* NW_Intrinsic_ToC(llvm::Function* f, struct NWContext& context)
{
  if(!f)
  {
    f = llvm::Function::Create(
      llvm::FunctionType::get(context.builder.getInt64Ty(), { context.builder.getInt64Ty() }, false),
      llvm::Function::InternalLinkage,
      "nw-intrinsic:to_c",
      context.llvm);
    f->setCallingConv(llvm::CallingConv::Fast);
    return f;
  }

  assert(context.n_memory > 0);
  if(context.n_memory > 0)
  {
    llvm::BasicBlock* bb = llvm::BasicBlock::Create(context.context, "to_block", f);
    context.builder.SetInsertPoint(bb);
    context.builder.CreateRet(context.builder.CreateAdd(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.linearmemory[0]), context.builder.getInt64Ty()), f->args().begin(), "", true, true));
  }
  return f;
}

llvm::Function* NW_Intrinsic_FromC(llvm::Function* f, struct NWContext& context)
{
  if(!f)
  {
    f = llvm::Function::Create(
      llvm::FunctionType::get(context.builder.getInt64Ty(), { context.builder.getInt64Ty() }, false),
      llvm::Function::InternalLinkage,
      "nw-intrinsic:from_c",
      context.llvm);
    f->setCallingConv(llvm::CallingConv::Fast);
    return f;
  }

  assert(context.n_memory > 0);
  if(context.n_memory > 0)
  {
    llvm::BasicBlock* bb = llvm::BasicBlock::Create(context.context, "from_block", f);
    context.builder.SetInsertPoint(bb);
    context.builder.CreateRet(context.builder.CreateSub(context.builder.CreatePtrToInt(context.builder.CreateLoad(context.linearmemory[0]), context.builder.getInt64Ty()), f->args().begin(), "", true, true));
  }
  return f;
}