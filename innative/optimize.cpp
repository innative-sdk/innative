// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "optimize.h"
#include "compile.h"
#pragma warning(push)
#pragma warning(disable : 4146 4267 4141 4244 4624)
#define _SCL_SECURE_NO_WARNINGS
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/DependenceAnalysis.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "polly/RegisterPasses.h"
#pragma warning(pop)

using namespace innative;

IN_ERROR innative::OptimizeModules(const Environment* env, llvm::TargetMachine* target)
{
  llvm::PassManagerBuilder builder;
  builder.DisableTailCalls = (env->optimize & ENV_DISABLE_TAIL_CALL) != 0;
  builder.OptLevel         = 0;
  builder.SizeLevel        = 0;
  builder.NewGVN           = true;
  builder.VerifyInput      = true;
  builder.VerifyOutput     = true;
  builder.PrepareForLTO    = false; // TODO: Maybe introduce LTO optimizations at some point?
  builder.LibraryInfo      = new llvm::TargetLibraryInfoImpl(target->getTargetTriple());
  
  switch(env->optimize & ENV_OPTIMIZE_OMASK)
  {
  case ENV_OPTIMIZE_O1: builder.OptLevel = 1; break;
  case ENV_OPTIMIZE_O2: builder.OptLevel = 2; break;
  case ENV_OPTIMIZE_O3: builder.OptLevel = 3; break;
  case ENV_OPTIMIZE_Os: builder.SizeLevel = 1;
  default: assert(false);
  }

  // LLVM points out that this would be useful even at O0, but damages debug info. 
  if(!(env->optimize & ENV_OPTIMIZE_NO_MERGING))
    builder.MergeFunctions = builder.OptLevel > 0;
  builder.Inliner        = llvm::createFunctionInliningPass(builder.OptLevel, builder.SizeLevel, false);
  
  target->adjustPassManager(builder);

  if(builder.OptLevel > 1)
  {
    // Allow all vectorization helpers at O2 and O3
    builder.SLPVectorize = true;
    builder.LoopVectorize = true;
    builder.LoopsInterleaved = true;
    builder.RerollLoops      = true;
  }

  llvm::legacy::PassManager mpm;
  mpm.add(llvm::createTargetTransformInfoWrapperPass(target->getTargetIRAnalysis()));
  builder.populateModulePassManager(mpm);
  //mpm.add(llvm::createVerifierPass()); // This may create false positives

  // Optimize all modules
  for(size_t i = 0; i < env->n_modules; ++i)
    mpm.run(*env->modules[i].cache->mod);

  return ERR_SUCCESS;
}