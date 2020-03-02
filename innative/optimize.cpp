// Copyright (c)2020 Black Sphere Studios
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
#pragma warning(pop)

using namespace innative;

IN_ERROR innative::OptimizeModules(const Environment* env)
{
  llvm::PassBuilder passBuilder;
  llvm::LoopAnalysisManager loopAnalysisManager(env->loglevel >= LOG_DEBUG);
  llvm::FunctionAnalysisManager functionAnalysisManager(env->loglevel >= LOG_DEBUG);
  llvm::CGSCCAnalysisManager cGSCCAnalysisManager(env->loglevel >= LOG_DEBUG);
  llvm::ModuleAnalysisManager moduleAnalysisManager(env->loglevel >= LOG_DEBUG);

  // Pass debugging
  /*llvm::PassInstrumentationCallbacks PIC;
  moduleAnalysisManager.registerPass([&]() {return llvm::PassInstrumentationAnalysis(&PIC); });
  functionAnalysisManager.registerPass([&]() { return llvm::PassInstrumentationAnalysis(&PIC); });
  loopAnalysisManager.registerPass([&]() { return llvm::PassInstrumentationAnalysis(&PIC); });
  cGSCCAnalysisManager.registerPass([&]() { return llvm::PassInstrumentationAnalysis(&PIC); });

  FILE* aux = fopen("passes.txt", "wb");
  int counter = 0;
  PIC.registerAfterPassCallback([&](const llvm::StringRef& name, const llvm::Any&) {
    if(name.contains_lower("PassManager") && env->n_modules > 1)
    {
      std::error_code EC;
      llvm::raw_fd_ostream dest(std::string(env->modules[1].cache->llvm->getName()) + "_" + std::to_string(++counter) +
  ".llvm", EC, llvm::sys::fs::OpenFlags::OF_None); env->modules[1].cache->llvm->print(dest, nullptr);
    }
    fprintf(aux, "%i: %s\n", counter, name.begin());
    });*/

  passBuilder.registerModuleAnalyses(moduleAnalysisManager);
  passBuilder.registerCGSCCAnalyses(cGSCCAnalysisManager);
  passBuilder.registerFunctionAnalyses(functionAnalysisManager);
  passBuilder.registerLoopAnalyses(loopAnalysisManager);
  passBuilder.crossRegisterProxies(loopAnalysisManager, functionAnalysisManager, cGSCCAnalysisManager,
                                   moduleAnalysisManager);

  llvm::PassBuilder::OptimizationLevel optlevel = llvm::PassBuilder::OptimizationLevel::O0;

  switch(env->optimize & ENV_OPTIMIZE_OMASK)
  {
  case ENV_OPTIMIZE_O1: optlevel = llvm::PassBuilder::OptimizationLevel::O1; break;
  case ENV_OPTIMIZE_O2: optlevel = llvm::PassBuilder::OptimizationLevel::O2; break;
  case ENV_OPTIMIZE_O3: optlevel = llvm::PassBuilder::OptimizationLevel::O3; break;
  case ENV_OPTIMIZE_Os: optlevel = llvm::PassBuilder::OptimizationLevel::Os; break;
  default: assert(false);
  }

  llvm::ModulePassManager modulePassManager =
    passBuilder.buildPerModuleDefaultPipeline(optlevel, env->loglevel >= LOG_DEBUG);

  // Optimize all modules
  for(size_t i = 0; i < env->n_modules; ++i)
    modulePassManager.run(*env->modules[i].cache->mod, moduleAnalysisManager);

  /*{
    auto manager = llvm::make_unique<llvm::legacy::FunctionPassManager>(context[i].llvm);

    manager->add(llvm::createDependenceAnalysisWrapperPass()); // (used by loop - unroll - and-jam)
    manager->add(llvm::createAAResultsWrapperPass()); // (used by LICM and argpromotion)
    manager->add(llvm::createGlobalsAAWrapperPass());
    manager->add(llvm::createSCEVAAWrapperPass());
    manager->add(new llvm::MemoryDependenceWrapperPass());
    manager->add(new llvm::ScalarEvolutionWrapperPass()); // (used by strength reduction)
    manager->add(new llvm::DominanceFrontierWrapperPass()); // (used by mem2reg)
    manager->add(llvm::createPostDomTree());

    manager->add(llvm::createSROAPass());
    manager->add(llvm::createReassociatePass());
    manager->add(llvm::createConstantMergePass());
    manager->add(llvm::createConstantPropagationPass());
    manager->add(llvm::createPostOrderFunctionAttrsLegacyPass());
    manager->add(llvm::createFunctionInliningPass());
    manager->add(llvm::createInstructionCombiningPass());
    manager->add(llvm::createGVNPass());
    manager->add(llvm::createStraightLineStrengthReducePass());
    manager->add(llvm::createJumpThreadingPass());
    manager->add(llvm::createLoadStoreVectorizerPass());
    manager->add(llvm::createSLPVectorizerPass());

    manager->add(llvm::createLoopSimplifyCFGPass());
    manager->add(llvm::createLoopSimplifyPass());
    manager->add(llvm::createLCSSAPass());
    manager->add(llvm::createLICMPass());
    manager->add(llvm::createIndVarSimplifyPass());
    manager->add(llvm::createLoopUnrollPass());
    manager->add(llvm::createLoopUnrollAndJamPass());
    manager->add(llvm::createLoopUnswitchPass());
    manager->add(llvm::createLoopStrengthReducePass());
    manager->add(llvm::createLoopVectorizePass());
    manager->add(llvm::createLoopDeletionPass());

    manager->add(llvm::createAggressiveInstCombinerPass());
    manager->add(llvm::createLoopSinkPass());
    manager->add(llvm::createSinkingPass());
    manager->add(llvm::createPromoteMemoryToRegisterPass());
    manager->add(llvm::createMergeICmpsPass());

    manager->add(llvm::createSCCPPass());
    manager->add(llvm::createIPConstantPropagationPass());
    manager->add(llvm::createIPSCCPPass());
    manager->add(llvm::createDeadInstEliminationPass());
    manager->add(llvm::createDeadStoreEliminationPass());
    manager->add(llvm::createDeadArgEliminationPass());
    manager->add(llvm::createDeadCodeEliminationPass());
    manager->add(llvm::createGlobalDCEPass());

    manager->add(llvm::createMergedLoadStoreMotionPass());
    manager->add(llvm::createPostOrderFunctionAttrsLegacyPass());
    manager->add(llvm::createGlobalOptimizerPass());
    manager->add(llvm::createCFGSimplificationPass());

    manager->add(llvm::createTailCallEliminationPass());
    manager->add(llvm::createPruneEHPass());

    if(!(env->flags&ENV_DEBUG))
    {
      manager->add(llvm::createStripDeadDebugInfoPass());
      manager->add(llvm::createStripDeadPrototypesPass());
      manager->add(llvm::createStripNonDebugSymbolsPass());
    }

    manager->doInitialization();
  }*/

  // fclose(aux);
  return ERR_SUCCESS;
}