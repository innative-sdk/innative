// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_wat.h"
#include "compile.h"

using namespace innative;
using namespace code;

DebugWat::DebugWat(llvm::IntegerType* intptr, llvm::Module& m, const char* name, const Environment* env,
                   const char* filepath) :
  Debugger(intptr, m, name, env, filepath)
{}

void DebugWat::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
{
  FunctionDebugInfo(fn, fn->getName(), optimized, true, false, dunit, line, 0);
}

void DebugWat::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body, code::Context& context)
{
  auto name   = std::string(!desc.debug.name.size() ? "func#" + std::to_string(indice) : desc.debug.name.str());
  auto line   = body.line;
  auto column = body.column;

  FunctionDebugInfo(fn, name + "|" + context.m.name.str(), context.env.optimize != 0, true, false, dunit, line, column);
  context.builder.SetCurrentDebugLocation(llvm::DILocation::get(context.context, line, column, fn->getSubprogram()));
}

void DebugWat::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc, code::Context& context)
{
  llvm::DILocation* loc = context.builder.getCurrentDebugLocation();
  if(desc.param_debug && desc.param_debug[index].line > 0)
    loc = llvm::DILocation::get(context.context, desc.param_debug[index].line, desc.param_debug[index].column,
                                fn->getSubprogram());

  llvm::DILocalVariable* dparam =
    dbuilder->createParameterVariable(fn->getSubprogram(), context.locals.back()->getName(),
                                      index + 1, // the arg index starts at 1
                                      loc->getFile(), loc->getLine(),
                                      CreateDebugType(context.locals.back()->getAllocatedType()), true);

  dbuilder->insertDeclare(context.locals.back(), dparam, dbuilder->createExpression(), loc,
                          context.builder.GetInsertBlock());
}

void DebugWat::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context)
{
  llvm::DILocation* loc = context.builder.getCurrentDebugLocation();

  llvm::DILocalVariable* dparam =
    dbuilder->createAutoVariable(fn->getSubprogram(), context.locals.back()->getName(), loc->getFile(), loc->getLine(),
                                 CreateDebugType(context.locals.back()->getAllocatedType()), true);

  dbuilder->insertDeclare(context.locals.back(), dparam, dbuilder->createExpression(), loc,
                          context.builder.GetInsertBlock());
}

void DebugWat::DebugIns(llvm::Function* fn, Instruction& i, code::Context& context)
{
  context.builder.SetCurrentDebugLocation(
    llvm::DILocation::get(context.context, i.line, i.column, curscope));
}

void DebugWat::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line)
{
  auto expr = dbuilder->createExpression(llvm::SmallVector<uint64_t, 1>{ llvm::dwarf::DW_OP_deref });

  v->addDebugInfo(dbuilder->createGlobalVariableExpression(dcu, name, v->getName(), dunit, (unsigned int)line,
                                                           CreateDebugType(v->getType()), !v->hasValidDeclarationLinkage(),
                                                           expr));
}

void DebugWat::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc)
{
  curscope = dbuilder->createLexicalBlock(scope, loc->getFile(), loc.getLine(), loc.getCol());
  scopes.Push(curscope);
}

void DebugWat::PopBlock()
{
  scopes.Pop();
  curscope = scopes.Size() > 0 ? scopes.Peek() : nullptr;
}