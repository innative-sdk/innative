// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_wat.h"
#include "compile.h"

using namespace innative;

DebugWat::DebugWat(Compiler* compiler, llvm::Module& m, const char* name, const char* filepath) :
  Debugger(compiler, m, name, filepath, compiler->env.flags & ENV_DEBUG)
{}

void DebugWat::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
{
  FunctionDebugInfo(fn, fn->getName(), optimized, true, false, dunit, line, 0);
}

void DebugWat::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body)
{
  auto name   = std::string(!desc.debug.name.size() ? "func#" + std::to_string(indice) : desc.debug.name.str());
  auto line   = body.line;
  auto column = body.column;

  FunctionDebugInfo(fn, name + "|" + _compiler->m.name.str(), _compiler->env.optimize != 0, true, false, dunit, line, column);
  _compiler->builder.SetCurrentDebugLocation(llvm::DILocation::get(_compiler->ctx, line, column, fn->getSubprogram()));
}

void DebugWat::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc)
{
  llvm::DILocation* loc = _compiler->builder.getCurrentDebugLocation();
  if(desc.param_debug && desc.param_debug[index].line > 0)
    loc = llvm::DILocation::get(_compiler->ctx, desc.param_debug[index].line, desc.param_debug[index].column,
                                fn->getSubprogram());

  llvm::DILocalVariable* dparam =
    _dbuilder->createParameterVariable(fn->getSubprogram(), _compiler->locals.back()->getName(),
                                       static_cast<unsigned int>(index + 1), // the arg index starts at 1
                                       loc->getFile(), loc->getLine(),
                                       CreateDebugType(_compiler->locals.back()->getAllocatedType()), true);

  _dbuilder->insertDeclare(_compiler->locals.back(), dparam, _dbuilder->createExpression(), loc,
                           _compiler->builder.GetInsertBlock());
}

void DebugWat::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc)
{
  llvm::DILocation* loc = _compiler->builder.getCurrentDebugLocation();

  llvm::DILocalVariable* dparam =
    _dbuilder->createAutoVariable(fn->getSubprogram(), _compiler->locals.back()->getName(), loc->getFile(), loc->getLine(),
                                  CreateDebugType(_compiler->locals.back()->getAllocatedType()), true);

  _dbuilder->insertDeclare(_compiler->locals.back(), dparam, _dbuilder->createExpression(), loc,
                           _compiler->builder.GetInsertBlock());
}

void DebugWat::DebugIns(llvm::Function* fn, Instruction& i)
{
  _compiler->builder.SetCurrentDebugLocation(llvm::DILocation::get(_compiler->ctx, i.line, i.column, _curscope));
}

void DebugWat::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line)
{
  auto expr = _dbuilder->createExpression(llvm::SmallVector<uint64_t, 1>{ llvm::dwarf::DW_OP_deref });

  v->addDebugInfo(_dbuilder->createGlobalVariableExpression(dcu, name, v->getName(), dunit, static_cast<unsigned int>(line),
                                                            CreateDebugType(v->getType()), !v->hasValidDeclarationLinkage(),
                                                            expr));
}

void DebugWat::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc)
{
  _curscope = _dbuilder->createLexicalBlock(scope, loc->getFile(), loc.getLine(), loc.getCol());
  scopes.Push(_curscope);
}

void DebugWat::PopBlock()
{
  scopes.Pop();
  _curscope = scopes.Size() > 0 ? scopes.Peek() : nullptr;
}