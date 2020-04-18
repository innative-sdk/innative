// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_dwarf.h"
#include "compile.h"
#include "utility.h"
#include <algorithm>

using namespace innative;
using namespace utility;
using namespace llvm::dwarf;

DebugDWARF::DebugDWARF(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath) :
  DebugSourceMap(s, compiler, m, name, filepath, ENV_DEBUG_DWARF)
{}

void DebugDWARF::PostFuncBody(llvm::Function* fn, FunctionBody& body)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(_compiler->globals.size() > 0)
  {
    _dbuilder->insertDeclare(
      _compiler->memlocal,
      _dbuilder->createAutoVariable(_curscope, "MEMLOCAL", GetSourceFile(f->source_index), 0,
                                    _dbuilder->createPointerType(diI32, _compiler->intptrty->getBitWidth()), true),
      _dbuilder->createExpression(), _compiler->builder.getCurrentDebugLocation(), _compiler->builder.GetInsertBlock());
  }
}

enum
{
  DW_OP_WASM_location = 0xEB,
};

enum DW_WASM_OP
{
  DW_WASM_OP_LOCAL  = 0x00,
  DW_WASM_OP_GLOBAL = 0x01,
  DW_WASM_OP_STACK  = 0x02,
};

llvm::DIType* DebugDWARF::StructOffsetType(llvm::DIType* ty, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                           uint64_t indice, unsigned int bitsize, unsigned int bytealign)
{
  auto member = _dbuilder->createMemberType(scope, name, file, 0, ty->getSizeInBits(), 1,
                                            (indice + 0x00000000000010008) << 3, llvm::DINode::FlagZero, ty);
  auto offset = _dbuilder->createStructType(scope, "p_" + name.str(), file, 0, ty->getSizeInBits(), 1,
                                            llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));
  return _dbuilder->createPointerType(offset, bitsize, bytealign);
}

void DebugDWARF::UpdateVariables(llvm::Function* fn, SourceMapScope& scope)
{
  int params = 0;

  for(size_t i = 0; i < scope.n_variables; ++i)
  {
    assert(scope.variables[i] < sourcemap->n_innative_variables);
    auto& v            = sourcemap->x_innative_variables[scope.variables[i]];
    const char* name   = v.name_index < sourcemap->n_names ? sourcemap->names[v.name_index] : "";
    llvm::DIFile* file = GetSourceFile(v.source_index);
    auto ty            = GetDebugType(v.type_index);

    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
      ty = StructOffsetType(ty, file, file, name, v.p_expr[1], _compiler->intptrty->getBitWidth(), 1);

    llvm::DILocalVariable* dparam;
    if(v.tag == DW_TAG_formal_parameter || v.tag == DW_TAG_unspecified_parameters)
      dparam = _dbuilder->createParameterVariable(_curscope, name, ++params, file, v.original_line, ty, true);
    else
      dparam = _dbuilder->createAutoVariable(_curscope, name, file, v.original_line, ty, true);

    auto expr = _dbuilder->createExpression();
    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
    {
      _dbuilder->insertDeclare(_compiler->memlocal, dparam, expr,
                               llvm::DILocation::get(_compiler->ctx, v.original_line, v.original_column, _curscope),
                               _compiler->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_LOCAL)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) < _compiler->locals.size())
        _dbuilder->insertDbgValueIntrinsic(_compiler->locals[static_cast<size_t>(v.p_expr[2])], dparam, expr,
                                           llvm::DILocation::get(_compiler->ctx, v.original_line, v.original_column,
                                                                 _curscope),
                                           _compiler->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_GLOBAL)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) < _compiler->globals.size())
        _dbuilder->insertDbgValueIntrinsic(_compiler->globals[static_cast<size_t>(v.p_expr[2])], dparam, expr,
                                           llvm::DILocation::get(_compiler->ctx, v.original_line, v.original_column,
                                                                 _curscope),
                                           _compiler->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_STACK)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) <
         _compiler->values
           .Size()) // TODO: we're going from the bottom of the limit, but it may be the bottom of the entire values stack
        _dbuilder->insertDbgValueIntrinsic(_compiler->values.Get()[_compiler->values.Limit() + v.p_expr[2]], dparam, expr,
                                           llvm::DILocation::get(_compiler->ctx, v.original_line, v.original_column,
                                                                 _curscope),
                                           _compiler->builder.GetInsertBlock());
    }
    else
      _dbuilder->insertDbgValueIntrinsic(_compiler->memlocal, dparam, expr,
                                         llvm::DILocation::get(_compiler->ctx, v.original_line, v.original_column,
                                                               _curscope),
                                         _compiler->builder.GetInsertBlock());
  }

  {
    auto dparam = _dbuilder->createAutoVariable(
      _curscope, "scopeoffset", _curscope->getFile(), 0,
      StructOffsetType(diI32, dunit, dunit, "scopeoffset", 0, _compiler->intptrty->getBitWidth(), 1), true);

    _dbuilder->insertDeclare(_compiler->memlocal, dparam, _dbuilder->createExpression(),
                             llvm::DILocation::get(_compiler->ctx, 0, 0, _curscope), _compiler->builder.GetInsertBlock());
  }
}

llvm::DIType* DebugDWARF::GetDebugType(size_t index, llvm::DIType* parent)
{
  if(index == (size_t)~0)
    return diVoid;
  auto& type = sourcemap->x_innative_types[index];

  switch(type.tag)
  {
  case DW_TAG_rvalue_reference_type:
  case DW_TAG_reference_type:
  case DW_TAG_pointer_type: break;
  default: return DebugSourceMap::GetDebugType(index, parent);
  }

  if(index < types.size() && !types[index])
  {
    const char* name   = type.name_index < sourcemap->n_names ? sourcemap->names[type.name_index] : 0;
    llvm::DIFile* file = GetSourceFile(type.source_index);

    switch(type.tag)
    {
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
    case DW_TAG_pointer_type:
      if(auto ty = GetDebugType(type.type_index))
      {
        if(!name)
          name = ty->getName().str().c_str();
        auto pname = std::string("p<") + name;
        if(pname.back() == '*' || pname.back() == '&') // strip one pointer indirection layer off
          pname.pop_back();

        auto member  = _dbuilder->createMemberType(file, name, file, 0, ty->getSizeInBits(), 0, 0x00000000000010008 << 3,
                                                  llvm::DINode::FlagZero, ty);
        types[index] = _dbuilder->createStructType(file, pname + ">", file, 0, ty->getSizeInBits(), 0,
                                                   llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));
      }
      break;
    }
  }

  return types[index];
}
