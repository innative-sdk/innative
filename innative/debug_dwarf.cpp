// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_dwarf.h"
#include "compile.h"
#include "util.h"
#include <algorithm>

using namespace innative;
using namespace utility;
using namespace code;
using namespace llvm::dwarf;

code::DebugDWARF::DebugDWARF(SourceMap* s, Context* context, llvm::Module& m, const char* name, const char* filepath) :
  DebugSourceMap(s, context, m, name, filepath)
{
}

llvm::DIFile* DebugDWARF::GetSourceFile(size_t i) { return (i < files.size()) ? files[i] : dunit; }

SourceMapFunction* DebugDWARF::GetSourceFunction(unsigned int offset)
{
  auto end             = sourcemap->x_innative_functions + sourcemap->n_innative_functions;
  SourceMapFunction* i = std::lower_bound(sourcemap->x_innative_functions, end, offset,
                                          [](const SourceMapFunction& f, unsigned int o) { return f.range.low < o; });
  return (i >= end || i->range.low != offset) ? nullptr : i;
}

void DebugDWARF::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
{
  // Use function low_PC to find the debug entry
  SourceMapFunction* f = GetSourceFunction(offset);

  if(!f)
  {
    FunctionDebugInfo(fn, fn->getName(), optimized, true, false, dunit, line, 0);
    return;
  }

  auto name = (f->scope.name_index < sourcemap->n_names) ? fn->getName() : sourcemap->names[f->scope.name_index];

  llvm::SmallVector<llvm::Metadata*, 8> dwarfTys = { GetDebugType(f->type_index) };
  for(unsigned int i = 0; i < f->scope.n_variables; ++i)
    if(sourcemap->x_innative_variables[f->scope.variables[i]].tag == DW_TAG_formal_parameter)
      dwarfTys.push_back(GetDebugType(sourcemap->x_innative_variables[f->scope.variables[i]].type_index));

  auto subtype =
    _dbuilder->createSubroutineType(_dbuilder->getOrCreateTypeArray(dwarfTys), llvm::DINode::FlagZero,
                                    (fn->getCallingConv() == llvm::CallingConv::C) ? DW_CC_normal : DW_CC_nocall);

  FunctionDebugInfo(fn, name, optimized, true, false, GetSourceFile(f->source_index), f->original_line, 0, subtype);
}

void DebugDWARF::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body)
{
  subprograms.resize(files.size());
  for(auto& i : subprograms)
    i = nullptr;

  curscopeindex = 0;
  if(sourcemap->n_innative_ranges > 0)
  {
    auto end   = sourcemap->x_innative_ranges + sourcemap->n_innative_ranges;
    auto range = std::lower_bound(sourcemap->x_innative_ranges, end, body.column,
                                  [](const SourceMapRange& s, unsigned int i) { return s.low < i; });
    if(range != end && range->scope < sourcemap->n_innative_scopes)
      curscopeindex = range->scope;
  }

  cursegment = 0;
  if(sourcemap->n_innative_ranges > 0)
  {
    auto end     = sourcemap->segments + sourcemap->n_segments;
    auto segment = std::lower_bound(sourcemap->segments, end, ((body.line - 1ULL) << 32) | body.column,
                                    [](const SourceMapSegment& s, uint64_t i) { return s.linecolumn < i; });
    cursegment   = segment - sourcemap->segments;
  }

  _context->builder.SetCurrentDebugLocation(
    llvm::DILocation::get(_context->context, fn->getSubprogram()->getLine(), 0, fn->getSubprogram()));

  // Unfortunately we can't actually gaurantee all the scopes will pop due to imprecise debug information
  while(scopes.Size() > 0)
    scopes.Pop();

  scopecache.resize(0); // Wipe the cache
  scopecache.resize(sourcemap->n_innative_scopes);
  _curscope = fn->getSubprogram();
  _curbody  = &body;
}

void DebugDWARF::PostFuncBody(llvm::Function* fn, FunctionBody& body)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(_context->globals.size() > 0)
  {
    _dbuilder->insertDeclare(_context->memlocal,
                             _dbuilder->createAutoVariable(_curscope, "MEMLOCAL", GetSourceFile(f->source_index), 0,
                                                           _dbuilder->createPointerType(diI32, _context->intptrty->getBitWidth()),
                                                           true),
                             _dbuilder->createExpression(), _context->builder.getCurrentDebugLocation(),
                             _context->builder.GetInsertBlock());
  }
}

void DebugDWARF::DebugSetGlobal(int index)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(!index)
  {
    stacklocal = _context->builder.CreateAlloca(_context->memories[0]->getType()->getElementType()->getContainedType(0),
                                              nullptr, "IN_!stacklocal");

    _dbuilder->insertDeclare(stacklocal,
                             _dbuilder->createAutoVariable(_curscope, "STACKLOCAL", GetSourceFile(f->source_index), 0,
                                                           CreateDebugType(stacklocal->getType()), true),
                             _dbuilder->createExpression(), llvm::DILocation::get(_context->context, 0, 0, _curscope),
                             _context->builder.GetInsertBlock());
    _context->builder.CreateStore(_context->builder.CreateIntToPtr(_context->builder.CreateLoad(_context->globals[0]),
                                                               _context->builder.getInt8PtrTy()),
                                stacklocal);

    // if(f)
    //  UpdateVariables(f->scope, context);
  }
}

void DebugDWARF::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc) {}

void DebugDWARF::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) {}

void DebugDWARF::UpdateLocation(Instruction& i)
{
  llvm::DILocation* loc = 0;

  while(cursegment < sourcemap->n_segments &&
        sourcemap->segments[cursegment].linecolumn < ((i.line - 1ULL) << 32 | i.column))
  {
    if(loc) // If we have a pending location, we need to create a nop instruction to hold it
      _context->builder.CreateIntrinsic(llvm::Intrinsic::donothing, {}, {})->setDebugLoc(loc);

    auto& s = sourcemap->segments[cursegment++];

    if(files[s.source_index] == _curscope->getFile() || s.source_index >= subprograms.size())
      loc = llvm::DILocation::get(_context->context, s.original_line, s.original_column, _curscope);
    else
    {
      if(!subprograms[s.source_index])
      {
        auto original = _curscope->getSubprogram();
        subprograms[s.source_index] =
          _dbuilder->createFunction(original->getScope(), original->getName(), original->getLinkageName(),
                                    files[s.source_index], original->getLine(), original->getType(),
                                    original->getScopeLine(), original->getFlags(), original->getSPFlags());
      }

      loc = _context->builder.getCurrentDebugLocation();
      if(loc->getInlinedAt())
        loc = loc->getInlinedAt();
      loc = llvm::DILocation::get(_context->context, s.original_line, s.original_column, subprograms[s.source_index], loc);
    }
  }

  if(loc)
    _context->builder.SetCurrentDebugLocation(loc);
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
  auto offset = _dbuilder->createStructType(scope, "WASM_" + name.str(), file, 0, ty->getSizeInBits(), 1,
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
      ty = StructOffsetType(ty, file, file, name, v.p_expr[1], _context->intptrty->getBitWidth(), 1);

    llvm::DILocalVariable* dparam;
    if(v.tag == DW_TAG_formal_parameter)
      dparam = _dbuilder->createParameterVariable(_curscope, name, ++params, file, v.original_line, ty, true);
    else
      dparam = _dbuilder->createAutoVariable(_curscope, name, file, v.original_line, ty, true);

    auto expr = _dbuilder->createExpression();
    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
    {
      _dbuilder->insertDeclare(stacklocal, dparam, expr,
                               llvm::DILocation::get(_context->context, v.original_line, v.original_column, _curscope),
                               _context->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_LOCAL)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) < _context->locals.size())
        _dbuilder->insertDbgValueIntrinsic(_context->locals[v.p_expr[2]], dparam, expr,
                                           llvm::DILocation::get(_context->context, v.original_line, v.original_column,
                                                                 _curscope),
                                           _context->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_GLOBAL)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) < _context->globals.size())
        _dbuilder->insertDbgValueIntrinsic(_context->globals[v.p_expr[2]], dparam, expr,
                                           llvm::DILocation::get(_context->context, v.original_line, v.original_column,
                                                                 _curscope),
                                           _context->builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_STACK)
    {
      expr = _dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_stack_value });

      if(static_cast<unsigned long long>(v.p_expr[2]) <
         _context->values
           .Size()) // TODO: we're going from the bottom of the limit, but it may be the bottom of the entire values stack
        _dbuilder->insertDbgValueIntrinsic(_context->values.Get()[_context->values.Limit() + v.p_expr[2]], dparam, expr,
                                           llvm::DILocation::get(_context->context, v.original_line, v.original_column,
                                                                 _curscope),
                                           _context->builder.GetInsertBlock());
    }
    else
      _dbuilder->insertDbgValueIntrinsic(
        stacklocal, dparam, expr, llvm::DILocation::get(_context->context, v.original_line, v.original_column, _curscope),
        _context->builder.GetInsertBlock());
  }

  {
    auto dparam =
      _dbuilder->createAutoVariable(_curscope, "scopeoffset", _curscope->getFile(), 0,
      StructOffsetType(diI32, dunit, dunit, "scopeoffset", 0, _context->intptrty->getBitWidth(), 1),
                                    true);

    _dbuilder->insertDeclare(stacklocal, dparam, _dbuilder->createExpression(),
                             llvm::DILocation::get(_context->context, 0, 0, _curscope), _context->builder.GetInsertBlock());
  }
}

void DebugDWARF::DebugIns(llvm::Function* fn, Instruction& i)
{
  // Pop scopes. Parent scopes should always contain child scopes, if this isn't true someone screwed up
  while(scopes.Size() > 0 && i.column > sourcemap->x_innative_ranges[scopes.Peek()].high)
    scopes.Pop();
  _curscope = scopes.Size() > 0 ? scopecache[sourcemap->x_innative_ranges[scopes.Peek()].scope] : fn->getSubprogram();
  assert(_curscope);

  // push scopes and any variables they contain
  while(curscopeindex < sourcemap->n_innative_ranges && i.column >= sourcemap->x_innative_ranges[curscopeindex].low)
  {
    auto& cur = sourcemap->x_innative_ranges[curscopeindex];
    if(i.column <= cur.high && cur.scope < scopecache.size())
    {
      auto& scope = sourcemap->x_innative_scopes[cur.scope];
      scopes.Push(curscopeindex);
      if(!scopecache[cur.scope])
      {
        llvm::DILocation* l   = _context->builder.getCurrentDebugLocation();
        auto file             = GetSourceFile(sourcemap->segments[cursegment].source_index);
        scopecache[cur.scope] = _dbuilder->createLexicalBlock(_curscope, file, l->getLine(), l->getColumn());
      }
      _curscope = scopecache[cur.scope];
      assert(cur.scope < sourcemap->n_innative_scopes);

      UpdateVariables(fn, scope);
    }

    ++curscopeindex;
  }

  UpdateLocation(i);
}

void DebugDWARF::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) {}

llvm::DIType* DebugDWARF::GetDebugType(size_t index, llvm::DIType* parent)
{
  auto& type = sourcemap->x_innative_types[index];

  if(type.tag != DW_TAG_pointer_type)
    return DebugSourceMap::GetDebugType(index, parent);

  if(index < types.size() && !types[index])
  {
    const char* name   = type.name_index < sourcemap->n_names ? sourcemap->names[type.name_index] : "";
    llvm::DIFile* file = GetSourceFile(type.source_index);

    switch(type.tag)
    {
    case DW_TAG_pointer_type:
      if(auto ty = GetDebugType(type.type_index))
      {
        auto member  = _dbuilder->createMemberType(file, name, file, 0, ty->getSizeInBits(), 0, 0x00000000000010008 << 3,
                                                  llvm::DINode::FlagZero, ty);
        auto offset  = _dbuilder->createStructType(file, "WASMOffset", file, 0, ty->getSizeInBits(), 0,
                                                  llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));
        types[index] = _dbuilder->createPointerType(offset, 32, 0, llvm::None, name);
      }
      break;
    }
  }

  return types[index];
}
void DebugDWARF::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) {}
