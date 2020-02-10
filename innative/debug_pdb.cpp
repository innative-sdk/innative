// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_pdb.h"
#include "compile.h"
#include "util.h"
#include <algorithm>

using namespace innative;
using namespace utility;
using namespace code;
using namespace llvm::dwarf;

#define TEXT(x) x
#define XML(x)  TEXT(#x)

code::DebugPDB::DebugPDB(SourceMap* s, Context* context, llvm::Module& m, const char* name, const char* filepath) :
  DebugSourceMap(s, context, m, name, filepath), _uid(0)
{}

void DebugPDB::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
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

void DebugPDB::PostFuncBody(llvm::Function* fn, FunctionBody& body)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(f)
    UpdateVariables(fn, f->scope);

  // clang-format off
    /*_context->natvis +=  XML(
          <Type Name="$_n">
            <DisplayString>{(int*)(benchmark_debug::linearmemory_0.m0 + benchmark_debug::globalvariable_0 + 0xc)}</DisplayString>
          </Type>
          <Type Name="$_f">
            <DisplayString>{(p&lt;Complex&gt;*)(*(unsigned int*)(benchmark_debug::linearmemory_0.m0+benchmark_debug::globalvariable_0+0x8))}</DisplayString>
            <Expand>
              <ExpandedItem>(p&lt;Complex&gt;*)(*(unsigned int*)(benchmark_debug::linearmemory_0.m0+benchmark_debug::globalvariable_0+0x8))</ExpandedItem>
            </Expand>
          </Type>
          <Type Name="p&lt;Complex&gt;">
            <DisplayString>p&lt;Complex&gt;: {(unsigned int)this} | {(benchmark_debug::linearmemory_0.m0+(unsigned int)this)}</DisplayString>
            <Expand>
              <Item Name="a">*(int*)(benchmark_debug::linearmemory_0.m0+(unsigned int)this)</Item>
              <Item Name="b">*(float*)(benchmark_debug::linearmemory_0.m0+(unsigned int)this+4)</Item>
              <Item Name="c">(p&lt;Complex&gt;*)(*(unsigned int*)(benchmark_debug::linearmemory_0.m0+(unsigned int)this+8))</Item>
            </Expand>
          </Type>
      );*/
  // clang-format on
}

void DebugPDB::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc) {}

void DebugPDB::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) {}

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

llvm::DIType* DebugPDB::StructOffsetType(llvm::DIType* ty, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                         uint64_t indice, llvm::Function* fn)
{
  auto member = _dbuilder->createMemberType(scope, name, file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
  auto offset = _dbuilder->createStructType(
    scope,
    FormatString(false, "{0}::${1}{2}", code::Context::CppString(_context->m.name.str()), name, _uid++, ty->getName()),
    file, 0, ty->getSizeInBits(), 1, llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));

  if(_context->globals.size() > 0 && _context->memories.size() > 0)
  {
    llvm::SmallVector<llvm::DIGlobalVariableExpression*, 1> expr;
    _context->globals[0]->getDebugInfo(expr);
    auto global = expr[0]->getVariable()->getName();
    expr.clear();
    _context->memories[0]->getDebugInfo(expr);
    auto mem = expr[0]->getVariable()->getName();

    if(ty->getName().startswith("p<"))
    {
      _context->natvis +=
        FormatString(true,
                     "<Type Name=\"{0}\">"
                     "<DisplayString>{({1}*)(*(unsigned int*)({2}.m0 + {3} + {4}))}</DisplayString>"
                     "<Expand><ExpandedItem>({1}*)(*(unsigned int*)({2}.m0 + {3} + {4}))</ExpandedItem></Expand>"
                     "</Type>",
                     offset->getName(), ty->getName(), mem, global, indice);
    }
    else
    {
      _context->natvis +=
        FormatString(true, "<Type Name = \"{0}\"><DisplayString>{*({1}*)({2}.m0 + {3} + {4})}</DisplayString></Type>",
                     offset->getName(), ty->getName(), mem, global, indice);
    }
  }

  return offset;
}

void DebugPDB::UpdateVariables(llvm::Function* fn, SourceMapScope& scope)
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
      ty = StructOffsetType(ty, file, file, name, v.p_expr[1], fn);

    llvm::DILocalVariable* dparam;
    if(v.tag == DW_TAG_formal_parameter)
      dparam = _dbuilder->createParameterVariable(_curscope, name, ++params, file, v.original_line, ty, true);
    else
      dparam = _dbuilder->createAutoVariable(_curscope, name, file, v.original_line, ty, true);

    auto expr = _dbuilder->createExpression();
    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
    {
      _dbuilder->insertDeclare(_context->memlocal, dparam, expr,
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
  }
}

llvm::DIType* DebugPDB::GetDebugType(size_t index, llvm::DIType* parent)
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
        auto member =
          _dbuilder->createMemberType(dcu, name, file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
        types[index] = _dbuilder->createStructType(dcu, std::string("p<") + name + ">", file, 0, ty->getSizeInBits(), 1,
                                                   llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));
        _deferred.push_back(index);
      }
      break;
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createReferenceType(type.tag, ty, type.bit_size, type.byte_align << 3, llvm::None);
      break;
    case DW_TAG_ptr_to_member_type:
      if(type.n_types == 2)
      {
        auto base = GetDebugType(type.types[1]);
        auto cls  = GetDebugType(type.types[0]);
        if(base && cls)
          types[index] = _dbuilder->createMemberPointerType(base, cls, type.bit_size, type.byte_align << 3);
      }
      break;
    }
  }

  assert(types[index] != 0);
  return types[index];
}

void DebugPDB::Finalize()
{
  llvm::SmallVector<llvm::DIGlobalVariableExpression*, 1> expr;
  _context->memories[0]->getDebugInfo(expr);

  for(auto index : _deferred)
  {
    auto& type = sourcemap->x_innative_types[index];
    if(auto ty = GetDebugType(type.type_index))
    {
      auto test = ty->getName();
      _context->natvis += FormatString(
        true,
        "<Type Name = \"{0}\"><DisplayString>{0}:{(unsigned int)this} | {({1}.m0+(unsigned int)this)}</DisplayString><Expand>",
        types[index]->getName(), expr[0]->getVariable()->getName());

      if(auto composite = llvm::dyn_cast<llvm::DICompositeType>(ty))
      {
        for(const auto& e : composite->getElements())
          if(auto element = llvm::dyn_cast<llvm::DIDerivedType>(e))
          {
            _context->natvis += FormatString(true, "<Item Name=\"{0}\">*({1}*)({2}.m0+(unsigned int)this+{3})</Item>",
                                             element->getName(), element->getBaseType()->getName(),
                                             expr[0]->getVariable()->getName(), element->getOffsetInBits() / 8);
            auto test = FormatString(true, "<Item Name=\"{0}\">*({1}*)({2}.m0+(unsigned int)this+{3})</Item>",
                                     element->getName(), element->getBaseType()->getName(),
                                     expr[0]->getVariable()->getName(), element->getOffsetInBits() / 8);
          }

        //  <Item Name="c">(p&lt;Complex&gt;*)(*(unsigned int*)(benchmark_debug::linearmemory_0.m0+(unsigned
        //  int)this+8))</Item>
      }
      else
        _context->natvis += FormatString(true, "<ExpandedItem>*({0}*)({1}.m0+(unsigned int)this)</Item>", ty->getName(),
                                         expr[0]->getVariable()->getName());
      _context->natvis += "</Expand></Type>";
    }
  }
  DebugSourceMap::Finalize();
}
