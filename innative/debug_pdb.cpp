// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_pdb.h"
#include "compile.h"
#include "utility.h"
#include <algorithm>

namespace innative {
  __KHASH_IMPL(intmap, , const char*, size_t, 1, kh_str_hash_func, kh_str_hash_equal);
}

using namespace innative;
using namespace utility;
using namespace llvm::dwarf;

#define TEXT(x) x
#define XML(x)  TEXT(#x)

DebugPDB::DebugPDB(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath) :
  DebugSourceMap(s, compiler, m, name, filepath, ENV_DEBUG_PDB), _uid(0), _deferred(kh_init_intmap())
{}
DebugPDB::~DebugPDB() { kh_destroy_intmap(_deferred); }

void DebugPDB::PostFuncBody(llvm::Function* fn, FunctionBody& body)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(f && f->range.scope < sourcemap->n_innative_scopes)
    UpdateVariables(fn, sourcemap->x_innative_scopes[f->range.scope]);
}

enum DW_WASM_OP
{
  DW_WASM_OP_LOCAL  = 0x00,
  DW_WASM_OP_GLOBAL = 0x01,
  DW_WASM_OP_STACK  = 0x02,
};

llvm::DIType* DebugPDB::StructOffsetType(size_t index, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                         uint64_t indice, llvm::Function* fn)
{
  llvm::DIType* ty      = GetDebugType(index);
  std::string type_name = GetTypeName(index, true, true);
  std::string str;
  llvm::DIType* base = nullptr;

  switch(sourcemap->x_innative_types[index].tag)
  {
  case DW_TAG_rvalue_reference_type:
  case DW_TAG_reference_type:
  case DW_TAG_pointer_type:
    str  = FormatString(false, "{0}_sp<{1},{2}>", _compiler->m.name.str(), type_name.c_str(), indice);
    base = GetBaseType(index, 0);
    break;
  case DW_TAG_array_type:
  {
    size_t base_index = index;
    while(sourcemap->x_innative_types[base_index].type_index != (size_t)~0)
      base_index = sourcemap->x_innative_types[base_index].type_index;

    str = FormatString(false, "{0}_sa<{1},{2},{3}>", _compiler->m.name.str(), GetTypeName(base_index, true, true).c_str(),
                       indice, sourcemap->x_innative_types[index].n_types);
    break;
  }
  default: str = FormatString(false, "{0}_sv<{1},{2}>", _compiler->m.name.str(), type_name.c_str(), indice); break;
  }

  auto member = _dbuilder->createMemberType(scope, "ptr", file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
  return _dbuilder->createClassType(scope, str, file, 0, 32, 1, 0, llvm::DINode::FlagZero, base,
                                    _dbuilder->getOrCreateArray({ member }));
}

void DebugPDB::UpdateVariables(llvm::Function* fn, SourceMapScope& scope)
{
  int params = 0;

  for(size_t i = 0; i < scope.n_variables; ++i)
  {
    assert(scope.variables[i] < sourcemap->n_innative_variables);
    auto& v = sourcemap->x_innative_variables[scope.variables[i]];
    llvm::DIType* ty;
    llvm::StringRef name = v.name_index < sourcemap->n_names ? sourcemap->names[v.name_index] : "";
    llvm::DIFile* file   = GetSourceFile(v.source_index);

    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
      ty = StructOffsetType(v.type_index, file, file, name, v.p_expr[1], fn);
    else
      ty = GetDebugType(v.type_index);

    llvm::DILocalVariable* dparam;
    if(v.tag == DW_TAG_formal_parameter)
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
  }
}

llvm::DIType* DebugPDB::GetBaseType(size_t index, llvm::DIType* parent)
{
  if(index == (size_t)~0)
    return 0;
  auto& type = sourcemap->x_innative_types[index];

  switch(type.tag)
  {
  case DW_TAG_rvalue_reference_type:
  case DW_TAG_reference_type:
  case DW_TAG_pointer_type: return GetDebugType(type.type_index);
  }

  return 0;
}

llvm::DIType* DebugPDB::GetDebugType(size_t index, llvm::DIType* parent)
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
    llvm::DIFile* file = GetSourceFile(type.source_index);

    switch(type.tag)
    {
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
    case DW_TAG_pointer_type:
      if(auto ty = GetDebugType(type.type_index))
      {
        if(ty == diVoid)
          ty = diI32;
        auto member =
          _dbuilder->createMemberType(dcu, "ptr", file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
        types[index] = _dbuilder->createStructType(dcu, GetTypeName(index, true, true), file, 0, 32, 1,
                                                   llvm::DINode::FlagZero, nullptr,
                                                   _dbuilder->getOrCreateArray({ member }));

        // Only defer single pointers. Multiple indirections shouldn't be deferred.
        size_t cur = type.type_index;
        while(cur != (size_t)~0)
        {
          switch(sourcemap->x_innative_types[cur].tag)
          {
          case DW_TAG_rvalue_reference_type:
          case DW_TAG_reference_type:
          case DW_TAG_pointer_type: return llvm::dyn_cast<llvm::DIType>(types[index]);
          }
          cur = sourcemap->x_innative_types[cur].type_index;
        }

        int r;
        auto pname = types[index]->getName().str();
        auto iter  = kh_put_intmap(_deferred, pname.c_str(), &r);
        if(r > 0)
        {
          kh_key(_deferred, iter) = AllocString(_compiler->env, kh_key(_deferred, iter));
          kh_val(_deferred, iter) = index;
        }
      }
      break;
    }
  }

  assert(types[index] != 0);
  return llvm::dyn_cast<llvm::DIType>(types[index]);
}

void DebugPDB::Finalize()
{
  _finalizecomposite();
  llvm::SmallVector<llvm::DIGlobalVariableExpression*, 1> expr;
  _compiler->globals[0]->getDebugInfo(expr);
  auto global = expr[0]->getVariable()->getName();
  expr.clear();
  _compiler->memories[0]->getDebugInfo(expr);
  auto mem = expr[0]->getVariable()->getName();

  _compiler->natvis += FormatString(
    true,
    "<Type Name=\"{2}_p&lt;*&gt;\"><DisplayString>{(({2}_p&lt;$T1&gt;*)({0}.m0+(unsigned int)this))->ptr}</DisplayString><Expand><ExpandedItem>(({2}_p&lt;$T1&gt;*)({0}.m0+(unsigned int)this))->ptr</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_pp&lt;*&gt;\"><DisplayString>{(({2}_pp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this)))->ptr.ptr}</DisplayString><Expand><ExpandedItem>(({2}_pp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this)))->ptr.ptr</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_ppp&lt;*&gt;\"><DisplayString>{(({2}_ppp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this))))->ptr.ptr.ptr}</DisplayString><Expand><ExpandedItem>(({2}_ppp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this))))->ptr.ptr.ptr</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_pppp&lt;*&gt;\"><DisplayString>{(({2}_pppp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this)))))->ptr.ptr.ptr.ptr}</DisplayString><Expand><ExpandedItem>(({2}_pppp&lt;$T1&gt;*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+*(unsigned int*)({0}.m0+(unsigned int)this)))))->ptr.ptr.ptr.ptr</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_sp&lt;*,*&gt;\"><DisplayString>{($T1*)(*(unsigned int*)({0}.m0 + {1} + $T2))}</DisplayString><Expand><ExpandedItem>($T1*)(*(unsigned int*)({0}.m0 + {1} + $T2))</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_sv&lt;*,*&gt;\"><DisplayString>{*($T1*)({0}.m0 + {1} + $T2)}</DisplayString><Expand><ExpandedItem>*($T1*)({0}.m0 + {1} + $T2)</ExpandedItem></Expand></Type>\n"
    "<Type Name=\"{2}_sa&lt;*,*,*&gt;\"><DisplayString>{{{*($T1*)({0}.m0 + {1} + $T2)}}}</DisplayString><Expand><Item Name=\"[size]\">$T3</Item><ArrayItems><Size>$T3</Size><ValuePointer>($T1*)({0}.m0 + {1} + $T2)</ValuePointer></ArrayItems></Expand></Type>\n",
    mem, global, _compiler->m.name.str());

  for(auto iter = kh_begin(_deferred); iter < kh_end(_deferred); ++iter)
  {
    if(!kh_exist(_deferred, iter))
      continue;
    // printf("%s\n", kh_key(_deferred, iter));
    _finalize(kh_key(_deferred, iter));
  }

  kh_clear_intmap(_deferred);
  Debugger::Finalize();
}

void DebugPDB::_finalize(const char* str)
{
  auto iter = kh_get_intmap(_deferred, str);
  if(!kh_exist2(_deferred, iter))
    return;
  auto index = kh_value(_deferred, iter);
  if(index == (size_t)~0)
    return;

  auto& type = sourcemap->x_innative_types[index];
  if(auto ty = GetDebugType(type.type_index))
  {
    // Mark this type as being processed
    kh_value(_deferred, iter) = (size_t)~0;
    std::string main;
    std::string aux;

    if(auto composite = llvm::dyn_cast<llvm::DICompositeType>(ty))
    {
      aux += FormatString(true, "<Type Name=\"{1}\"><DisplayString>{{", types[index]->getName(),
                          GetTypeName(type.type_index, true, true));

      for(const auto& e : composite->getElements())
      {
        if(auto element = llvm::dyn_cast<llvm::DIDerivedType>(e))
        {
          auto sname = element->getBaseType()->getName().str();
          auto iter  = kh_get_intmap(_deferred, sname.c_str());
          if(kh_exist2(_deferred, iter))
          {
            /*auto basetype = llvm::dyn_cast<llvm::DICompositeType>(element->getBaseType());
            if(basetype->getElements().size() > 0)
            {
              auto baseelement = llvm::dyn_cast<llvm::DIType>(basetype->getElements()[0]);
              if(!baseelement->getName().empty())
                aux += FormatString(true, "{0}:{({1}*)(*(unsigned int*)(&amp;this->{0}))}}", element->getName(),
                                    baseelement->getName(), expr[0]->getVariable()->getName(),
                                    element->getOffsetInBits() / 8);
            }*/

            aux += FormatString(true, "{0}:{(void*)(*(unsigned int*)(&amp;this->{0}))} ", element->getName(),
                                element->getBaseType()->getName());
            main += FormatString(true, "  <Item Name=\"{0}\">({1}*)(*(unsigned int*)(&amp;this->{0}))</Item>\n",
                                 element->getName(), element->getBaseType()->getName());
          }
          else if(element->getTag() == DW_TAG_inheritance)
          {
            main += FormatString(true, "  <Item Name=\"[{0}]\">*({0}*)((char*)this+{1})</Item>\n",
                                 element->getBaseType()->getName(), element->getOffsetInBits() / 8);
          }
          else if(element->getBaseType()->getTag() == DW_TAG_array_type)
          {
            auto composite = llvm::cast<llvm::DICompositeType>(element->getBaseType());
            auto base      = composite->getBaseType();
            while(base->getName().empty())
            {
              if(auto derived = llvm::dyn_cast<llvm::DIDerivedType>(base))
                base = derived->getBaseType();
              else if(auto comp = llvm::dyn_cast<llvm::DICompositeType>(base))
                base = comp->getBaseType();
            }
            auto count = !base->getSizeInBits() ? 0 : (composite->getSizeInBits() / base->getSizeInBits());
            for(auto* e : composite->getElements())
            {
              if(auto* subrange = llvm::dyn_cast<llvm::DISubrange>(e))
              {
                auto* data = subrange->getCount().dyn_cast<llvm::ConstantInt*>();
                if(data != nullptr)
                  count = data->getSExtValue();
              }
            }

            aux += FormatString(true, "{0}:{this->{0}} ", element->getName());
            main += FormatString(true, "  <Item Name=\"{0}\">this->{0}</Item>\n", element->getName());
          }
          else
          {
            aux += FormatString(true, "{0}:{this->{0}} ", element->getName());
            main += FormatString(true, "  <Item Name=\"{0}\">this->{0}</Item>\n", element->getName());
          }
        }
      }

      if(aux.size() && aux.back() == ' ') // strip last space
        aux.pop_back();

      _compiler->natvis += aux;
      _compiler->natvis += "}}</DisplayString><Expand>\n";
      _compiler->natvis += main;
      _compiler->natvis += "</Expand></Type>\n";
    }
  }
}
