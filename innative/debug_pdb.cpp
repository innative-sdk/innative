// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_pdb.h"
#include "compile.h"
#include "utility.h"
#include <algorithm>

namespace innative {
#ifdef IN_64BIT
  __KHASH_IMPL(intset, , size_t, char, 0, kh_int64_hash_func, kh_int64_hash_equal);
#else
  __KHASH_IMPL(intset, , size_t, char, 0, kh_int_hash_func, kh_int_hash_equal);
#endif
}

using namespace innative;
using namespace utility;
using namespace llvm::dwarf;

#define TEXT(x) x
#define XML(x)  TEXT(#x)

DebugPDB::DebugPDB(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath) :
  DebugSourceMap(s, compiler, m, name, filepath, ENV_DEBUG_PDB), _uid(0), _deferred(kh_init_intset())
{}
DebugPDB::~DebugPDB() { kh_destroy_intset(_deferred); }

void DebugPDB::PostFuncBody(llvm::Function* fn, FunctionBody& body)
{
  SourceMapFunction* f = GetSourceFunction(_curbody->column);

  if(f && f->range.scope < sourcemap->n_innative_scopes)
    UpdateVariables(fn, sourcemap->x_innative_scopes[f->range.scope]);
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

llvm::DIType* DebugPDB::StructOffsetType(size_t index, llvm::DIScope* scope, llvm::DIFile* file, llvm::StringRef name,
                                         uint64_t indice, llvm::Function* fn)
{
  llvm::DIType* ty          = GetDebugType(index);
  llvm::StringRef type_name = sourcemap->x_innative_types[index].name_index < sourcemap->n_names ?
                                sourcemap->names[sourcemap->x_innative_types[index].name_index] :
                                ty->getName();

  auto member = _dbuilder->createMemberType(scope, name, file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
  auto offset = _dbuilder->createStructType(
    scope, FormatString(false, "{0}::${1}{2}", Compiler::CppString(_compiler->m.name.str()), name, _uid++, type_name), file,
    0, ty->getSizeInBits(), 1, llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));

  if(_compiler->globals.size() > 0 && _compiler->memories.size() > 0)
  {
    llvm::SmallVector<llvm::DIGlobalVariableExpression*, 1> expr;
    _compiler->globals[0]->getDebugInfo(expr);
    auto global = expr[0]->getVariable()->getName();
    expr.clear();
    _compiler->memories[0]->getDebugInfo(expr);
    auto mem = expr[0]->getVariable()->getName();

    switch(sourcemap->x_innative_types[index].tag)
    {
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
    case DW_TAG_pointer_type:
      _compiler->natvis +=
        FormatString(true,
                     "<Type Name=\"{0}\">"
                     "<DisplayString>{({1}*)(*(unsigned int*)({2}.m0 + {3} + {4}))}</DisplayString>"
                     "<Expand><ExpandedItem>({1}*)(*(unsigned int*)({2}.m0 + {3} + {4}))</ExpandedItem></Expand>"
                     "</Type>",
                     offset->getName(), ty->getName(), mem, global, indice);
      break;
    case DW_TAG_array_type:
    {
      size_t base_index = index;
      while(sourcemap->x_innative_types[base_index].type_index != (size_t)~0)
        base_index = sourcemap->x_innative_types[base_index].type_index;

      _compiler->natvis +=
        FormatString(true,
                     "<Type Name=\"{0}\"><DisplayString>{{{*({1}*)({2}.m0 + {3} + {4})}}}</DisplayString>"
                     "<Expand><Item Name=\"[size]\">{5}</Item>"
                     "<ArrayItems><Size>{5}</Size><ValuePointer>({1}*)({2}.m0 + {3} + {4})</ValuePointer></ArrayItems>"
                     "</Expand></Type>",
                     offset->getName(), GetDebugType(base_index)->getName(), mem, global, indice,
                     sourcemap->x_innative_types[index].n_types);
    }
    break;
    default:
      _compiler->natvis +=
        FormatString(true, "<Type Name=\"{0}\"><DisplayString>{*({1}*)({2}.m0 + {3} + {4})}</DisplayString></Type>",
                     offset->getName(), type_name, mem, global, indice);
      break;
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

        auto member =
          _dbuilder->createMemberType(dcu, name, file, 0, ty->getSizeInBits(), 1, 0, llvm::DINode::FlagZero, ty);
        types[index] = _dbuilder->createStructType(dcu, pname + ">", file, 0, ty->getSizeInBits(), 1,
                                                   llvm::DINode::FlagZero, 0, _dbuilder->getOrCreateArray({ member }));
        int r;
        kh_put_intset(_deferred, index, &r);
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
  _compiler->memories[0]->getDebugInfo(expr);

  for(auto iter = kh_begin(_deferred); iter < kh_end(_deferred); ++iter)
  {
    if(!kh_exist(_deferred, iter))
      continue;
    auto index = kh_key(_deferred, iter);
    auto& type = sourcemap->x_innative_types[index];
    if(auto ty = GetDebugType(type.type_index))
    {
      // We cannot rely on the base type actually having a name, so we strip p<> from the name we know we have.
      auto name = types[index]->getName().str();
      name      = name.substr(2, name.size() - 3);
      std::string aux;
      _compiler->natvis +=
        FormatString(true, "<Type Name=\"{0}\"><DisplayString>{*({2}*)({1}.m0+(unsigned int)this)}</DisplayString><Expand>",
                     types[index]->getName(), expr[0]->getVariable()->getName(), name);

      if(auto composite = llvm::dyn_cast<llvm::DICompositeType>(ty))
      {
        std::string expansion = "<Expand>";
        aux += FormatString(true, "<Type Name=\"{2}\"><DisplayString>{{", types[index]->getName(),
                            expr[0]->getVariable()->getName(), name);
        for(const auto& e : composite->getElements())
          if(auto element = llvm::dyn_cast<llvm::DIDerivedType>(e))
          {
            if(element->getBaseType()->getName().startswith("p<"))
            {
              if(auto basetype = llvm::dyn_cast<llvm::DICompositeType>(element->getBaseType());
                 basetype->getElements().size() > 0)
                if(auto baseelement = llvm::dyn_cast<llvm::DIType>(basetype->getElements()[0]);
                   !baseelement->getName().empty())
                  aux += FormatString(true, "{0}:{({1})({2}.m0 + *(unsigned int*)(&amp;this->{0}))}}", element->getName(),
                                      baseelement->getName(), expr[0]->getVariable()->getName(),
                                      element->getOffsetInBits() / 8);

              _compiler->natvis +=
                FormatString(true, "<Item Name=\"{0}\">({1}*)(*(unsigned int*)({2}.m0+(unsigned int)this+{3}))</Item>",
                             element->getName(), element->getBaseType()->getName(), expr[0]->getVariable()->getName(),
                             element->getOffsetInBits() / 8);
            }
            else if(element->getTag() == DW_TAG_inheritance)
            {
              _compiler->natvis += FormatString(true, "<Item Name=\"[{0}]\">*({0}*)({1}.m0+(unsigned int)this+{2})</Item>",
                                                element->getBaseType()->getName(), expr[0]->getVariable()->getName(),
                                                element->getOffsetInBits() / 8);
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
                  if(auto* data = subrange->getCount().dyn_cast<llvm::ConstantInt*>(); true)
                    count = data->getSExtValue();
              }

              aux += FormatString(true, "{0}:{this->{0}} ", element->getName());
              _compiler->natvis += FormatString(true,
                                                "<Item Name=\"{0}\">*({1}*)({2}.m0+(unsigned int)this+{3})</Item>",
                                                element->getName(), base->getName(), expr[0]->getVariable()->getName(),
                                                element->getOffsetInBits() / 8, count);
            }
            else
            {
              aux += FormatString(true, "{0}:{this->{0}} ", element->getName());
              _compiler->natvis += FormatString(true, "<Item Name=\"{0}\">*({1}*)({2}.m0+(unsigned int)this+{3})</Item>",
                                                element->getName(), element->getBaseType()->getName(),
                                                expr[0]->getVariable()->getName(), element->getOffsetInBits() / 8);
            }
          }

        if(aux.size()) // strip last space
          aux.pop_back();

        aux += "}}</DisplayString>";
        aux += expansion;
        aux += "</Expand></Type>";
      }
      else
        _compiler->natvis += FormatString(true, "<ExpandedItem>*({0}*)({1}.m0+(unsigned int)this)</ExpandedItem>", name,
                                          expr[0]->getVariable()->getName());
      _compiler->natvis += "</Expand></Type>";
      _compiler->natvis += aux;
    }
  }
  DebugSourceMap::Finalize();
}
