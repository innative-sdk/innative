// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_sourcemap.h"
#include "compile.h"
#include "util.h"
#include <algorithm>

using namespace innative;
using namespace utility;
using namespace code;
using namespace llvm::dwarf;

code::DebugSourceMap::DebugSourceMap(SourceMap* s, llvm::IntegerType* intptr, llvm::Module& m, const char* name,
                                     Environment* env, const char* filepath, llvm::LLVMContext& context) :
  sourcemap(s), Debugger(intptr, m, name, env, filepath)
{
  // If we have a sourcemap, check to see if the files exist. If they don't, see if we can reconstruct them
  path root      = GetPath(sourcemap->sourceRoot);
  path workdir   = GetWorkingDir();
  path parentdir = GetPath(filepath).parent_path();

  auto check = [](const path& file) -> bool {
    FILE* f = 0;
    FOPEN(f, file.c_str(), "rb");
    if(f)
    {
      fclose(f);
      return true;
    }
    return false;
  };

  for(size_t i = 0; i < sourcemap->n_sources; ++i)
  {
    path source = GetPath(sourcemap->sources[i]);
    if(check(root / source))
      sourcemap->sources[i] = AllocString(*env, (root / source).u8string());
    else if(check(source))
      sourcemap->sources[i] = AllocString(*env, GetAbsolutePath(source).u8string());
    else if(check(workdir / source))
      sourcemap->sources[i] = AllocString(*env, (workdir / source).u8string());
    else if(check(parentdir / source))
      sourcemap->sources[i] = AllocString(*env, (parentdir / source).u8string());
    else if(i < sourcemap->n_sourcesContent && sourcemap->sourcesContent[i] != 0 && sourcemap->sourcesContent[i][0] != 0)
    {
      source = temp_directory_path() / source.filename();
      if(DumpFile(source, sourcemap->sourcesContent[i], strlen(sourcemap->sourcesContent[i])))
        sourcemap->sources[i] = AllocString(*env, source.u8string());
    }
  }

  for(size_t i = 0; i < sourcemap->n_sources; ++i)
  {
    path p = GetPath(sourcemap->sources[i]);
    files.push_back(dbuilder->createFile(p.filename().u8string(), p.parent_path().u8string()));
  }

  types.resize(s->n_innative_types);

  llvm::Metadata** globals = tmalloc<llvm::Metadata*>(*env, sourcemap->n_innative_globals);
  for(size_t i = 0; i < sourcemap->n_innative_globals; ++i)
  {
    auto& g          = sourcemap->x_innative_variables[sourcemap->x_innative_globals[i]];
    const char* name = g.name_index < sourcemap->n_names ? sourcemap->names[g.name_index] : "";
    // auto expr        = dbuilder->createExpression(llvm::ArrayRef<int64_t>(g.p_expr, g.n_expr));
    auto expr = dbuilder->createExpression(llvm::SmallVector<uint64_t, 1>{});
    assert(expr->isValid());
    globals[i] = dbuilder->createGlobalVariableExpression(dcu, name, name, GetSourceFile(g.source_index), g.original_line,
                                                          SourceDebugType(g.type_index), false, expr);
  }
  dcu->replaceGlobalVariables(
    llvm::MDTuple::get(context, llvm::ArrayRef<llvm::Metadata*>(globals, sourcemap->n_innative_globals)));
}

llvm::DIFile* DebugSourceMap::GetSourceFile(size_t i) { return (i < files.size()) ? files[i] : dunit; }

SourceMapFunction* DebugSourceMap::GetSourceFunction(unsigned int offset)
{
  auto end             = sourcemap->x_innative_functions + sourcemap->n_innative_functions;
  SourceMapFunction* i = std::lower_bound(sourcemap->x_innative_functions, end, offset,
                                          [](const SourceMapFunction& f, unsigned int o) { return f.range.low < o; });
  return (i >= end || i->range.low != offset) ? nullptr : i;
}

void DebugSourceMap::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
{
  // Use function low_PC to find the debug entry
  SourceMapFunction* f = GetSourceFunction(offset);

  if(!f)
  {
    FunctionDebugInfo(fn, fn->getName(), optimized, true, false, dunit, line, 0);
    return;
  }

  auto name = (f->scope.name_index < sourcemap->n_names) ? fn->getName() : sourcemap->names[f->scope.name_index];

  llvm::SmallVector<llvm::Metadata*, 8> dwarfTys = { SourceDebugType(f->type_index) };
  for(unsigned int i = 0; i < f->scope.n_variables; ++i)
    if(sourcemap->x_innative_variables[f->scope.variables[i]].tag == DW_TAG_formal_parameter)
      dwarfTys.push_back(SourceDebugType(sourcemap->x_innative_variables[f->scope.variables[i]].type_index));

  auto subtype =
    dbuilder->createSubroutineType(dbuilder->getOrCreateTypeArray(dwarfTys), llvm::DINode::FlagZero,
                                   (fn->getCallingConv() == llvm::CallingConv::C) ? DW_CC_normal : DW_CC_nocall);

  FunctionDebugInfo(fn, name, optimized, true, false, GetSourceFile(f->source_index), f->original_line, 0, subtype);
}

void DebugSourceMap::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body,
                              code::Context& context)
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

  context.builder.SetCurrentDebugLocation(
    llvm::DILocation::get(context.context, fn->getSubprogram()->getLine(), 0, fn->getSubprogram()));

  // Unfortunately we can't actually gaurantee all the scopes will pop due to imprecise debug information
  while(scopes.Size() > 0)
    scopes.Pop();

  scopecache.resize(0); // Wipe the cache
  scopecache.resize(sourcemap->n_innative_scopes);
  curscope = fn->getSubprogram();
  curbody  = &body;
}

void DebugSourceMap::PostFuncBody(FunctionBody& body, code::Context& context)
{
  SourceMapFunction* f = GetSourceFunction(curbody->column);

  if(context.globals.size() > 0)
  {
    dbuilder->insertDeclare(context.memlocal,
                            dbuilder->createAutoVariable(curscope, "MEMLOCAL", GetSourceFile(f->source_index), 0,
                                                         dbuilder->createPointerType(diI32, intptrty->getBitWidth()), true),
                            dbuilder->createExpression(), context.builder.getCurrentDebugLocation(),
                            context.builder.GetInsertBlock());
  }
}

void DebugSourceMap::DebugSetGlobal(int index, code::Context& context)
{
  SourceMapFunction* f = GetSourceFunction(curbody->column);

  if(!index)
  {
    stacklocal = context.builder.CreateAlloca(context.memories[0]->getType()->getElementType()->getContainedType(0), nullptr, "IN_!stacklocal");

    dbuilder->insertDeclare(stacklocal,
                            dbuilder->createAutoVariable(curscope, "STACKLOCAL", GetSourceFile(f->source_index), 0,
                                                         CreateDebugType(stacklocal->getType()), true),
                            dbuilder->createExpression(), llvm::DILocation::get(context.context, 0, 0, curscope),
                            context.builder.GetInsertBlock());
    context.builder.CreateStore(context.builder.CreateIntToPtr(context.builder.CreateLoad(context.globals[0]),
                                                               context.builder.getInt8PtrTy()),
                                stacklocal);

    if(f)
      UpdateVariables(f->scope, context);
  }
}

void DebugSourceMap::DebugMemLocal(code::Context& context) {}

void DebugSourceMap::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc, code::Context& context) {}

void DebugSourceMap::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) {}

void DebugSourceMap::UpdateLocation(Instruction& i, code::Context& context)
{
  llvm::DILocation* loc = 0;

  while(cursegment < sourcemap->n_segments &&
        sourcemap->segments[cursegment].linecolumn < ((i.line - 1ULL) << 32 | i.column))
  {
    if(loc) // If we have a pending location, we need to create a nop instruction to hold it
      context.builder.CreateIntrinsic(llvm::Intrinsic::donothing, {}, {})->setDebugLoc(loc);

    auto& s = sourcemap->segments[cursegment++];

    if(files[s.source_index] == curscope->getFile() || s.source_index >= subprograms.size())
      loc = llvm::DILocation::get(context.context, s.original_line, s.original_column, curscope);
    else
    {
      if(!subprograms[s.source_index])
      {
        auto original = curscope->getSubprogram();
        subprograms[s.source_index] =
          dbuilder->createFunction(original->getScope(), original->getName(), original->getLinkageName(),
                                   files[s.source_index], original->getLine(), original->getType(),
                                   original->getScopeLine(), original->getFlags(), original->getSPFlags());
      }

      loc = context.builder.getCurrentDebugLocation();
      if(loc->getInlinedAt())
        loc = loc->getInlinedAt();
      loc = llvm::DILocation::get(context.context, s.original_line, s.original_column, subprograms[s.source_index], loc);
    }
  }

  if(loc)
    context.builder.SetCurrentDebugLocation(loc);
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

llvm::DIType* DebugSourceMap::StructOffsetType(llvm::DIType* ty, llvm::DIScope* scope, llvm::DIFile* file,
                                               llvm::StringRef name, uint64_t indice, unsigned int bitsize, unsigned int bytealign)
{
  auto member = dbuilder->createMemberType(scope, name, file, 0, ty->getSizeInBits(), 1, (indice + 0x00000000000010008) << 3,
                                           llvm::DINode::FlagZero, ty);
  auto offset = dbuilder->createStructType(scope, "WASM_" + name.str(), file, 0,
                                           ty->getSizeInBits(),
                                           1, llvm::DINode::FlagZero, 0, dbuilder->getOrCreateArray({ member }));
  return dbuilder->createPointerType(offset, bitsize, bytealign, llvm::None, "ptr_" + name.str());
}

void DebugSourceMap::UpdateVariables(SourceMapScope& scope, code::Context& context)
{
  int params = 0;

  for(size_t i = 0; i < scope.n_variables; ++i)
  {
    assert(scope.variables[i] < sourcemap->n_innative_variables);
    auto& v            = sourcemap->x_innative_variables[scope.variables[i]];
    const char* name   = v.name_index < sourcemap->n_names ? sourcemap->names[v.name_index] : "";
    llvm::DIFile* file = GetSourceFile(v.source_index);
    auto ty            = SourceDebugType(v.type_index);

    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
      ty = StructOffsetType(ty, file, file, name, v.p_expr[1], intptrty->getBitWidth(), 1);

    llvm::DILocalVariable* dparam;
    if(v.tag == DW_TAG_formal_parameter)
      dparam = dbuilder->createParameterVariable(curscope, name, ++params, file, v.original_line, ty, true);
    else
      dparam = dbuilder->createAutoVariable(curscope, name, file, v.original_line, ty, true);

    auto expr = dbuilder->createExpression();
    if(v.n_expr > 1 && v.p_expr[0] == DW_OP_plus_uconst)
    {
      dbuilder->insertDeclare(stacklocal, dparam, expr,
                              llvm::DILocation::get(context.context, v.original_line, v.original_column, curscope),
                              context.builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_LOCAL)
    {
      expr = dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(v.p_expr[2] < context.locals.size())
        dbuilder->insertDbgValueIntrinsic(context.locals[v.p_expr[2]], dparam, expr,
                                          llvm::DILocation::get(context.context, v.original_line, v.original_column,
                                                                curscope),
                                          context.builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_GLOBAL)
    {
      expr = dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_deref, DW_OP_stack_value });

      if(v.p_expr[2] < context.globals.size())
        dbuilder->insertDbgValueIntrinsic(context.globals[v.p_expr[2]], dparam, expr,
                                          llvm::DILocation::get(context.context, v.original_line, v.original_column,
                                                                curscope),
                                          context.builder.GetInsertBlock());
    }
    else if(v.n_expr > 2 && v.p_expr[0] == DW_OP_WASM_location && v.p_expr[1] == DW_WASM_OP_STACK)
    {
      expr = dbuilder->createExpression(llvm::SmallVector<int64_t, 3>{ DW_OP_stack_value });

      if(v.p_expr[2] <
         context.values
           .Size()) // TODO: we're going from the bottom of the limit, but it may be the bottom of the entire values stack
        dbuilder->insertDbgValueIntrinsic(context.values.Get()[context.values.Limit() + v.p_expr[2]], dparam, expr,
                                          llvm::DILocation::get(context.context, v.original_line, v.original_column,
                                                                curscope),
                                          context.builder.GetInsertBlock());
    }
    else
      dbuilder->insertDbgValueIntrinsic(
        stacklocal, dparam, expr, llvm::DILocation::get(context.context, v.original_line, v.original_column, curscope),
        context.builder.GetInsertBlock());
  }

  {
    auto dparam =
      dbuilder->createAutoVariable(curscope, "scopeoffset", curscope->getFile(), 0,
                                   StructOffsetType(diI32, dunit, dunit, "scopeoffset", 0, intptrty->getBitWidth(), 1), true);

    dbuilder->insertDeclare(stacklocal, dparam, dbuilder->createExpression(),
                            llvm::DILocation::get(context.context, 0, 0, curscope), context.builder.GetInsertBlock());
  }
}

void DebugSourceMap::DebugIns(llvm::Function* fn, Instruction& i, code::Context& context)
{
  // Pop scopes. Parent scopes should always contain child scopes, if this isn't true someone screwed up
  while(scopes.Size() > 0 && i.column > sourcemap->x_innative_ranges[scopes.Peek()].high)
    scopes.Pop();
  curscope = scopes.Size() > 0 ? scopecache[sourcemap->x_innative_ranges[scopes.Peek()].scope] : fn->getSubprogram();
  assert(curscope);

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
        llvm::DILocation* l   = context.builder.getCurrentDebugLocation();
        auto file             = GetSourceFile(sourcemap->segments[cursegment].source_index);
        scopecache[cur.scope] = dbuilder->createLexicalBlock(curscope, file, l->getLine(), l->getColumn());
      }
      curscope = scopecache[cur.scope];
      assert(cur.scope < sourcemap->n_innative_scopes);

      UpdateVariables(scope, context);
    }

    ++curscopeindex;
  }

  UpdateLocation(i, context);
}

void DebugSourceMap::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) {}

llvm::DIType* DebugSourceMap::SourceDebugType(size_t index)
{
  auto& type = sourcemap->x_innative_types[index];

  if(index < types.size() && !types[index])
  {
    const char* name   = type.name_index < sourcemap->n_names ? sourcemap->names[type.name_index] : "";
    llvm::DIFile* file = GetSourceFile(type.source_index);

    switch(type.tag)
    {
    case DW_TAG_base_type: types[index] = dbuilder->createBasicType(name, type.bit_size, type.encoding); break;
    case DW_TAG_const_type:
    case DW_TAG_volatile_type:
    case DW_TAG_restrict_type:
    case DW_TAG_atomic_type:
    case DW_TAG_interface_type:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createQualifiedType(type.tag, ty);
      break;
    case DW_TAG_pointer_type:
      if(auto ty = SourceDebugType(type.type_index))
      {
        auto member = dbuilder->createMemberType(file, name, file, 0, ty->getSizeInBits(), 0, 0x00000000000010008 << 3,
                                                 llvm::DINode::FlagZero, ty);
        auto offset = dbuilder->createStructType(file, "WASMOffset", file, 0, ty->getSizeInBits(), 0,
                                                 llvm::DINode::FlagZero, 0, dbuilder->getOrCreateArray({ member }));
        types[index] =
          dbuilder->createPointerType(offset, 32, 0, llvm::None, name);
      }
      break;
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createReferenceType(type.tag, ty, type.bit_size, type.byte_align << 3, llvm::None);
      break;
    case DW_TAG_typedef:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createTypedef(ty, name, file, type.original_line, file);
      break;
    case DW_TAG_ptr_to_member_type:
      if(type.n_types == 2)
      {
        auto base = SourceDebugType(type.types[1]);
        auto cls  = SourceDebugType(type.types[0]);
        if(base && cls)
          types[index] = dbuilder->createMemberPointerType(base, cls, type.bit_size, type.byte_align << 3);
      }
      break;
    case DW_TAG_structure_type: break;
    // case DW_TAG_inheritance:
    //  if(auto ty = SourceDebugType(type.type_index))
    //    dbuilder->createInheritance()
    case DW_TAG_enumeration_type:
      if(auto ty = SourceDebugType(type.type_index))
      {
        llvm::SmallVector<llvm::Metadata*, 8> elements;
        for(size_t i = 0; i < type.n_types; ++i)
        {
          if(type.types[i] < sourcemap->n_innative_enumerators)
          {
            auto& e = sourcemap->x_innative_enumerators[type.types[i]];
            auto id = e.name_index >= sourcemap->n_names ? "" : sourcemap->names[e.name_index];
            elements.push_back(dbuilder->createEnumerator(id, e.val, e.is_unsigned));
          }
        }
        types[index] = dbuilder->createEnumerationType(file, name, file, type.original_line, type.bit_size,
                                                       type.byte_align << 3, dbuilder->getOrCreateArray(elements), ty);
      }
      break;
    case DW_TAG_subroutine_type:
    {
      llvm::SmallVector<llvm::Metadata*, 8> params;
      for(size_t i = 0; i < type.n_types; ++i)
        params.push_back(SourceDebugType(type.types[i]));

      types[index] = dbuilder->createSubroutineType(dbuilder->getOrCreateTypeArray(params));
      break;
    }
    }
  }

  return types[index];
}
void DebugSourceMap::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) {}
