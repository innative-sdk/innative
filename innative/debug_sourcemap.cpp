// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_sourcemap.h"
#include "compile.h"
#include "util.h"
#include <algorithm>

using namespace innative;
using namespace code;
using namespace utility;

DebugSourceMap::DebugSourceMap(SourceMap* s, llvm::IntegerType* intptr, llvm::Module& m, const char* name, Environment* env,
                               const char* filepath) :
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
}

const SourceMapSegment* DebugSourceMap::GetMappedSegment(unsigned int line, unsigned int column) { return nullptr; }

llvm::DIFile* DebugSourceMap::ResolveMappedSegment(unsigned int& line, unsigned int& column)
{
  auto file = dunit;

  if(auto segment = GetMappedSegment(line, column))
  {
    line   = (unsigned int)segment->original_line;
    column = (unsigned int)segment->original_column;
    file   = files[segment->source_index];
  }

  return file;
}

llvm::DILocation* DebugSourceMap::GetMappedLocation(unsigned int line, unsigned int column, llvm::DILocalScope* scope,
                                                    llvm::LLVMContext& context)
{
  auto segment = GetMappedSegment(line, column);
  if(segment && segment->source_index < files.size())
  {
    if(files[segment->source_index] == scope->getFile())
      return llvm::DILocation::get(context, segment->original_line, segment->original_column, scope);
    if(segment->source_index < subprograms.size())
    {
      if(!subprograms[segment->source_index])
      {
        auto original = scope->getSubprogram();
        subprograms[segment->source_index] =
          dbuilder->createFunction(original->getScope(), scope->getFile()->getName(), original->getName(),
                                   files[segment->source_index], segment->original_line, original->getType(),
                                   segment->original_line, original->getFlags(), original->getSPFlags());
      }

      return llvm::DILocation::get(context, segment->original_line, segment->original_column,
                                   subprograms[segment->source_index], llvm::DILocation::get(context, line, column, scope));
    }
  }
  return llvm::DILocation::get(context, line, column, scope);
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
    if(sourcemap->x_innative_variables[f->scope.variables[i]].tag == llvm::dwarf::DW_TAG_formal_parameter)
      dwarfTys.push_back(SourceDebugType(sourcemap->x_innative_variables[f->scope.variables[i]].type_index));

  auto subtype = dbuilder->createSubroutineType(dbuilder->getOrCreateTypeArray(dwarfTys), llvm::DINode::FlagZero,
                                                (fn->getCallingConv() == llvm::CallingConv::C) ? llvm::dwarf::DW_CC_normal :
                                                                                                 llvm::dwarf::DW_CC_nocall);

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

  while(scopes.Size() > 0) // Unfortunately we can't actually gaurantee all the scopes will pop due to imprecise debug information
    scopes.Pop();

  scopecache.resize(0); // Wipe the cache
  scopecache.resize(sourcemap->n_innative_scopes);
  curscope = fn->getSubprogram();
}

void DebugSourceMap::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc, code::Context& context) {}

void DebugSourceMap::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc, code::Context& context) {}

llvm::DILocation* DebugSourceMap::UpdateLocation(llvm::DILocation* loc, Instruction& i, code::Context& context)
{
  while(cursegment < sourcemap->n_segments &&
        sourcemap->segments[cursegment].linecolumn < ((i.line - 1ULL) << 32 | i.column))
  {
    if(loc) // If we have a pending location, we need to create a nop instruction to hold it
      context.builder.CreateIntrinsic(llvm::Intrinsic::donothing, {}, {})->setDebugLoc(loc);

    auto& s = sourcemap->segments[cursegment++];
    loc     = llvm::DILocation::get(context.context, s.original_line, s.original_column, curscope);
  }

  if(loc)
    context.builder.SetCurrentDebugLocation(loc);
  return context.builder.getCurrentDebugLocation();
}

void DebugSourceMap::DebugIns(llvm::Function* fn, Instruction& i, code::Context& context)
{
  // Pop scopes. Parent scopes should always contain child scopes, if this isn't true someone screwed up
  while(scopes.Size() > 0 && i.column > sourcemap->x_innative_ranges[scopes.Peek()].high)
    scopes.Pop();
  curscope = scopes.Size() > 0 ? scopecache[sourcemap->x_innative_ranges[scopes.Peek()].scope] : fn->getSubprogram();
  assert(curscope);
  assert(context.builder.getCurrentDebugLocation()->getScope()->getSubprogram() == fn->getSubprogram());

  llvm::DILocation* loc = 0;

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
      int params = 0;

      loc = UpdateLocation(loc, i, context);
      assert(loc->getScope()->getSubprogram() == fn->getSubprogram());

      for(size_t i = 0; i < scope.n_variables; ++i)
      {
        assert(scope.variables[i] < sourcemap->n_innative_variables);
        auto& v            = sourcemap->x_innative_variables[scope.variables[i]];
        const char* name   = v.name_index < sourcemap->n_names ? sourcemap->names[v.name_index] : "";
        llvm::DIFile* file = GetSourceFile(v.source_index);

        llvm::DILocalVariable* dparam;
        if(v.tag == llvm::dwarf::DW_TAG_formal_parameter)
          dparam = dbuilder->createParameterVariable(curscope, name, ++params, file, v.original_line,
                                                     SourceDebugType(v.type_index), true);
        else
          dparam = dbuilder->createAutoVariable(curscope, name, file, v.original_line, SourceDebugType(v.type_index), true);

        dbuilder->insertDeclare(context.memlocal, dparam,
                                dbuilder->createExpression(llvm::ArrayRef<int64_t>(v.p_expr, v.n_expr)), loc,
                                context.builder.GetInsertBlock());
      }
    }

    ++curscopeindex;
  }

  loc = UpdateLocation(loc, i, context);
  assert(loc->getScope()->getSubprogram() == fn->getSubprogram());
}

void DebugSourceMap::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line)
{
  auto expr = dbuilder->createExpression(llvm::SmallVector<uint64_t, 1>{ llvm::dwarf::DW_OP_deref });

  v->addDebugInfo(dbuilder->createGlobalVariableExpression(dcu, name, v->getName(), dunit, (unsigned int)line,
                                                           CreateDebugType(v->getType()), !v->hasValidDeclarationLinkage(),
                                                           expr));
}

llvm::DIType* DebugSourceMap::SourceDebugType(size_t index)
{
  auto& type = sourcemap->x_innative_types[index];

  if(index < types.size() && !types[index])
  {
    const char* name   = type.name_index < sourcemap->n_names ? sourcemap->names[type.name_index] : "";
    llvm::DIFile* file = GetSourceFile(type.source_index);

    switch(type.tag)
    {
    case llvm::dwarf::DW_TAG_base_type: types[index] = dbuilder->createBasicType(name, type.bit_size, type.encoding); break;
    case llvm::dwarf::DW_TAG_const_type:
    case llvm::dwarf::DW_TAG_volatile_type:
    case llvm::dwarf::DW_TAG_restrict_type:
    case llvm::dwarf::DW_TAG_atomic_type:
    case llvm::dwarf::DW_TAG_interface_type:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createQualifiedType(type.tag, ty);
      break;
    case llvm::dwarf::DW_TAG_pointer_type:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createPointerType(ty, type.bit_size, type.byte_align << 3, llvm::None, name);
      break;
    case llvm::dwarf::DW_TAG_rvalue_reference_type:
    case llvm::dwarf::DW_TAG_reference_type:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createReferenceType(type.tag, ty, type.bit_size, type.byte_align << 3, llvm::None);
      break;
    case llvm::dwarf::DW_TAG_typedef:
      if(auto ty = SourceDebugType(type.type_index))
        types[index] = dbuilder->createTypedef(ty, name, file, type.original_line, file);
      break;
    case llvm::dwarf::DW_TAG_ptr_to_member_type:
      if(type.n_types == 2)
      {
        auto base = SourceDebugType(type.types[1]);
        auto cls  = SourceDebugType(type.types[0]);
        if(base && cls)
          types[index] = dbuilder->createMemberPointerType(base, cls, type.bit_size, type.byte_align << 3);
      }
      break;
    // case llvm::dwarf::DW_TAG_inheritance:
    //  if(auto ty = SourceDebugType(type.type_index))
    //    dbuilder->createInheritance()
    case llvm::dwarf::DW_TAG_enumeration_type:
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
    case llvm::dwarf::DW_TAG_subroutine_type:
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
